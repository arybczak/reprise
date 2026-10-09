-- | The format language, which describes how a song is shown.
--
-- A format renders to styled spans. A tag that the song lacks makes its
-- sequence missing, up to the nearest @[...]@, which then tries its next
-- alternative. A missing tag outside any @[...]@ renders as a marker.
module Reprise.Format
  ( -- * Formats
    Format (..)
  , Item (..)
  , Field (..)
  , fields

    -- * Parsing
  , FormatError (..)
  , parseStyledFormat
  , parsePlainFormat
  , printFormat

    -- * Rendering
  , Span (..)
  , RenderContext (..)
  , renderFormat
  , renderPlain
  , fieldValue
  , spansText
  , highlightSpans
  , spansWidth
  , fitSpans
  , songName
  , formatDuration
  ) where

import Data.Char hiding (Format)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Void
import System.FilePath
import Yamlet hiding (Comment)

import Reprise.Mpd.Protocol.Types
import Reprise.Number
import Reprise.Style
import Reprise.Width

----------------------------------------
-- Formats

-- | A format, with styles of type @s@. A format of the type @Format Void@
-- can't contain a style.
newtype Format s = Format [Item s]
  deriving stock (Eq, Show)

data Item s
  = Literal T.Text
  | -- | A field with an optional maximum width in terminal columns.
    FieldItem Field (Maybe Int)
  | -- | @[A | B]@: the first alternative without a missing field, or nothing.
    Alternatives [[Item s]]
  | Styled s [Item s]
  deriving stock (Eq, Show)

-- | The fields of a song that a format can show.
data Field
  = ArtistField
  | AlbumArtistField
  | AlbumField
  | TitleField
  | -- | The track number without the total and with two digits, e.g. @01@
    -- for @1/12@.
    TrackField
  | TrackRawField
  | DiscField
  | DateField
  | -- | The year part of the date.
    YearField
  | GenreField
  | ComposerField
  | PerformerField
  | CommentField
  | PriorityField
  | LengthField
  | FileField
  | FilenameField
  | DirectoryField
  deriving stock (Eq, Ord, Show, Enum, Bounded)

fieldName :: Field -> T.Text
fieldName = \case
  ArtistField -> "artist"
  AlbumArtistField -> "albumartist"
  AlbumField -> "album"
  TitleField -> "title"
  TrackField -> "track"
  TrackRawField -> "track_raw"
  DiscField -> "disc"
  DateField -> "date"
  YearField -> "year"
  GenreField -> "genre"
  ComposerField -> "composer"
  PerformerField -> "performer"
  CommentField -> "comment"
  PriorityField -> "priority"
  LengthField -> "length"
  FileField -> "file"
  FilenameField -> "filename"
  DirectoryField -> "directory"

fields :: [Field]
fields = [minBound .. maxBound]

----------------------------------------
-- Parsing

-- | An error with the position of the character, from 0, where it was found.
data FormatError = FormatError
  { position :: Int
  , message :: T.Text
  }
  deriving stock (Eq, Show)

-- | Parse a format with styles, using a parser for the text between @<@ and
-- @>@.
parseFormat :: (T.Text -> Either T.Text s) -> T.Text -> Either FormatError (Format s)
parseFormat parseSpanStyle input = do
  (items, rest) <- parseSequence parseSpanStyle TopLevel (zip [0 ..] (T.unpack input))
  case rest of
    [] -> Right $ Format items
    (i, c) : _ -> Left . FormatError i $ "unexpected " <> T.singleton c

parseStyledFormat :: T.Text -> Either FormatError (Format Style)
parseStyledFormat = parseFormat parseStyle

-- | Parse a format for a context that shows plain text, e.g. the window
-- title.
parsePlainFormat :: T.Text -> Either FormatError (Format Void)
parsePlainFormat = parseFormat $ \_ -> Left "this format can't contain styles"

-- | Where a sequence is, which decides the characters that end it.
data Context = TopLevel | InAlternatives | InSpan
  deriving stock (Eq)

type Input = [(Int, Char)]

parseSequence
  :: forall s
   . (T.Text -> Either T.Text s)
  -> Context
  -> Input
  -> Either FormatError ([Item s], Input)
parseSequence parseSpanStyle context = go []
  where
    go :: [Item s] -> Input -> Either FormatError ([Item s], Input)
    go acc input = case input of
      -- The caller of a nested sequence reports a missing ] or </>, with the
      -- position where the sequence started.
      [] -> done acc input
      (i, c) : rest -> case c of
        '%' -> case rest of
          (_, '{') : rest' -> do
            (item, rest'') <- parseFieldItem i rest'
            go (item : acc) rest''
          (_, e) : rest'
            | e `elem` specialChars -> go (addLiteral (T.singleton e) acc) rest'
            | isAlpha e ->
                Left . FormatError i $
                  "%" <> T.singleton e <> " is not a tag; tags are written %{artist}, %{title}, ..."
            | otherwise ->
                Left . FormatError i $ "%" <> T.singleton e <> " is not an escape; use %% for %"
          [] -> Left $ FormatError i "% at the end; use %% for %"
        '[' -> do
          (alternatives, rest') <- parseAlternatives i rest
          go (Alternatives alternatives : acc) rest'
        '|' -> case context of
          InAlternatives -> done acc input
          TopLevel -> Left $ FormatError i "| outside [...]; use %| for |"
          InSpan -> Left $ FormatError i "| inside a styled span; close it with </> first"
        ']' -> case context of
          InAlternatives -> done acc input
          TopLevel -> Left $ FormatError i "] without [; use %] for ]"
          InSpan -> Left $ FormatError i "] inside a styled span; close it with </> first"
        '<' -> case rest of
          (_, '/') : (_, '>') : _ -> case context of
            InSpan -> done acc ((i, c) : rest)
            -- A span may be open outside the [...], but it can't end in it.
            InAlternatives ->
              Left $
                FormatError i "</> without a styled span open in this [...]; a span can't cross [ or ]"
            TopLevel -> Left $ FormatError i "</> without an open styled span"
          _ -> do
            let (styleChars, rest') = break ((== '>') . snd) rest
            case rest' of
              [] -> Left $ FormatError i "< isn't closed with >; use %< for <"
              _ : rest'' -> case parseSpanStyle (T.pack (map snd styleChars)) of
                Left err -> Left $ FormatError (i + 1) err
                Right s -> do
                  (items, rest''') <- parseSequence parseSpanStyle InSpan rest''
                  case rest''' of
                    (_, '<') : (_, '/') : (_, '>') : rest'''' -> go (Styled s items : acc) rest''''
                    _ -> Left $ FormatError i "a styled span isn't closed with </>"
        _ -> go (addLiteral (T.singleton c) acc) rest

    done :: [Item s] -> Input -> Either FormatError ([Item s], Input)
    done acc rest = Right (reverse acc, rest)

    parseAlternatives :: Int -> Input -> Either FormatError ([[Item s]], Input)
    parseAlternatives start input = do
      (alternative, rest) <- parseSequence parseSpanStyle InAlternatives input
      case rest of
        (_, '|') : rest' -> do
          (alternatives, rest'') <- parseAlternatives start rest'
          Right (alternative : alternatives, rest'')
        (_, ']') : rest' -> Right ([alternative], rest')
        _ -> Left $ FormatError start "[ isn't closed with ]"

    parseFieldItem :: Int -> Input -> Either FormatError (Item s, Input)
    parseFieldItem start input =
      let (nameChars, rest) = break ((`elem` [':', '}']) . snd) input
          name = T.pack (map snd nameChars)
      in case lookup name [(fieldName f, f) | f <- fields] of
           Nothing ->
             Left . FormatError (start + 2) $
               "unknown tag \""
                 <> name
                 <> "\", expected one of: "
                 <> T.intercalate ", " (map fieldName fields)
           Just field -> case rest of
             (_, '}') : rest' -> Right (FieldItem field Nothing, rest')
             (i, ':') : rest' ->
               let (digits, rest'') = span (isDigit . snd) rest'
               in case (digits, rest'') of
                    (_ : _, (_, '}') : rest''')
                      | Just w <- decimalIn 1 maxBound (T.pack (map snd digits)) ->
                          Right (FieldItem field (Just w), rest''')
                    _ -> Left $ FormatError (i + 1) "expected a width from 1 and }"
             _ -> Left $ FormatError start "%{ isn't closed with }"

addLiteral :: T.Text -> [Item s] -> [Item s]
addLiteral t = \case
  Literal l : acc -> Literal (l <> t) : acc
  acc -> Literal t : acc

-- | The characters that need a @%@ before them in a literal.
specialChars :: [Char]
specialChars = "%[]|<"

instance FromYaml (Format Style) where
  parseYaml = formatDecoder parseStyledFormat

-- | A format for plain text rejects styles.
instance FromYaml (Format Void) where
  parseYaml = formatDecoder parsePlainFormat

formatDecoder :: (T.Text -> Either FormatError (Format s)) -> Node -> Parser (Format s)
formatDecoder parse n = withText (either (failAt n . formatError) pure . parse) n
  where
    formatError :: FormatError -> String
    formatError err =
      "at character " <> show (err.position + 1) <> " of the format: " <> T.unpack err.message

-- | Print a format, so that 'parseFormat' reads it back.
printFormat :: (s -> T.Text) -> Format s -> T.Text
printFormat printStyle (Format items) = printItems items
  where
    printItems = T.concat . map printItem

    printItem = \case
      Literal t -> T.concatMap escape t
      FieldItem f w -> "%{" <> fieldName f <> maybe "" ((":" <>) . T.pack . show) w <> "}"
      Alternatives as -> "[" <> T.intercalate "|" (map printItems as) <> "]"
      Styled s is -> "<" <> printStyle s <> ">" <> printItems is <> "</>"

    escape c
      | c `elem` specialChars = T.pack ['%', c]
      | otherwise = T.singleton c

----------------------------------------
-- Rendering

-- | Text with a style. 'Nothing' is the style of the surrounding text.
data Span s = Span
  { style :: Maybe s
  , text :: T.Text
  }
  deriving stock (Eq, Show)

data RenderContext s = RenderContext
  { tagSeparator :: T.Text
  -- ^ Between the values of a tag with more than one value.
  , missingTag :: [Span s]
  -- ^ What a missing tag outside any @[...]@ renders as.
  }

-- | Render a format for a song. The styles of nested spans combine with
-- '<>', the inner one laid over the outer one.
renderFormat
  :: forall s. (Eq s, Semigroup s) => RenderContext s -> Song -> Format s -> [Span s]
renderFormat ctx song (Format items) = mergeSpans . fromMaybe [] $ renderItems True Nothing items
  where
    -- Nothing means that a field is missing. At the top level, a missing
    -- field renders as the marker instead.
    renderItems :: Bool -> Maybe s -> [Item s] -> Maybe [Span s]
    renderItems topLevel style is = concat <$> traverse (renderItem topLevel style) is

    renderItem :: Bool -> Maybe s -> Item s -> Maybe [Span s]
    renderItem topLevel style = \case
      Literal t -> Just [Span style t]
      FieldItem f width -> case fieldValue ctx.tagSeparator song f of
        Just v -> Just [Span style (maybe v (`truncateToWidth` v) width)]
        Nothing
          | topLevel -> Just [Span (style <> s.style) s.text | s <- ctx.missingTag]
          | otherwise -> Nothing
      Alternatives as -> Just . fromMaybe [] . listToMaybe $ mapMaybe (renderItems False style) as
      Styled s is -> renderItems topLevel (style <> Just s) is

-- | Render a format without styles to text.
renderPlain :: RenderContext Void -> Song -> Format Void -> T.Text
renderPlain ctx song = spansText . renderFormat ctx song

-- | The value of a field, if the song has it and it isn't empty.
fieldValue :: T.Text -> Song -> Field -> Maybe T.Text
fieldValue separator song =
  nonEmpty . \case
    ArtistField -> tag Artist
    AlbumArtistField -> tag AlbumArtist
    AlbumField -> tag Album
    TitleField -> tag Title
    TrackField -> normalizeTrack <$> firstTag Track song
    TrackRawField -> tag Track
    DiscField -> tag Disc
    DateField -> tag Date
    YearField -> T.takeWhile isDigit <$> firstTag Date song
    GenreField -> tag Genre
    ComposerField -> tag Composer
    PerformerField -> tag Performer
    CommentField -> tag Comment
    PriorityField -> Just . T.pack $ show song.priority
    LengthField -> formatDuration <$> song.duration
    FileField -> Just song.file
    FilenameField -> Just $ baseName song.file
    DirectoryField -> Just $ directoryOf song.file
  where
    nonEmpty :: Maybe T.Text -> Maybe T.Text
    nonEmpty = \case
      Just t | not (T.null t) -> Just t
      _ -> Nothing

    tag :: Tag -> Maybe T.Text
    tag t = T.intercalate separator . L.nub <$> M.lookup t song.tags

    normalizeTrack :: T.Text -> T.Text
    normalizeTrack t =
      let n = T.takeWhile (/= '/') t
      in if T.length n == 1 && T.all isDigit n then "0" <> n else n

-- | What a song is known by, e.g. in a title or the name of its lyrics, as
-- ncmpcpp names lyrics: the first artist and the first title, or without
-- both, the name of the song's file without its extension.
songName :: Song -> T.Text
songName song = case (firstTag Artist song, firstTag Title song) of
  (Just artist, Just title) -> artist <> " - " <> title
  _ -> T.pack . dropExtension . takeFileName $ T.unpack song.file

-- | @m:ss@, or @h:mm:ss@ from an hour.
formatDuration :: Seconds -> T.Text
formatDuration s =
  let total = floor s :: Int
      (h, rest) = total `divMod` 3600
      (m, sec) = rest `divMod` 60
      pad2 n = T.justifyRight 2 '0' (T.pack (show n))
  in if h > 0
       then T.pack (show h) <> ":" <> pad2 m <> ":" <> pad2 sec
       else T.pack (show m) <> ":" <> pad2 sec

spansText :: [Span s] -> T.Text
spansText = T.concat . map (.text)

-- | Spans with a style laid over ranges of the characters of their text,
-- each from its start and with its length, e.g. the matches of a find.
highlightSpans :: forall s. Monoid s => s -> [(Int, Int)] -> [Span s] -> [Span s]
highlightSpans over ranges = go 0
  where
    go :: Int -> [Span s] -> [Span s]
    go at = \case
      [] -> []
      Span st t : rest ->
        [ Span (if covered i then Just (fromMaybe mempty st <> over) else st) piece
        | (i, piece) <- cut at t
        , not (T.null piece)
        ]
          <> go (at + T.length t) rest

    -- The text cut where the ranges begin and end, each piece with the
    -- index of its first character.
    cut :: Int -> T.Text -> [(Int, T.Text)]
    cut at t =
      let edges =
            L.nub . L.sort $
              [e | (s, n) <- ranges, e <- [s, s + n], e > at, e < at + T.length t]
          lengths = zipWith (-) (edges <> [at + T.length t]) (at : edges)
      in snd $
           L.mapAccumL (\(i, rest) n -> ((i + n, T.drop n rest), (i, T.take n rest))) (at, t) lengths

    covered :: Int -> Bool
    covered i = any (\(s, n) -> i >= s && i < s + n) ranges

spansWidth :: [Span s] -> Int
spansWidth = sum . map (textWidth . (.text))

-- | Shorten spans to at most the given width, with an ellipsis in the
-- style of the last span that fits if they were too wide.
fitSpans :: Int -> [Span s] -> [Span s]
fitSpans width spans
  | spansWidth spans <= width = spans
  | width < textWidth ellipsis = []
  | otherwise = go (width - textWidth ellipsis) spans
  where
    go :: Int -> [Span s] -> [Span s]
    go room = \case
      [] -> []
      s : rest
        | textWidth s.text < room -> s : go (room - textWidth s.text) rest
        | otherwise -> [Span s.style (takeWidth room s.text <> ellipsis)]

-- | Drop empty spans and join neighbours with the same style.
mergeSpans :: Eq s => [Span s] -> [Span s]
mergeSpans = foldr add [] . filter (not . T.null . (.text))
  where
    add :: Eq s => Span s -> [Span s] -> [Span s]
    add s = \case
      next : rest | next.style == s.style -> Span s.style (s.text <> next.text) : rest
      rest -> s : rest
