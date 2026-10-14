-- | Lyrics from tekstowo.pl, a site of lyrics with many that LRCLIB doesn't
-- have. It has no API, so the fetcher reads its pages, and it breaks when
-- the site changes them.
module Reprise.Lyrics.Tekstowo
  ( tekstowo
  , tekstowoLyrics
  , searchResults
  , songLyrics
  ) where

import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Text.HTML.TagSoup

import Reprise.Lyrics
import Reprise.Lyrics.Http
import Reprise.Mpd.Protocol.Types hiding (Tag)

tekstowo :: Get -> Fetcher
tekstowo = Fetcher "tekstowo.pl" . tekstowoLyrics

-- | The lyrics of a song from tekstowo.pl: of the song that its search
-- finds by the same artist and title, with or without what follows the
-- title in brackets. Another song's lyrics would be stored as the song's,
-- so a result that is only close isn't taken.
tekstowoLyrics :: Get -> Song -> IO FetchResult
tekstowoLyrics get song = byArtistAndTitle song $ \artist title -> do
  let wanted = map (key . ((artist <> " - ") <>)) (L.nub [title, cleanTitle title])
  get "/szukaj" [("search-query", artist <> " " <> cleanTitle title)] >>= \case
    Right (200, body) -> case searchResults (T.decodeUtf8Lenient body) of
      Nothing -> pure changed
      Just results -> case [p | (p, t) <- results, key t `elem` wanted] of
        songPath : _ ->
          get (T.encodeUtf8 songPath) [] >>= \case
            Right (200, page) -> pure $ case songLyrics (T.decodeUtf8Lenient page) of
              Nothing -> changed
              Just text
                | T.null text -> FetchedNothing
                | otherwise -> FetchedLyrics (plainLyrics text)
            other -> pure $ failure other
        [] -> pure FetchedNothing
    -- A search that finds nothing redirects to the advanced search.
    Right (status, _) | status `div` 100 == 3 -> pure FetchedNothing
    other -> pure $ failure other
  where
    -- What a song is matched by, without case and extra spaces.
    key :: T.Text -> T.Text
    key = T.unwords . T.words . T.toCaseFold

    -- A page without what every page of its kind has. Taken for a page
    -- without lyrics, it would hide that the fetcher no longer works.
    changed :: FetchResult
    changed = FetchFailed "tekstowo.pl's page can't be read, as the site may have changed it"

    failure :: Either T.Text (Int, a) -> FetchResult
    failure = \case
      Left reason -> FetchFailed reason
      Right (status, _) -> FetchFailed $ answeredWith "tekstowo.pl" status

-- | The songs that a page of the search found: the path of each, and its
-- artist and title, e.g. @/kult/arahja@ and @Kult - Arahja@. They are the
-- links of the section of songs, up to the next heading; the page has other
-- songs elsewhere, e.g. the popular ones. Nothing for a page without the
-- section, which every page of a search has, also without songs.
searchResults :: T.Text -> Maybe [(T.Text, T.Text)]
searchResults page = case dropWhile (not . heading) (parseTags page) of
  [] -> Nothing
  _ : rest ->
    Just
      [ (href, title)
      | TagOpen "a" attributes <- takeWhile (not . isTagOpenName "h2") rest
      , hasClass "title" attributes
      , Just href <- [lookup "href" attributes]
      , Just title <- [lookup "title" attributes]
      ]
  where
    heading :: Tag T.Text -> Bool
    heading t = isTagText t && T.strip (fromTagText t) == "Znalezione utwory:"

-- | The lyrics on the page of a song: its first text, as the second is the
-- translation. A @<br />@ ends a line, and other tags aren't text. Nothing
-- for a page without the text, which every page of a song has, empty if
-- the song has no lyrics.
songLyrics :: T.Text -> Maybe T.Text
songLyrics page = case dropWhile (not . lyricsStart) (parseTags page) of
  [] -> Nothing
  _ : rest ->
    Just . T.strip . T.unlines . map T.stripEnd . T.lines . T.concat . map lyric $
      inside (1 :: Int) rest
  where
    lyricsStart :: Tag T.Text -> Bool
    lyricsStart = \case
      TagOpen "div" attributes -> hasClass "inner-text" attributes
      _ -> False

    -- The tags up to the end of the div at a depth of nested divs.
    inside :: Int -> [Tag T.Text] -> [Tag T.Text]
    inside depth = \case
      [] -> []
      t : ts
        | isTagCloseName "div" t -> if depth == 1 then [] else t : inside (depth - 1) ts
        | isTagOpenName "div" t -> t : inside (depth + 1) ts
        | otherwise -> t : inside depth ts

    -- The page's line ends are in its markup, and @<br />@ is the lyrics'.
    lyric :: Tag T.Text -> T.Text
    lyric = \case
      TagText t -> T.filter (`notElem` ['\r', '\n']) t
      TagOpen "br" _ -> "\n"
      _ -> ""

-- | Whether a tag has a class among its classes, which the site adds to,
-- e.g. the translation's "inner-text " with a space.
hasClass :: T.Text -> [Attribute T.Text] -> Bool
hasClass c = maybe False ((c `elem`) . T.words) . lookup "class"
