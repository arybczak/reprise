-- | The action registry: every action with its name, argument parser and
-- description. Keymaps and the @:@ prompt read actions with 'parseAction'.
--
-- Actions are data, so that the config can hold them. "Reprise.Handler" runs
-- them.
module Reprise.Action
  ( -- * Actions
    Action (..)
  , MoveTarget (..)
  , SelectTarget (..)
  , FindTarget (..)
  , AddPosition (..)
  , ToggleTarget (..)
  , VolumeChange (..)
  , SeekStep (..)
  , UpdateScope (..)
  , MoveSelectionTarget (..)

    -- * Screens
  , ScreenName (..)
  , screenName
  , screenFromName
  , screenNames

    -- * Registry
  , ActionSpec (..)
  , registry
  , parseAction
  , actionHint
  , renderAction
  , describeAction
  , isDestructive
  ) where

import Control.Monad
import Data.Char
import Data.List qualified as L
import Data.Text qualified as T
import Yamlet

----------------------------------------
-- Actions

data Action
  = Move MoveTarget
  | JumpToPlaying
  | JumpToBrowser
  | Back
  | Select SelectTarget
  | Activate
  | AddOrRemove
  | Delete
  | Parent
  | NextColumn
  | PreviousColumn
  | Pause
  | Stop
  | Previous
  | Next
  | Replay
  | Seek SeekStep
  | Volume VolumeChange
  | -- | Seconds.
    Crossfade Int
  | Find FindTarget
  | Filter
  | Show ScreenName
  | NextScreen [ScreenName]
  | PreviousScreen [ScreenName]
  | -- | The @:@ prompt, with the start of its line, e.g. an action's name
    -- whose arguments the user then types.
    CommandPrompt T.Text
  | Quit
  | Add AddPosition
  | AddAndPlay
  | AddPath T.Text
  | Clear
  | Shuffle
  | Save
  | Toggle ToggleTarget
  | Update UpdateScope
  | MoveSelection MoveSelectionTarget
  | Priority Int
  | NextSortMode
  | RefetchLyrics
  | EditLyrics
  deriving stock (Eq, Show)

data MoveTarget
  = MoveUp
  | MoveDown
  | MovePageUp
  | MovePageDown
  | MoveFirst
  | MoveLast
  | MovePreviousAlbum
  | MoveNextAlbum
  | MovePreviousArtist
  | MoveNextArtist
  deriving stock (Eq, Show, Enum, Bounded)

data SelectTarget
  = -- | Toggle the selection of the item, then move.
    SelectItem (Maybe MoveTarget)
  | SelectRange
  | SelectInvert
  | SelectNone
  | SelectAlbum
  | SelectArtist
  | SelectFound
  deriving stock (Eq, Show)

data FindTarget = FindForward | FindBackward | FindNext | FindPrevious
  deriving stock (Eq, Show, Enum, Bounded)

data AddPosition = AddEnd | AddNext | AddBeginning
  deriving stock (Eq, Show, Enum, Bounded)

data ToggleTarget
  = ToggleRepeat
  | ToggleRandom
  | ToggleSingle
  | ToggleConsume
  | -- | Between 0 and the given number of seconds.
    ToggleCrossfade Int
  | ToggleReplayGain
  | ToggleDisplay
  | ToggleAlbumSeparators
  | ToggleFollowPlaying
  | ToggleBitrate
  | ToggleVisualization
  deriving stock (Eq, Show)

data VolumeChange = VolumeBy Int | VolumeTo Int
  deriving stock (Eq, Show)

data SeekStep
  = -- | Seconds forward or backward. Holding the key goes further.
    SeekBy Int
  | SeekToSecond Int
  | SeekToPercent Int
  deriving stock (Eq, Show)

data UpdateScope = UpdateCurrent | UpdateAll
  deriving stock (Eq, Show, Enum, Bounded)

data MoveSelectionTarget
  = MoveSelectionUp
  | MoveSelectionDown
  | MoveSelectionToCursor
  | MoveSelectionToEnd
  deriving stock (Eq, Show, Enum, Bounded)

----------------------------------------
-- Screens

data ScreenName
  = QueueScreen
  | BrowserScreen
  | SearchEngineScreen
  | MediaLibraryScreen
  | PlaylistEditorScreen
  | OutputsScreen
  | VisualizerScreen
  | LyricsScreen
  | SongInfoScreen
  | HelpScreen
  deriving stock (Eq, Ord, Show, Enum, Bounded)

screenName :: ScreenName -> T.Text
screenName = \case
  QueueScreen -> "queue"
  BrowserScreen -> "browser"
  SearchEngineScreen -> "search_engine"
  MediaLibraryScreen -> "media_library"
  PlaylistEditorScreen -> "playlist_editor"
  OutputsScreen -> "outputs"
  VisualizerScreen -> "visualizer"
  LyricsScreen -> "lyrics"
  SongInfoScreen -> "song_info"
  HelpScreen -> "help"

screenNames :: [ScreenName]
screenNames = [minBound .. maxBound]

screenFromName :: T.Text -> Either T.Text ScreenName
screenFromName = choice "screen" [(screenName s, s) | s <- screenNames]

instance FromYaml ScreenName where
  parseYaml = oneOf [(screenName s, s) | s <- screenNames]

instance FromYaml Action where
  parseYaml n = withText (either (failAt n . T.unpack) pure . parseAction) n

----------------------------------------
-- Registry

-- | An action as the registry lists it.
data ActionSpec = ActionSpec
  { name :: T.Text
  , usage :: T.Text
  -- ^ The arguments, e.g. @+N | -N | N@.
  , parse :: T.Text -> Either T.Text Action
  -- ^ Parse the text after the name.
  }

-- | An argument: a word, or a list in brackets, e.g. @[browser, outputs]@.
data Argument = Word T.Text | List [T.Text]
  deriving stock (Eq, Show)

registry :: [ActionSpec]
registry =
  [ spec "move" (alternatives (map fst moveTargets)) $ one (fmap Move . moveTarget)
  , spec "jump_to_playing" "" $ none JumpToPlaying
  , spec "jump_to_browser" "" $ none JumpToBrowser
  , spec "back" "" $ none Back
  , spec "select" "[up | down] | range | invert | none | album | artist | found" $ \case
      [] -> Right . Select $ SelectItem Nothing
      [Word w] -> Select <$> choice "selection" selectTargets w
      _ -> Left "expected at most one argument"
  , spec "activate" "" $ none Activate
  , spec "add_or_remove" "" $ none AddOrRemove
  , spec "delete" "" $ none Delete
  , spec "parent" "" $ none Parent
  , spec "next_column" "" $ none NextColumn
  , spec "previous_column" "" $ none PreviousColumn
  , spec "pause" "" $ none Pause
  , spec "stop" "" $ none Stop
  , spec "previous" "" $ none Previous
  , spec "next" "" $ none Next
  , spec "replay" "" $ none Replay
  , spec "seek" "+Ns | -Ns | m:ss | N%" $ one (fmap Seek . seekStep)
  , spec "volume" "+N | -N | N" $ one (fmap Volume . volumeChange)
  , spec "crossfade" "SECONDS" $ one (fmap Crossfade . natural)
  , spec "find" "forward | backward | next | previous" $
      one
        ( fmap Find
            . choice
              "direction"
              [ ("forward", FindForward)
              , ("backward", FindBackward)
              , ("next", FindNext)
              , ("previous", FindPrevious)
              ]
        )
  , spec "filter" "" $ none Filter
  , spec "show" "SCREEN" $ one (fmap Show . screenFromName)
  , spec "next_screen" "[SCREEN, ...]" $ screenList NextScreen
  , spec "previous_screen" "[SCREEN, ...]" $ screenList PreviousScreen
  , ActionSpec "command" "[START OF THE LINE]" (Right . CommandPrompt)
  , spec "quit" "" $ none Quit
  , spec "add" "end | next | beginning" $
      one
        ( fmap Add
            . choice "position" [("end", AddEnd), ("next", AddNext), ("beginning", AddBeginning)]
        )
  , spec "add_and_play" "" $ none AddAndPlay
  , -- A path can hold spaces and brackets, so it is the rest of the line.
    ActionSpec "add_path" "PATH" $ \t ->
      if T.null t then Left "expected a path" else Right (AddPath t)
  , spec "clear" "" $ none Clear
  , spec "shuffle" "" $ none Shuffle
  , spec "save" "" $ none Save
  , spec "toggle" (alternatives ("crossfade N" : map fst toggleTargets)) $ \case
      [Word "crossfade", Word n] -> Toggle . ToggleCrossfade <$> natural n
      [Word w] -> Toggle <$> choice "option" toggleTargets w
      _ -> Left "expected an option"
  , spec "update" "current | all" $
      one (fmap Update . choice "scope" [("current", UpdateCurrent), ("all", UpdateAll)])
  , spec "move_selection" "up | down | cursor | end"
      $ one
      $ fmap MoveSelection
        . choice
          "target"
          [ ("up", MoveSelectionUp)
          , ("down", MoveSelectionDown)
          , ("cursor", MoveSelectionToCursor)
          , ("end", MoveSelectionToEnd)
          ]
  , spec "priority" "0-255" $ one (fmap Priority . priority)
  , spec "next_sort_mode" "" $ none NextSortMode
  , spec "refetch_lyrics" "" $ none RefetchLyrics
  , spec "edit_lyrics" "" $ none EditLyrics
  ]
  where
    -- An action whose arguments are words and lists.
    spec :: T.Text -> T.Text -> ([Argument] -> Either T.Text Action) -> ActionSpec
    spec name usage f = ActionSpec name usage (tokenize >=> f)

    alternatives :: [T.Text] -> T.Text
    alternatives = T.intercalate " | "

    none :: Action -> [Argument] -> Either T.Text Action
    none a = \case
      [] -> Right a
      _ -> Left "expected no arguments"

    one :: (T.Text -> Either T.Text Action) -> [Argument] -> Either T.Text Action
    one f = \case
      [Word w] -> f w
      [] -> Left "expected an argument"
      _ -> Left "expected one argument"

    screenList :: ([ScreenName] -> Action) -> [Argument] -> Either T.Text Action
    screenList f = \case
      [List ws] -> f <$> traverse screenFromName ws
      [] -> Right (f screenNames)
      _ -> Left "expected a list of screens, e.g. [browser, outputs]"

    moveTarget :: T.Text -> Either T.Text MoveTarget
    moveTarget = choice "direction" moveTargets

    selectTargets :: [(T.Text, SelectTarget)]
    selectTargets =
      [ ("up", SelectItem (Just MoveUp))
      , ("down", SelectItem (Just MoveDown))
      , ("range", SelectRange)
      , ("invert", SelectInvert)
      , ("none", SelectNone)
      , ("album", SelectAlbum)
      , ("artist", SelectArtist)
      , ("found", SelectFound)
      ]

    toggleTargets :: [(T.Text, ToggleTarget)]
    toggleTargets =
      [ ("repeat", ToggleRepeat)
      , ("random", ToggleRandom)
      , ("single", ToggleSingle)
      , ("consume", ToggleConsume)
      , ("replay_gain", ToggleReplayGain)
      , ("display", ToggleDisplay)
      , ("album_separators", ToggleAlbumSeparators)
      , ("follow_playing", ToggleFollowPlaying)
      , ("bitrate", ToggleBitrate)
      , ("visualization", ToggleVisualization)
      ]

    volumeChange :: T.Text -> Either T.Text VolumeChange
    volumeChange w = case T.uncons w of
      Just ('+', n) -> VolumeBy <$> natural n
      Just ('-', n) -> VolumeBy . negate <$> natural n
      _ -> do
        v <- natural w
        if v <= maxVolume then Right (VolumeTo v) else Left "a volume is from 0 to 100"

    seekStep :: T.Text -> Either T.Text SeekStep
    seekStep w
      | Just n <- T.stripSuffix "%" w = do
          p <- natural n
          if p <= 100 then Right (SeekToPercent p) else Left "a percentage is from 0 to 100"
      | Just (sign, rest) <- T.uncons w
      , sign `elem` ['+', '-'] = do
          n <- maybe (Left "expected seconds with an s, e.g. +5s") natural (T.stripSuffix "s" rest)
          Right . SeekBy $ if sign == '-' then negate n else n
      | otherwise = SeekToSecond <$> clockTime w

    clockTime :: T.Text -> Either T.Text Int
    clockTime w = case T.splitOn ":" w of
      parts@(_ : _ : _)
        | length parts <= 3 -> do
            ns <- traverse natural parts
            Right $ foldl (\acc n -> acc * 60 + n) 0 ns
      _ -> Left "expected +Ns, -Ns, m:ss or N%"

    priority :: T.Text -> Either T.Text Int
    priority w = do
      p <- natural w
      if p <= maxPriority then Right p else Left "a priority is from 0 to 255"

    maxVolume :: Int
    maxVolume = 100

    -- MPD's priorities are from 0 to 255.
    maxPriority :: Int
    maxPriority = 255

moveTargets :: [(T.Text, MoveTarget)]
moveTargets =
  [ ("up", MoveUp)
  , ("down", MoveDown)
  , ("page_up", MovePageUp)
  , ("page_down", MovePageDown)
  , ("first", MoveFirst)
  , ("last", MoveLast)
  , ("previous_album", MovePreviousAlbum)
  , ("next_album", MoveNextAlbum)
  , ("previous_artist", MovePreviousArtist)
  , ("next_artist", MoveNextArtist)
  ]

-- | Parse an action with its arguments, e.g. @volume +2@ or
-- @next_screen [browser, media_library]@.
parseAction :: T.Text -> Either T.Text Action
parseAction input = case T.break isSpace (T.strip input) of
  ("", _) -> Left "expected an action"
  (name, rest) -> case findSpec name of
    Just s -> case s.parse (T.strip rest) of
      Right a -> Right a
      Left err -> Left $ name <> ": " <> err <> "; usage: " <> usageOf s
    Nothing -> Left $ "unknown action " <> name <> suggestion name
  where
    suggestion :: T.Text -> T.Text
    suggestion name = case L.sortOn snd [(s.name, distance name s.name) | s <- registry] of
      (best, d) : _ | d <= max 1 (T.length name `div` 2) -> ", did you mean " <> best <> "?"
      _ -> ""

-- | What the @:@ prompt shows while the user types a line: what the line
-- would do, or how to write the action that it names.
actionHint :: T.Text -> T.Text
actionHint input = case parseAction input of
  Right a -> describeAction a
  Left _ -> maybe "" usageOf . findSpec . fst $ T.break isSpace (T.strip input)

findSpec :: T.Text -> Maybe ActionSpec
findSpec name = L.find (\s -> s.name == name) registry

usageOf :: ActionSpec -> T.Text
usageOf s = T.strip (s.name <> " " <> s.usage)

tokenize :: T.Text -> Either T.Text [Argument]
tokenize t0 = go (T.stripStart t0)
  where
    go :: T.Text -> Either T.Text [Argument]
    go t
      | T.null t = Right []
      | Just rest <- T.stripPrefix "[" t =
          let (inside, rest') = T.breakOn "]" rest
          in if T.null rest'
               then Left "[ isn't closed with ]"
               else
                 (List (filter (not . T.null) . map T.strip $ T.splitOn "," inside) :)
                   <$> go (T.stripStart (T.drop 1 rest'))
      | otherwise =
          let (w, rest) = T.break (\c -> isSpace c || c == '[') t
          in (Word w :) <$> go (T.stripStart rest)

-- | The text of an action, which 'parseAction' reads back.
renderAction :: Action -> T.Text
renderAction = \case
  Move t -> "move " <> moveName t
  JumpToPlaying -> "jump_to_playing"
  JumpToBrowser -> "jump_to_browser"
  Back -> "back"
  Select t -> case t of
    SelectItem Nothing -> "select"
    SelectItem (Just m) -> "select " <> moveName m
    SelectRange -> "select range"
    SelectInvert -> "select invert"
    SelectNone -> "select none"
    SelectAlbum -> "select album"
    SelectArtist -> "select artist"
    SelectFound -> "select found"
  Activate -> "activate"
  AddOrRemove -> "add_or_remove"
  Delete -> "delete"
  Parent -> "parent"
  NextColumn -> "next_column"
  PreviousColumn -> "previous_column"
  Pause -> "pause"
  Stop -> "stop"
  Previous -> "previous"
  Next -> "next"
  Replay -> "replay"
  Seek t ->
    "seek " <> case t of
      SeekBy n -> signed n <> "s"
      SeekToSecond n -> clock n
      SeekToPercent n -> T.pack (show n) <> "%"
  Volume v ->
    "volume " <> case v of
      VolumeBy n -> signed n
      VolumeTo n -> T.pack (show n)
  Crossfade n -> "crossfade " <> T.pack (show n)
  Find t ->
    "find " <> case t of
      FindForward -> "forward"
      FindBackward -> "backward"
      FindNext -> "next"
      FindPrevious -> "previous"
  Filter -> "filter"
  Show s -> "show " <> screenName s
  NextScreen ss -> "next_screen " <> screenList ss
  PreviousScreen ss -> "previous_screen " <> screenList ss
  CommandPrompt t -> T.strip ("command " <> t)
  Quit -> "quit"
  Add p ->
    "add " <> case p of
      AddEnd -> "end"
      AddNext -> "next"
      AddBeginning -> "beginning"
  AddAndPlay -> "add_and_play"
  AddPath p -> "add_path " <> p
  Clear -> "clear"
  Shuffle -> "shuffle"
  Save -> "save"
  Toggle t -> "toggle " <> toggleName t
  Update s ->
    "update " <> case s of
      UpdateCurrent -> "current"
      UpdateAll -> "all"
  MoveSelection t ->
    "move_selection " <> case t of
      MoveSelectionUp -> "up"
      MoveSelectionDown -> "down"
      MoveSelectionToCursor -> "cursor"
      MoveSelectionToEnd -> "end"
  Priority p -> "priority " <> T.pack (show p)
  NextSortMode -> "next_sort_mode"
  RefetchLyrics -> "refetch_lyrics"
  EditLyrics -> "edit_lyrics"
  where
    screenList :: [ScreenName] -> T.Text
    screenList ss = "[" <> T.intercalate ", " (map screenName ss) <> "]"

clock :: Int -> T.Text
clock n = T.pack (show (n `div` 60)) <> ":" <> T.justifyRight 2 '0' (T.pack (show (n `mod` 60)))

moveName :: MoveTarget -> T.Text
moveName t = maybe "?" fst $ L.find ((== t) . snd) moveTargets

toggleName :: ToggleTarget -> T.Text
toggleName = \case
  ToggleRepeat -> "repeat"
  ToggleRandom -> "random"
  ToggleSingle -> "single"
  ToggleConsume -> "consume"
  ToggleCrossfade n -> "crossfade " <> T.pack (show n)
  ToggleReplayGain -> "replay_gain"
  ToggleDisplay -> "display"
  ToggleAlbumSeparators -> "album_separators"
  ToggleFollowPlaying -> "follow_playing"
  ToggleBitrate -> "bitrate"
  ToggleVisualization -> "visualization"

signed :: Int -> T.Text
signed n = if n >= 0 then "+" <> T.pack (show n) else T.pack (show n)

-- | A short description for the which-key panel and the help screen.
describeAction :: Action -> T.Text
describeAction = \case
  Move t -> "move " <> T.replace "_" " " (moveName t)
  JumpToPlaying -> "jump to the playing song"
  JumpToBrowser -> "show the song under the cursor in the browser"
  Back -> "go back to the previous screen"
  Select t -> case t of
    SelectItem Nothing -> "toggle selection"
    SelectItem (Just m) -> "toggle selection, move " <> moveName m
    SelectRange -> "select between the last two selected"
    SelectInvert -> "invert selection"
    SelectNone -> "clear selection"
    SelectAlbum -> "select the album around the cursor"
    SelectArtist -> "select the artist around the cursor"
    SelectFound -> "select found items"
  Activate -> "activate"
  AddOrRemove -> "add or remove"
  Delete -> "delete"
  Parent -> "go to parent"
  NextColumn -> "next column"
  PreviousColumn -> "previous column"
  Pause -> "pause or resume"
  Stop -> "stop"
  Previous -> "previous song"
  Next -> "next song"
  Replay -> "replay song"
  Seek t -> case t of
    SeekBy n -> "seek " <> signed n <> "s"
    SeekToSecond n -> "seek to " <> clock n
    SeekToPercent n -> "seek to " <> T.pack (show n) <> "%"
  Volume v -> case v of
    VolumeBy n -> "change volume by " <> signed n
    VolumeTo n -> "set volume to " <> T.pack (show n)
  Crossfade n -> "set crossfade to " <> T.pack (show n) <> "s"
  Find t -> case t of
    FindForward -> "find forward"
    FindBackward -> "find backward"
    FindNext -> "find next"
    FindPrevious -> "find previous"
  Filter -> "filter"
  Show LyricsScreen -> "show the lyrics of the song under the cursor"
  Show SongInfoScreen -> "show the info of the song under the cursor"
  Show s -> "show " <> T.replace "_" " " (screenName s)
  NextScreen _ -> "next screen"
  PreviousScreen _ -> "previous screen"
  CommandPrompt t
    | T.null t -> "run an action"
    | otherwise -> ":" <> t <> " …"
  Quit -> "quit"
  Add p -> case p of
    AddEnd -> "add at the end"
    AddNext -> "add after the playing song"
    AddBeginning -> "add at the beginning"
  AddAndPlay -> "add and play"
  AddPath p -> "add " <> p
  Clear -> "clear"
  Shuffle -> "shuffle"
  Save -> "save as a playlist"
  Toggle t -> "toggle " <> T.replace "_" " " (toggleName t)
  Update s -> case s of
    UpdateCurrent -> "update the database here"
    UpdateAll -> "update the whole database"
  MoveSelection t -> case t of
    MoveSelectionUp -> "move selection up"
    MoveSelectionDown -> "move selection down"
    MoveSelectionToCursor -> "move selection above the cursor"
    MoveSelectionToEnd -> "move selection to the end"
  Priority p -> "set priority " <> T.pack (show p)
  NextSortMode -> "next sort mode"
  RefetchLyrics -> "fetch the lyrics again"
  EditLyrics -> "edit the lyrics"

-- | Whether the action may throw away something the user didn't point at,
-- so that it asks for confirmation. Its handler decides whether it does,
-- e.g. shuffling only the selected songs doesn't ask.
isDestructive :: Action -> Bool
isDestructive = \case
  Clear -> True
  Shuffle -> True
  Save -> True
  _ -> False

----------------------------------------
-- Helpers

choice :: T.Text -> [(T.Text, a)] -> T.Text -> Either T.Text a
choice what choices w = case lookup w choices of
  Just a -> Right a
  Nothing ->
    Left $
      "unknown "
        <> what
        <> " "
        <> w
        <> ", expected one of: "
        <> T.intercalate ", " (map fst choices)

natural :: T.Text -> Either T.Text Int
natural w = case reads (T.unpack w) of
  [(n, "")] | T.all isDigit w && n <= toInteger (maxBound @Int) -> Right (fromInteger n)
  _ -> Left $ "expected a number, not " <> w

-- | The edit distance, for suggestions.
distance :: T.Text -> T.Text -> Int
distance a b = last . foldl step [0 .. T.length a] $ T.unpack b
  where
    step :: [Int] -> Char -> [Int]
    step prev@(p : ps) c = scanl compute (p + 1) (zip3 (T.unpack a) prev ps)
      where
        compute :: Int -> (Char, Int, Int) -> Int
        compute left (ca, diag, up) = minimum [left + 1, up + 1, diag + if ca == c then 0 else 1]
    step [] _ = []
