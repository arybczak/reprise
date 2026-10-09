-- | Parsers for MPD's replies. They are pure, so they work on replies from
-- any source.
module Reprise.Mpd.Protocol.Response
  ( -- * Replies
    Field (..)
  , isFinalLine
  , parseReply
  , parseAck

    -- * Fields
  , FieldMap
  , fieldMap
  , required
  , optional
  , splitOn
  , decode

    -- * Values
  , readInt
  , readBool
  , readSeconds
  , readRange
  , readTime

    -- * Replies of commands
  , parseSong
  , parseSongs
  , parseEntries
  , parseStatus
  , parseStats
  , parseComments
  , parseOutputs
  , parseSubsystems
  , parseSingleMode
  , parseConsumeMode
  , parseReplayGainMode
  ) where

import Control.Applicative hiding (optional)
import Control.DeepSeq
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Fixed
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Proxy
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time qualified as Time
import Data.Time.Clock.POSIX
import Data.Time.FromText
import Data.Word

import Reprise.Mpd.Protocol.Types

----------------------------------------
-- Replies

-- | A @key: value@ line of a reply.
data Field = Field
  { key :: BS.ByteString
  , value :: BS.ByteString
  }
  deriving stock (Eq, Show)

-- | Whether a line (without its newline) ends a reply.
isFinalLine :: BS.ByteString -> Bool
isFinalLine l = l == "OK" || "ACK " `BS.isPrefixOf` l

-- | Parse the lines of one reply, without their newlines. The last line must
-- be @OK@ or an @ACK@.
--
-- The result has the fields of each part of the reply, split at @list_OK@.
-- The part after the last @list_OK@ is included, so the reply to a command
-- list of @n@ commands has @n + 1@ parts, the last one empty.
parseReply :: [BS.ByteString] -> Either MpdError [[Field]]
parseReply = go [] []
  where
    go :: [[Field]] -> [Field] -> [BS.ByteString] -> Either MpdError [[Field]]
    go parts fields = \case
      [] -> Left $ ProtocolError "the reply ended without OK or ACK"
      l : ls
        | l == "OK" -> case ls of
            [] -> Right $ reverse (reverse fields : parts)
            _ -> Left $ ProtocolError "the reply continues after OK"
        | l == "list_OK" -> go (reverse fields : parts) [] ls
        | "ACK " `BS.isPrefixOf` l -> case parseAck l of
            Just ack -> Left $ AckError ack
            Nothing -> Left . ProtocolError $ "malformed ACK: " <> decode l
        | otherwise -> case separator 0 l of
            Just i | i > 0 -> go parts (Field (BS.take i l) (BS.drop (i + 2) l) : fields) ls
            _ -> Left . ProtocolError $ "malformed line: " <> decode l

    -- The index of the first ": " from an index. 'BS.breakSubstring' made
    -- a search function for each line.
    separator :: Int -> BS.ByteString -> Maybe Int
    separator from l = do
      i <- (from +) <$> BS8.elemIndex ':' (BS.drop from l)
      if BS8.indexMaybe l (i + 1) == Just ' ' then Just i else separator (i + 1) l

-- | Parse an @ACK [code\@index] {command} message@ line.
parseAck :: BS.ByteString -> Maybe Ack
parseAck l0 = do
  l1 <- BS.stripPrefix "ACK [" l0
  (code, l2) <- BS8.readInt l1
  l3 <- BS.stripPrefix "@" l2
  (index, l4) <- BS8.readInt l3
  l5 <- BS.stripPrefix "] {" l4
  let (command, l6) = BS8.break (== '}') l5
  message <- BS.stripPrefix "} " l6 <|> BS.stripPrefix "}" l6
  pure
    Ack
      { code = ackCode code
      , index = index
      , command = decode command
      , message = decode message
      }
  where
    ackCode :: Int -> AckCode
    ackCode = \case
      1 -> AckNotList
      2 -> AckArg
      3 -> AckPassword
      4 -> AckPermission
      5 -> AckUnknown
      50 -> AckNoExist
      51 -> AckPlaylistMax
      52 -> AckSystem
      53 -> AckPlaylistLoad
      54 -> AckUpdateAlready
      55 -> AckPlayerSync
      56 -> AckExist
      n -> AckOther n

----------------------------------------
-- Fields

-- | The values of each key, in the order of the reply.
type FieldMap = M.Map BS.ByteString [BS.ByteString]

fieldMap :: [Field] -> FieldMap
fieldMap fields = M.fromListWith (flip (++)) [(f.key, [f.value]) | f <- fields]

-- | The first value of a key that must be present.
required :: BS.ByteString -> (BS.ByteString -> Maybe a) -> FieldMap -> Either T.Text a
required k parse m = case M.lookup k m of
  Just (v : _) -> parseValue k parse v
  _ -> Left $ "missing key: " <> decode k

-- | The first value of a key that may be absent.
optional
  :: BS.ByteString -> (BS.ByteString -> Maybe a) -> FieldMap -> Either T.Text (Maybe a)
optional k parse m = case M.lookup k m of
  Just (v : _) -> Just <$> parseValue k parse v
  _ -> Right Nothing

parseValue
  :: BS.ByteString -> (BS.ByteString -> Maybe a) -> BS.ByteString -> Either T.Text a
parseValue k parse v = case parse v of
  Just a -> Right a
  Nothing -> Left $ "bad value of " <> decode k <> ": " <> decode v

-- | Split fields into entries, each starting at a key that the predicate
-- accepts, e.g. @file@ for songs.
splitOn :: (BS.ByteString -> Bool) -> [Field] -> Either T.Text [[Field]]
splitOn isStart = \case
  [] -> Right []
  f : fs
    | isStart f.key ->
        let (entry, rest) = break (\g -> isStart g.key) fs
        in ((f : entry) :) <$> splitOn isStart rest
    | otherwise -> Left $ "unexpected key: " <> decode f.key

-- | Decode a value as UTF-8. Invalid bytes become U+FFFD, so a broken tag
-- doesn't make the whole reply fail.
decode :: BS.ByteString -> T.Text
decode = T.decodeUtf8Lenient

----------------------------------------
-- Values

readInt :: BS.ByteString -> Maybe Int
readInt s = case BS8.readInt s of
  Just (n, rest) | BS.null rest -> Just n
  _ -> Nothing

readBool :: BS.ByteString -> Maybe Bool
readBool = \case
  "0" -> Just False
  "1" -> Just True
  _ -> Nothing

-- | A non-negative decimal number. Digits past the precision of 'Seconds'
-- are dropped.
readSeconds :: BS.ByteString -> Maybe Seconds
readSeconds s = case BS8.break (== '.') s of
  (whole, frac) -> do
    w <- readDigits whole
    f <- case BS.uncons frac of
      Nothing -> Just 0
      Just (_, digits)
        | allDigits digits ->
            readDigits (BS.take precision (digits <> BS8.replicate precision '0'))
        | otherwise -> Nothing
    pure . Seconds . MkFixed $ w * scale + f
  where
    scale :: Integer
    scale = resolution (Proxy @E3)

    precision :: Int
    precision = length . takeWhile (> 1) $ iterate (`div` 10) scale

    readDigits :: BS.ByteString -> Maybe Integer
    readDigits d
      | allDigits d = fst <$> BS8.readInteger d
      | otherwise = Nothing

    allDigits :: BS.ByteString -> Bool
    allDigits d = not (BS.null d) && BS8.all (\c -> c >= '0' && c <= '9') d

-- | A part of a song's file, e.g. @0.000-2.000@, or @2.000-@ up to the end.
readRange :: BS.ByteString -> Maybe SongRange
readRange s = case BS8.break (== '-') s of
  (start, rest) -> do
    end <- BS.stripPrefix "-" rest
    SongRange
      <$> readSeconds start
      <*> if BS.null end then Just Nothing else Just <$> readSeconds end

-- | An ISO 8601 time, e.g. @2024-01-02T03:04:05Z@.
--
-- The parser of the time library took most of the time of reading a reply
-- to @plchanges@.
readTime :: BS.ByteString -> Maybe Time.UTCTime
readTime = either (const Nothing) Just . parseUTCTime . decode

----------------------------------------
-- Replies of commands

-- The parsers of this section return fully evaluated values. A value left
-- lazy would keep its slice of the reply, and with it the whole buffer that
-- the slice is in.

-- | Parse one song. The first field must be @file@.
parseSong :: [Field] -> Either T.Text Song
parseSong = parseSingle song

-- | A song, before it is evaluated.
song :: [Field] -> Either T.Text Song
song = \case
  Field "file" file : rest -> songFrom file (L.foldl' addSongField noSongFields rest)
  _ -> Left "a song doesn't start with the file key"

-- | Parse a list of songs, e.g. the reply to @playlistinfo@.
--
-- It reads the fields of each song in one pass and evaluates the song
-- right away: after a deletion in a long queue, the reply to @plchanges@
-- has a song for each one after the deleted one.
parseSongs :: [Field] -> Either T.Text [Song]
parseSongs = go []
  where
    go :: [Song] -> [Field] -> Either T.Text [Song]
    go songs = \case
      [] -> Right $! reverse songs
      Field "file" file : rest -> do
        let (fields, next) = collectSong (== "file") noSongFields rest
        s <- parseSingle (songFrom file) fields
        go (s : songs) next
      f : _ -> Left $ "unexpected key: " <> decode f.key

-- | Parse a directory listing, e.g. the reply to @lsinfo@, in one pass as
-- 'parseSongs' does.
parseEntries :: [Field] -> Either T.Text [Entry]
parseEntries = go []
  where
    go :: [Entry] -> [Field] -> Either T.Text [Entry]
    go entries = \case
      [] -> Right $! reverse entries
      Field "file" file : rest -> do
        let (fields, next) = collectSong isEntryKey noSongFields rest
        s <- parseSingle (songFrom file) fields
        go (SongEntry s : entries) next
      Field "directory" path : rest ->
        named entries (\t -> DirectoryEntry (Directory (decode path) t)) rest
      Field "playlist" path : rest ->
        named entries (\t -> PlaylistEntry (Playlist (decode path) t)) rest
      f : _ -> Left $ "unexpected key: " <> decode f.key

    -- A directory or a playlist, which has no other field to keep.
    named :: [Entry] -> (Maybe Time.UTCTime -> Entry) -> [Field] -> Either T.Text [Entry]
    named entries entry rest = do
      let (fields, next) = break (isEntryKey . (.key)) rest
      e <- parseSingle (fmap entry . optional "Last-Modified" readTime . fieldMap) fields
      go (e : entries) next

    isEntryKey :: BS.ByteString -> Bool
    isEntryKey k = k == "file" || k == "directory" || k == "playlist"

-- | The fields of a song up to the key that starts the next entry.
collectSong :: (BS.ByteString -> Bool) -> SongFields -> [Field] -> (SongFields, [Field])
collectSong isNext !fields = \case
  next@(f : _) | isNext f.key -> (fields, next)
  f : fs -> collectSong isNext (addSongField fields f) fs
  [] -> (fields, [])

-- | The fields of a song after its @file@ key, not parsed yet: the first
-- value of each key, and the tags.
data SongFields = SongFields
  { rawDuration :: Maybe BS.ByteString
  , rawTime :: Maybe BS.ByteString
  , rawRange :: Maybe BS.ByteString
  , rawLastModified :: Maybe BS.ByteString
  , rawPosition :: Maybe BS.ByteString
  , rawId :: Maybe BS.ByteString
  , rawPriority :: Maybe BS.ByteString
  , rawFormat :: Maybe BS.ByteString
  , rawTags :: [(Tag, BS.ByteString)]
  -- ^ In the reverse order of the reply.
  }

noSongFields :: SongFields
noSongFields = SongFields Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

addSongField :: SongFields -> Field -> SongFields
addSongField s (Field k v) = case k of
  "duration" -> s {rawDuration = s.rawDuration <|> Just v}
  "Time" -> s {rawTime = s.rawTime <|> Just v}
  "Range" -> s {rawRange = s.rawRange <|> Just v}
  "Last-Modified" -> s {rawLastModified = s.rawLastModified <|> Just v}
  "Pos" -> s {rawPosition = s.rawPosition <|> Just v}
  "Id" -> s {rawId = s.rawId <|> Just v}
  "Prio" -> s {rawPriority = s.rawPriority <|> Just v}
  "Format" -> s {rawFormat = s.rawFormat <|> Just v}
  _ -> case M.lookup k tagsByKey of
    Just t -> s {rawTags = (t, v) : s.rawTags}
    Nothing -> s

tagsByKey :: M.Map BS.ByteString Tag
tagsByKey = M.fromList [(T.encodeUtf8 (tagName t), t) | t <- [minBound .. maxBound]]

-- | A song from its file and its other fields, before it is evaluated.
songFrom :: BS.ByteString -> SongFields -> Either T.Text Song
songFrom file s = do
  duration <- traverse (parseValue "duration" readSeconds) s.rawDuration
  time <- traverse (parseValue "Time" readInt) s.rawTime
  range <- traverse (parseValue "Range" readRange) s.rawRange
  lastModified <- traverse (parseValue "Last-Modified" readTime) s.rawLastModified
  position <- traverse (parseValue "Pos" readInt) s.rawPosition
  songId <- traverse (parseValue "Id" readInt) s.rawId
  priority <- traverse (parseValue "Prio" readInt) s.rawPriority
  pure
    Song
      { file = decode file
      , tags = M.fromListWith (flip (++)) [(t, [decode v]) | (t, v) <- reverse s.rawTags]
      , duration = maybe (fromIntegral <$> time) Just duration
      , range = range
      , lastModified = lastModified
      , format = decode <$> s.rawFormat
      , position = SongPos <$> position
      , songId = SongId <$> songId
      , priority = fromMaybe 0 priority
      }

parseStatus :: [Field] -> Either T.Text Status
parseStatus = parseSingle $ \fields -> do
  let m = fieldMap fields
  volume <- optional "volume" readInt m
  repeat_ <- required "repeat" readBool m
  random <- required "random" readBool m
  single <- required "single" parseSingleMode m
  consume <- required "consume" parseConsumeMode m
  playlistVersion <- required "playlist" readVersion m
  playlistLength <- required "playlistlength" readInt m
  state <- required "state" readState m
  currentPosition <- optional "song" readInt m
  currentId <- optional "songid" readInt m
  nextPosition <- optional "nextsong" readInt m
  nextId <- optional "nextsongid" readInt m
  elapsed <- optional "elapsed" readSeconds m
  duration <- optional "duration" readSeconds m
  bitrate <- optional "bitrate" readInt m
  crossfade <- optional "xfade" readInt m
  audio <- optional "audio" Just m
  updatingDb <- optional "updating_db" readInt m
  err <- optional "error" Just m
  pure
    Status
      { volume = volume
      , repeat = repeat_
      , random = random
      , single = single
      , consume = consume
      , playlistVersion = playlistVersion
      , playlistLength = playlistLength
      , state = state
      , currentPosition = SongPos <$> currentPosition
      , currentId = SongId <$> currentId
      , nextPosition = SongPos <$> nextPosition
      , nextId = SongId <$> nextId
      , elapsed = elapsed
      , duration = duration
      , bitrate = bitrate
      , crossfade = fromMaybe 0 crossfade
      , audio = decode <$> audio
      , updatingDb = updatingDb
      , error = decode <$> err
      }
  where
    readState :: BS.ByteString -> Maybe PlayerState
    readState = \case
      "play" -> Just Playing
      "pause" -> Just Paused
      "stop" -> Just Stopped
      _ -> Nothing

    readVersion :: BS.ByteString -> Maybe PlaylistVersion
    readVersion s = case BS8.readInteger s of
      Just (n, rest)
        | BS.null rest && n >= 0 && n <= toInteger (maxBound @Word32) ->
            Just . PlaylistVersion $ fromInteger n
      _ -> Nothing

parseStats :: [Field] -> Either T.Text Stats
parseStats = parseSingle $ \fields -> do
  let m = fieldMap fields
  artists <- required "artists" readInt m
  albums <- required "albums" readInt m
  songs <- required "songs" readInt m
  uptime <- required "uptime" readInt m
  playtime <- required "playtime" readInt m
  dbPlaytime <- required "db_playtime" readInt m
  dbUpdate <- optional "db_update" readInt m
  pure
    Stats
      { artists = artists
      , albums = albums
      , songs = songs
      , uptime = uptime
      , playtime = playtime
      , dbPlaytime = dbPlaytime
      , dbUpdate = posixSecondsToUTCTime . fromIntegral <$> dbUpdate
      }

-- | The comments of a file, in the order of the reply.
parseComments :: [Field] -> Either T.Text [(T.Text, T.Text)]
parseComments = parseAll (\f -> Right (decode f.key, decode f.value))

parseOutputs :: [Field] -> Either T.Text [Output]
parseOutputs fields = parseAll parseOutput =<< splitOn (== "outputid") fields
  where
    parseOutput :: [Field] -> Either T.Text Output
    parseOutput entry = do
      let m = fieldMap entry
      outputId <- required "outputid" readInt m
      name <- required "outputname" Just m
      plugin <- required "plugin" Just m
      enabled <- required "outputenabled" readBool m
      pure
        Output
          { outputId = outputId
          , name = decode name
          , plugin = decode plugin
          , enabled = enabled
          , attributes =
              M.fromList
                [ (decode k, decode (BS.drop 1 v))
                | a <- M.findWithDefault [] "attribute" m
                , let (k, v) = BS8.break (== '=') a
                ]
          }

-- | Parse the reply to @idle@.
parseSubsystems :: [Field] -> Either T.Text [Subsystem]
parseSubsystems = parseAll $ \case
  Field "changed" v -> Right . subsystemFromName $ decode v
  f -> Left $ "unexpected key: " <> decode f.key

-- | Parse a value, and return it fully evaluated.
parseSingle :: NFData b => (a -> Either T.Text b) -> a -> Either T.Text b
parseSingle parse input = do
  value <- parse input
  pure $!! value

-- | Parse each entry, and return the list fully evaluated.
parseAll :: NFData b => (a -> Either T.Text b) -> [a] -> Either T.Text [b]
parseAll = parseSingle . traverse

parseSingleMode :: BS.ByteString -> Maybe SingleMode
parseSingleMode = byName singleModeName

parseConsumeMode :: BS.ByteString -> Maybe ConsumeMode
parseConsumeMode = byName consumeModeName

parseReplayGainMode :: BS.ByteString -> Maybe ReplayGainMode
parseReplayGainMode = byName replayGainModeName

-- | A value by its name in the protocol.
byName :: (Enum a, Bounded a) => (a -> T.Text) -> BS.ByteString -> Maybe a
byName name = (`lookup` [(T.encodeUtf8 (name a), a) | a <- [minBound .. maxBound]])
