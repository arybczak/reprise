-- | Parsers for MPD's replies. They are pure, so they work on replies from
-- any source.
module MPD.Protocol.Response
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
  , readTime

    -- * Replies of commands
  , parseSong
  , parseSongs
  , parseStatus
  , parseStats
  , parseOutputs
  , parseSubsystems
  , parseSingleMode
  , parseConsumeMode
  , parseReplayGainMode
  ) where

import Control.Applicative hiding (optional)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Fixed
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Word

import MPD.Types

----------------------------------------
-- Replies

-- | A @key: value@ line of a reply.
--
-- @since 0.1.0.0
data Field = Field
  { key :: ByteString
  , value :: ByteString
  }
  deriving stock (Eq, Show)

-- | Whether a line (without its newline) ends a reply.
--
-- @since 0.1.0.0
isFinalLine :: ByteString -> Bool
isFinalLine l = l == "OK" || "ACK " `BS.isPrefixOf` l

-- | Parse the lines of one reply, without their newlines. The last line must
-- be @OK@ or an @ACK@.
--
-- The result has the fields of each part of the reply, split at @list_OK@.
-- The part after the last @list_OK@ is included, so the reply to a command
-- list of @n@ commands has @n + 1@ parts, the last one empty.
--
-- @since 0.1.0.0
parseReply :: [ByteString] -> Either MpdError [[Field]]
parseReply = go [] []
  where
    go :: [[Field]] -> [Field] -> [ByteString] -> Either MpdError [[Field]]
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
        | otherwise -> case BS.breakSubstring ": " l of
            (k, v)
              | not (BS.null k) && not (BS.null v) -> go parts (Field k (BS.drop 2 v) : fields) ls
              | otherwise -> Left . ProtocolError $ "malformed line: " <> decode l

-- | Parse an @ACK [code\@index] {command} message@ line.
--
-- @since 0.1.0.0
parseAck :: ByteString -> Maybe Ack
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
--
-- @since 0.1.0.0
type FieldMap = M.Map ByteString [ByteString]

-- | @since 0.1.0.0
fieldMap :: [Field] -> FieldMap
fieldMap fields = M.fromListWith (flip (++)) [(f.key, [f.value]) | f <- fields]

-- | The first value of a key that must be present.
--
-- @since 0.1.0.0
required :: ByteString -> (ByteString -> Maybe a) -> FieldMap -> Either Text a
required k parse m = case M.lookup k m of
  Just (v : _) -> parseValue k parse v
  _ -> Left $ "missing key: " <> decode k

-- | The first value of a key that may be absent.
--
-- @since 0.1.0.0
optional :: ByteString -> (ByteString -> Maybe a) -> FieldMap -> Either Text (Maybe a)
optional k parse m = case M.lookup k m of
  Just (v : _) -> Just <$> parseValue k parse v
  _ -> Right Nothing

parseValue :: ByteString -> (ByteString -> Maybe a) -> ByteString -> Either Text a
parseValue k parse v = case parse v of
  Just a -> Right a
  Nothing -> Left $ "bad value of " <> decode k <> ": " <> decode v

-- | Split fields into entries, each starting at a key that the predicate
-- accepts, e.g. @file@ for songs.
--
-- @since 0.1.0.0
splitOn :: (ByteString -> Bool) -> [Field] -> Either Text [[Field]]
splitOn isStart = \case
  [] -> Right []
  f : fs
    | isStart f.key ->
        let (entry, rest) = break (\g -> isStart g.key) fs
        in ((f : entry) :) <$> splitOn isStart rest
    | otherwise -> Left $ "unexpected key: " <> decode f.key

-- | Decode a value as UTF-8. Invalid bytes become U+FFFD, so a broken tag
-- doesn't make the whole reply fail.
--
-- @since 0.1.0.0
decode :: ByteString -> Text
decode = T.decodeUtf8Lenient

----------------------------------------
-- Values

-- | @since 0.1.0.0
readInt :: ByteString -> Maybe Int
readInt s = case BS8.readInt s of
  Just (n, rest) | BS.null rest -> Just n
  _ -> Nothing

-- | @since 0.1.0.0
readBool :: ByteString -> Maybe Bool
readBool = \case
  "0" -> Just False
  "1" -> Just True
  _ -> Nothing

-- | A non-negative decimal number. Digits past the precision of 'Seconds'
-- are dropped.
--
-- @since 0.1.0.0
readSeconds :: ByteString -> Maybe Seconds
readSeconds s = case BS8.break (== '.') s of
  (whole, frac) -> do
    w <- readDigits whole
    f <- case BS.uncons frac of
      Nothing -> Just 0
      Just (_, digits)
        | BS.null digits -> Nothing
        | otherwise -> readDigits (BS.take precision (digits <> BS8.replicate precision '0'))
    pure . Seconds . MkFixed $ w * scale + f
  where
    scale :: Integer
    scale = resolution (Proxy @E3)

    precision :: Int
    precision = length . takeWhile (> 1) $ iterate (`div` 10) scale

    readDigits :: ByteString -> Maybe Integer
    readDigits d
      | not (BS.null d) && BS8.all (\c -> c >= '0' && c <= '9') d = fst <$> BS8.readInteger d
      | otherwise = Nothing

-- | An ISO 8601 time, e.g. @2024-01-02T03:04:05Z@.
--
-- @since 0.1.0.0
readTime :: ByteString -> Maybe UTCTime
readTime = iso8601ParseM . T.unpack . decode

----------------------------------------
-- Replies of commands

-- | Parse one song. The first field must be @file@.
--
-- @since 0.1.0.0
parseSong :: [Field] -> Either Text Song
parseSong = \case
  Field "file" file : rest -> do
    let m = fieldMap rest
    duration <- optional "duration" readSeconds m
    time <- optional "Time" readInt m
    lastModified <- optional "Last-Modified" readTime m
    position <- optional "Pos" readInt m
    songId <- optional "Id" readInt m
    priority <- optional "Prio" readInt m
    pure
      Song
        { file = decode file
        , tags =
            M.fromList [(t, map decode vs) | (k, vs) <- M.toList m, Just t <- [M.lookup k tagsByKey]]
        , duration = maybe (fromIntegral <$> time) Just duration
        , lastModified = lastModified
        , format = decode <$> lookupFirst "Format" m
        , position = SongPos <$> position
        , songId = SongId <$> songId
        , priority = fromMaybe 0 priority
        }
  _ -> Left "a song doesn't start with the file key"
  where
    tagsByKey :: M.Map ByteString Tag
    tagsByKey = M.fromList [(T.encodeUtf8 (tagName t), t) | t <- [minBound .. maxBound]]

    lookupFirst :: ByteString -> FieldMap -> Maybe ByteString
    lookupFirst k m = case M.lookup k m of
      Just (v : _) -> Just v
      _ -> Nothing

-- | Parse a list of songs, e.g. the reply to @playlistinfo@.
--
-- @since 0.1.0.0
parseSongs :: [Field] -> Either Text [Song]
parseSongs fields = traverse parseSong =<< splitOn (== "file") fields

-- | @since 0.1.0.0
parseStatus :: [Field] -> Either Text Status
parseStatus fields = do
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
      { volume = case volume of
          -- MPD before 0.24 sends -1 without a mixer.
          Just v | v >= 0 -> Just v
          _ -> Nothing
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
    readState :: ByteString -> Maybe PlayerState
    readState = \case
      "play" -> Just Playing
      "pause" -> Just Paused
      "stop" -> Just Stopped
      _ -> Nothing

    readVersion :: ByteString -> Maybe PlaylistVersion
    readVersion s = case BS8.readInteger s of
      Just (n, rest)
        | BS.null rest && n >= 0 && n <= toInteger (maxBound @Word32) ->
            Just . PlaylistVersion $ fromInteger n
      _ -> Nothing

-- | @since 0.1.0.0
parseStats :: [Field] -> Either Text Stats
parseStats fields = do
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

-- | @since 0.1.0.0
parseOutputs :: [Field] -> Either Text [Output]
parseOutputs fields = traverse parseOutput =<< splitOn (== "outputid") fields
  where
    parseOutput :: [Field] -> Either Text Output
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
--
-- @since 0.1.0.0
parseSubsystems :: [Field] -> Either Text [Subsystem]
parseSubsystems = traverse $ \case
  Field "changed" v -> Right . subsystemFromName $ decode v
  f -> Left $ "unexpected key: " <> decode f.key

-- | @since 0.1.0.0
parseSingleMode :: ByteString -> Maybe SingleMode
parseSingleMode = \case
  "0" -> Just SingleOff
  "1" -> Just SingleOn
  "oneshot" -> Just SingleOneshot
  _ -> Nothing

-- | @since 0.1.0.0
parseConsumeMode :: ByteString -> Maybe ConsumeMode
parseConsumeMode = \case
  "0" -> Just ConsumeOff
  "1" -> Just ConsumeOn
  "oneshot" -> Just ConsumeOneshot
  _ -> Nothing

-- | @since 0.1.0.0
parseReplayGainMode :: ByteString -> Maybe ReplayGainMode
parseReplayGainMode = \case
  "off" -> Just ReplayGainOff
  "track" -> Just ReplayGainTrack
  "album" -> Just ReplayGainAlbum
  "auto" -> Just ReplayGainAuto
  _ -> Nothing
