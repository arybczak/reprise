-- | Typed MPD commands.
--
-- A 'Command' is a value: its request lines and a parser for its reply. Run it
-- with 'MPD.Connection.run'. Commands combine with the 'Applicative'
-- interface, and the combination runs as a single command list, e.g.
--
-- @
-- (,) '<$>' 'status' '<*>' 'currentSong'
-- @
--
-- 'Command' is not a 'Monad'. A command that needs the result of another one
-- needs a second round trip.
module MPD.Command
  ( -- * Commands
    Command
  , command
  , commandRequests
  , parseCommandReply

    -- * Arguments
  , Position (..)
  , Range (..)
  , onePosition
  , SeekTarget (..)

    -- * Status
  , status
  , currentSong
  , stats

    -- * Queue
  , playlistInfo
  , plChanges
  , add
  , addId
  , delete
  , deleteId
  , move
  , moveId
  , shuffle
  , clear
  , prio
  , prioId

    -- * Playback
  , play
  , playId
  , pause
  , stop
  , next
  , previous
  , seekCur

    -- * Volume
  , setVolume
  , changeVolume

    -- * Options
  , setRepeat
  , setRandom
  , setSingle
  , setConsume
  , setCrossfade
  , setReplayGainMode
  , replayGainStatus

    -- * Database
  , update

    -- * Outputs
  , outputs
  , enableOutput
  , disableOutput

    -- * Connection
  , password
  , ping
  ) where

import Data.Text (Text)
import Data.Text qualified as T

import MPD.Protocol.Request
import MPD.Protocol.Response
import MPD.Types

----------------------------------------
-- Commands

-- | A command, or a command list, with a result of type @a@.
--
-- @since 0.1.0.0
data Command a = Command [Request] ([[Field]] -> Either Text (a, [[Field]]))

-- | @since 0.1.0.0
instance Functor Command where
  fmap f (Command requests parse) = Command requests $ \parts -> do
    (a, rest) <- parse parts
    pure (f a, rest)

-- | 'pure' sends nothing, '<*>' joins the requests into one command list.
--
-- @since 0.1.0.0
instance Applicative Command where
  pure a = Command [] $ \parts -> Right (a, parts)
  Command requests1 parse1 <*> Command requests2 parse2 =
    Command (requests1 ++ requests2) $ \parts -> do
      (f, rest1) <- parse1 parts
      (a, rest2) <- parse2 rest1
      pure (f a, rest2)

-- | A command with a parser for its part of the reply. Use it for a command
-- that this module doesn't provide.
--
-- @since 0.1.0.0
command
  :: Text
  -- ^ The name.
  -> [Text]
  -- ^ The arguments, unquoted.
  -> ([Field] -> Either Text a)
  -- ^ The parser.
  -> Command a
command name args parse = Command [Request name args] $ \case
  part : rest -> case parse part of
    Right a -> Right (a, rest)
    Left err -> Left $ name <> ": " <> err
  [] -> Left $ name <> ": the reply has no part for this command"

-- | The requests of a command, in the order they are sent.
--
-- @since 0.1.0.0
commandRequests :: Command a -> [Request]
commandRequests (Command requests _) = requests

-- | Parse the reply to a command from the parts that 'parseReply' returns.
--
-- @since 0.1.0.0
parseCommandReply :: Command a -> [[Field]] -> Either MpdError a
parseCommandReply (Command requests parse) parts = case requests of
  [] -> run []
  [_] -> run parts
  _ -> case reverse parts of
    [] : listParts -> run (reverse listParts)
    _ -> Left $ ProtocolError "a command list reply doesn't end with list_OK"
  where
    run ps = case parse ps of
      Right (a, []) -> Right a
      Right (_, _ : _) -> Left $ ProtocolError "the reply has more parts than commands"
      Left err -> Left $ ProtocolError err

----------------------------------------
-- Arguments

-- | A position in the queue to add or move songs to. A relative position
-- needs a current song.
--
-- @since 0.1.0.0
data Position
  = At SongPos
  | -- | @AfterCurrent 0@ is right after the current song.
    AfterCurrent Int
  | -- | @BeforeCurrent 0@ is right before the current song.
    BeforeCurrent Int
  deriving stock (Eq, Show)

-- | @since 0.1.0.0
instance Argument Position where
  toArgument = \case
    At p -> toArgument p
    AfterCurrent n -> "+" <> toArgument n
    BeforeCurrent n -> "-" <> toArgument n

-- | A range of positions in the queue, from 'start' up to, but not including,
-- 'end'. Without an end, the range reaches the end of the queue.
--
-- @since 0.1.0.0
data Range = Range
  { start :: SongPos
  , end :: Maybe SongPos
  }
  deriving stock (Eq, Show)

-- | @since 0.1.0.0
instance Argument Range where
  toArgument r = toArgument r.start <> ":" <> maybe "" toArgument r.end

-- | The range of one position.
--
-- @since 0.1.0.0
onePosition :: SongPos -> Range
onePosition p = Range p (Just (p + 1))

-- | @since 0.1.0.0
data SeekTarget
  = SeekTo Seconds
  | SeekForward Seconds
  | SeekBackward Seconds
  deriving stock (Eq, Show)

-- | @since 0.1.0.0
instance Argument SeekTarget where
  toArgument = \case
    SeekTo s -> toArgument s
    SeekForward s -> "+" <> toArgument s
    SeekBackward s -> "-" <> toArgument s

----------------------------------------
-- Status

-- | @since 0.1.0.0
status :: Command Status
status = command "status" [] parseStatus

-- | The current song, if there is one.
--
-- @since 0.1.0.0
currentSong :: Command (Maybe Song)
currentSong = command "currentsong" [] $ \case
  [] -> Right Nothing
  fields -> Just <$> parseSong fields

-- | @since 0.1.0.0
stats :: Command Stats
stats = command "stats" [] parseStats

----------------------------------------
-- Queue

-- | The whole queue.
--
-- @since 0.1.0.0
playlistInfo :: Command [Song]
playlistInfo = command "playlistinfo" [] parseSongs

-- | The songs of the queue that changed since a version of the queue. The
-- queue may also have become shorter; compare its length with
-- 'playlistLength'.
--
-- @since 0.1.0.0
plChanges :: PlaylistVersion -> Command [Song]
plChanges v = command "plchanges" [toArgument v] parseSongs

-- | Add a song or a directory, at the end or at a position.
--
-- @since 0.1.0.0
add :: Text -> Maybe Position -> Command ()
add uri pos = command "add" (uri : maybe [] (pure . toArgument) pos) noReply

-- | Add a song, at the end or at a position, and return its id.
--
-- @since 0.1.0.0
addId :: Text -> Maybe Position -> Command SongId
addId uri pos = command "addid" (uri : maybe [] (pure . toArgument) pos) $ \fields ->
  SongId <$> required "Id" readInt (fieldMap fields)

-- | @since 0.1.0.0
delete :: Range -> Command ()
delete r = command "delete" [toArgument r] noReply

-- | @since 0.1.0.0
deleteId :: SongId -> Command ()
deleteId i = command "deleteid" [toArgument i] noReply

-- | @since 0.1.0.0
move :: Range -> Position -> Command ()
move r to = command "move" [toArgument r, toArgument to] noReply

-- | @since 0.1.0.0
moveId :: SongId -> Position -> Command ()
moveId i to = command "moveid" [toArgument i, toArgument to] noReply

-- | Shuffle the queue, or a range of it.
--
-- @since 0.1.0.0
shuffle :: Maybe Range -> Command ()
shuffle r = command "shuffle" (maybe [] (pure . toArgument) r) noReply

-- | @since 0.1.0.0
clear :: Command ()
clear = command "clear" [] noReply

-- | Set the priority of ranges of songs, from 0 to 255. In random mode, MPD
-- plays songs with a higher priority first.
--
-- @since 0.1.0.0
prio :: Int -> [Range] -> Command ()
prio p rs = command "prio" (toArgument p : map toArgument rs) noReply

-- | Set the priority of songs, from 0 to 255.
--
-- @since 0.1.0.0
prioId :: Int -> [SongId] -> Command ()
prioId p is = command "prioid" (toArgument p : map toArgument is) noReply

----------------------------------------
-- Playback

-- | Play the song at a position, or resume playback.
--
-- @since 0.1.0.0
play :: Maybe SongPos -> Command ()
play p = command "play" (maybe [] (pure . toArgument) p) noReply

-- | @since 0.1.0.0
playId :: SongId -> Command ()
playId i = command "playid" [toArgument i] noReply

-- | Pause ('True') or resume ('False').
--
-- @since 0.1.0.0
pause :: Bool -> Command ()
pause b = command "pause" [toArgument b] noReply

-- | @since 0.1.0.0
stop :: Command ()
stop = command "stop" [] noReply

-- | @since 0.1.0.0
next :: Command ()
next = command "next" [] noReply

-- | @since 0.1.0.0
previous :: Command ()
previous = command "previous" [] noReply

-- | Seek within the current song.
--
-- @since 0.1.0.0
seekCur :: SeekTarget -> Command ()
seekCur t = command "seekcur" [toArgument t] noReply

----------------------------------------
-- Volume

-- | Set the volume, from 0 to 100.
--
-- @since 0.1.0.0
setVolume :: Int -> Command ()
setVolume v = command "setvol" [toArgument v] noReply

-- | Change the volume by a number of percentage points. MPD keeps the result
-- between 0 and 100.
--
-- @since 0.1.0.0
changeVolume :: Int -> Command ()
changeVolume d = command "volume" [T.pack (if d >= 0 then '+' : show d else show d)] noReply

----------------------------------------
-- Options

-- | @since 0.1.0.0
setRepeat :: Bool -> Command ()
setRepeat b = command "repeat" [toArgument b] noReply

-- | @since 0.1.0.0
setRandom :: Bool -> Command ()
setRandom b = command "random" [toArgument b] noReply

-- | @since 0.1.0.0
setSingle :: SingleMode -> Command ()
setSingle m = command "single" [toArgument m] noReply

-- | @since 0.1.0.0
setConsume :: ConsumeMode -> Command ()
setConsume m = command "consume" [toArgument m] noReply

-- | Set the crossfade, in seconds.
--
-- @since 0.1.0.0
setCrossfade :: Int -> Command ()
setCrossfade s = command "crossfade" [toArgument s] noReply

-- | @since 0.1.0.0
setReplayGainMode :: ReplayGainMode -> Command ()
setReplayGainMode m = command "replay_gain_mode" [toArgument m] noReply

-- | @since 0.1.0.0
replayGainStatus :: Command ReplayGainMode
replayGainStatus = command "replay_gain_status" [] $ \fields ->
  required "replay_gain_mode" parseReplayGainMode (fieldMap fields)

----------------------------------------
-- Database

-- | Start a database update, of everything or of a directory or a file.
-- Returns the job id, which 'updatingDb' shows while the update runs.
--
-- @since 0.1.0.0
update :: Maybe Text -> Command Int
update uri = command "update" (maybe [] pure uri) $ \fields ->
  required "updating_db" readInt (fieldMap fields)

----------------------------------------
-- Outputs

-- | @since 0.1.0.0
outputs :: Command [Output]
outputs = command "outputs" [] parseOutputs

-- | @since 0.1.0.0
enableOutput :: Int -> Command ()
enableOutput i = command "enableoutput" [toArgument i] noReply

-- | @since 0.1.0.0
disableOutput :: Int -> Command ()
disableOutput i = command "disableoutput" [toArgument i] noReply

----------------------------------------
-- Connection

-- | Authenticate. Without the password, MPD may refuse some commands with
-- 'AckPermission'.
--
-- @since 0.1.0.0
password :: Text -> Command ()
password p = command "password" [p] noReply

-- | @since 0.1.0.0
ping :: Command ()
ping = command "ping" [] noReply

----------------------------------------
-- Helpers

noReply :: [Field] -> Either Text ()
noReply = \case
  [] -> Right ()
  f : _ -> Left $ "unexpected key: " <> decode f.key
