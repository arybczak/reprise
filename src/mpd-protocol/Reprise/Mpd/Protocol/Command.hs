-- | Typed MPD commands.
--
-- A 'Command' is a value: its request lines and a parser for its reply. Run it
-- with 'Reprise.Mpd.Protocol.Connection.run'. Commands combine with the 'Applicative'
-- interface, and the combination runs as a single command list, e.g.
--
-- @
-- (,) '<$>' 'status' '<*>' 'currentSong'
-- @
--
-- 'Command' is not a 'Monad'. A command that needs the result of another one
-- needs a second round trip.
module Reprise.Mpd.Protocol.Command
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
  , lsInfo
  , readComments

    -- * Playlists
  , listPlaylistInfo
  , listPlaylists
  , SaveMode (..)
  , save
  , playlistAdd
  , playlistAddDirectory
  , playlistClear
  , load

    -- * Outputs
  , outputs
  , enableOutput
  , disableOutput

    -- * Connection
  , password
  , ping
  ) where

import Data.Foldable
import Data.Maybe
import Data.Text qualified as T

import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Response
import Reprise.Mpd.Protocol.Types

----------------------------------------
-- Commands

-- | A command, or a command list, with a result of type @a@.
data Command a = Command [Request] ([[Field]] -> Either T.Text (a, [[Field]]))

instance Functor Command where
  fmap f (Command requests parse) = Command requests $ \parts -> do
    (a, rest) <- parse parts
    pure (f a, rest)

-- | 'pure' sends nothing, '<*>' joins the requests into one command list.
instance Applicative Command where
  pure a = Command [] $ \parts -> Right (a, parts)
  Command requests1 parse1 <*> Command requests2 parse2 =
    Command (requests1 ++ requests2) $ \parts -> do
      (f, rest1) <- parse1 parts
      (a, rest2) <- parse2 rest1
      pure (f a, rest2)

-- | A command with a parser for its part of the reply. Use it for a command
-- that this module doesn't provide.
command
  :: T.Text
  -- ^ The name.
  -> [T.Text]
  -- ^ The arguments, unquoted.
  -> ([Field] -> Either T.Text a)
  -- ^ The parser.
  -> Command a
command name args parse = Command [Request name args] $ \case
  part : rest -> case parse part of
    Right a -> Right (a, rest)
    Left err -> Left $ name <> ": " <> err
  [] -> Left $ name <> ": the reply has no part for this command"

-- | The requests of a command, in the order they are sent.
commandRequests :: Command a -> [Request]
commandRequests (Command requests _) = requests

-- | Parse the reply to a command from the parts that 'parseReply' returns.
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
data Position
  = At SongPos
  | -- | @AfterCurrent 0@ is right after the current song.
    AfterCurrent Int
  | -- | @BeforeCurrent 0@ is right before the current song.
    BeforeCurrent Int
  deriving stock (Eq, Show)

instance Argument Position where
  toArgument = \case
    At p -> toArgument p
    AfterCurrent n -> "+" <> toArgument n
    BeforeCurrent n -> "-" <> toArgument n

-- | A range of positions in the queue or in a playlist, from 'start' up to,
-- but not including, 'end'. Without an end, the range reaches the end.
data Range = Range
  { start :: SongPos
  , end :: Maybe SongPos
  }
  deriving stock (Eq, Show)

instance Argument Range where
  toArgument r = toArgument r.start <> ":" <> maybe "" toArgument r.end

-- | The range of one position.
onePosition :: SongPos -> Range
onePosition p = Range p (Just (p + 1))

data SeekTarget
  = SeekTo Seconds
  | SeekForward Seconds
  | SeekBackward Seconds
  deriving stock (Eq, Show)

instance Argument SeekTarget where
  toArgument = \case
    SeekTo s -> toArgument s
    SeekForward s -> "+" <> toArgument s
    SeekBackward s -> "-" <> toArgument s

----------------------------------------
-- Status

status :: Command Status
status = command "status" [] parseStatus

-- | The current song, if there is one.
currentSong :: Command (Maybe Song)
currentSong = command "currentsong" [] $ \case
  [] -> Right Nothing
  fields -> Just <$> parseSong fields

stats :: Command Stats
stats = command "stats" [] parseStats

----------------------------------------
-- Queue

-- | The whole queue.
playlistInfo :: Command [Song]
playlistInfo = command "playlistinfo" [] parseSongs

-- | The songs of the queue that changed since a version of the queue. The
-- queue may also have become shorter; compare its length with
-- 'playlistLength'.
plChanges :: PlaylistVersion -> Command [Song]
plChanges v = command "plchanges" [toArgument v] parseSongs

-- | Add a song or a directory, at the end or at a position.
add :: T.Text -> Maybe Position -> Command ()
add uri pos = command "add" (uri : optionalArgument pos) noReply

-- | Add a song, at the end or at a position, and return its id.
addId :: T.Text -> Maybe Position -> Command SongId
addId uri pos = command "addid" (uri : optionalArgument pos) $ \fields ->
  SongId <$> required "Id" readInt (fieldMap fields)

delete :: Range -> Command ()
delete r = command "delete" [toArgument r] noReply

deleteId :: SongId -> Command ()
deleteId i = command "deleteid" [toArgument i] noReply

move :: Range -> Position -> Command ()
move r to = command "move" [toArgument r, toArgument to] noReply

moveId :: SongId -> Position -> Command ()
moveId i to = command "moveid" [toArgument i, toArgument to] noReply

-- | Shuffle the queue, or a range of it.
shuffle :: Maybe Range -> Command ()
shuffle r = command "shuffle" (optionalArgument r) noReply

clear :: Command ()
clear = command "clear" [] noReply

-- | Set the priority of ranges of songs, from 0 to 255. In random mode, MPD
-- plays songs with a higher priority first.
prio :: Int -> [Range] -> Command ()
prio p = withPriority "prio" p . map toArgument

-- | Set the priority of songs, from 0 to 255.
prioId :: Int -> [SongId] -> Command ()
prioId p = withPriority "prioid" p . map toArgument

-- | A command of a priority and any number of songs, as many requests as
-- MPD's limit of arguments needs.
withPriority :: T.Text -> Int -> [T.Text] -> Command ()
withPriority name p = traverse_ (\songs -> command name (toArgument p : songs) noReply) . batches
  where
    batches :: [T.Text] -> [[T.Text]]
    batches = \case
      [] -> []
      songs -> let (batch, rest) = splitAt (maxArguments - 1) songs in batch : batches rest

----------------------------------------
-- Playback

-- | Play the song at a position, or resume playback.
play :: Maybe SongPos -> Command ()
play p = command "play" (optionalArgument p) noReply

playId :: SongId -> Command ()
playId i = command "playid" [toArgument i] noReply

-- | Pause ('True') or resume ('False').
pause :: Bool -> Command ()
pause b = command "pause" [toArgument b] noReply

stop :: Command ()
stop = command "stop" [] noReply

next :: Command ()
next = command "next" [] noReply

previous :: Command ()
previous = command "previous" [] noReply

-- | Seek within the current song.
seekCur :: SeekTarget -> Command ()
seekCur t = command "seekcur" [toArgument t] noReply

----------------------------------------
-- Volume

-- | Set the volume, from 0 to 100.
setVolume :: Int -> Command ()
setVolume v = command "setvol" [toArgument v] noReply

-- | Change the volume by a number of percentage points. MPD keeps the result
-- between 0 and 100.
changeVolume :: Int -> Command ()
changeVolume d = command "volume" [T.pack (if d >= 0 then '+' : show d else show d)] noReply

----------------------------------------
-- Options

setRepeat :: Bool -> Command ()
setRepeat b = command "repeat" [toArgument b] noReply

setRandom :: Bool -> Command ()
setRandom b = command "random" [toArgument b] noReply

setSingle :: SingleMode -> Command ()
setSingle m = command "single" [toArgument m] noReply

setConsume :: ConsumeMode -> Command ()
setConsume m = command "consume" [toArgument m] noReply

-- | Set the crossfade, in seconds.
setCrossfade :: Int -> Command ()
setCrossfade s = command "crossfade" [toArgument s] noReply

setReplayGainMode :: ReplayGainMode -> Command ()
setReplayGainMode m = command "replay_gain_mode" [toArgument m] noReply

replayGainStatus :: Command ReplayGainMode
replayGainStatus = command "replay_gain_status" [] $ \fields ->
  required "replay_gain_mode" parseReplayGainMode (fieldMap fields)

----------------------------------------
-- Database

-- | Start a database update, of everything or of a directory or a file.
-- Returns the job id, which 'updatingDb' shows while the update runs.
update :: Maybe T.Text -> Command Int
update uri = command "update" (optionalArgument uri) $ \fields ->
  required "updating_db" readInt (fieldMap fields)

-- | The entries of a directory, @""@ for the root. MPD also lists the stored
-- playlists at the root, which it has deprecated.
lsInfo :: T.Text -> Command [Entry]
lsInfo path = command "lsinfo" [path | not (T.null path)] parseEntries

-- | The comments of a song's file, as the file has them, e.g. ReplayGain's.
-- MPD reads them for some formats only, e.g. FLAC and Ogg, not MP3. It
-- leaves out the values of more than one line.
readComments :: T.Text -> Command [(T.Text, T.Text)]
readComments uri = command "readcomments" [uri] parseComments

----------------------------------------
-- Playlists

-- | The songs of a stored playlist, or of a playlist file in the music
-- directory.
listPlaylistInfo :: T.Text -> Command [Song]
listPlaylistInfo name = command "listplaylistinfo" [name] parseSongs

-- | The names of the stored playlists.
listPlaylists :: Command [T.Text]
listPlaylists = command "listplaylists" [] $ \fields ->
  Right [decode f.value | f <- fields, f.key == "playlist"]

-- | What @save@ does with a stored playlist of its name.
data SaveMode
  = -- | Make a new one, which fails if it exists.
    CreatePlaylist
  | ReplacePlaylist
  | AppendToPlaylist
  deriving stock (Eq, Show)

instance Argument SaveMode where
  toArgument = \case
    CreatePlaylist -> "create"
    ReplacePlaylist -> "replace"
    AppendToPlaylist -> "append"

-- | Save the queue as a stored playlist.
save :: T.Text -> SaveMode -> Command ()
save name mode = command "save" [name, toArgument mode] noReply

-- | Add a song to a stored playlist, which it makes if it doesn't exist.
playlistAdd :: T.Text -> T.Text -> Command ()
playlistAdd name uri = command "playlistadd" [name, uri] noReply

-- | Add the songs of a directory of the database, with those of the
-- directories in it, to a stored playlist, which it makes if it doesn't
-- exist.
playlistAddDirectory :: T.Text -> T.Text -> Command ()
playlistAddDirectory name path =
  command "searchaddpl" [name, "(base " <> quoteText path <> ")"] noReply

-- | Remove the songs of a stored playlist.
playlistClear :: T.Text -> Command ()
playlistClear name = command "playlistclear" [name] noReply

-- | Add a playlist, or a range of its songs, to the queue, at the end or at
-- a position.
load :: T.Text -> Maybe Range -> Maybe Position -> Command ()
load name range pos = command "load" (name : arguments) noReply
  where
    -- MPD takes a position only after a range.
    arguments :: [T.Text]
    arguments = case pos of
      Nothing -> optionalArgument range
      Just p -> [toArgument (fromMaybe (Range 0 Nothing) range), toArgument p]

----------------------------------------
-- Outputs

outputs :: Command [Output]
outputs = command "outputs" [] parseOutputs

enableOutput :: Int -> Command ()
enableOutput i = command "enableoutput" [toArgument i] noReply

disableOutput :: Int -> Command ()
disableOutput i = command "disableoutput" [toArgument i] noReply

----------------------------------------
-- Connection

-- | Authenticate. Without the password, MPD may refuse some commands with
-- 'AckPermission'.
password :: T.Text -> Command ()
password p = command "password" [p] noReply

ping :: Command ()
ping = command "ping" [] noReply

----------------------------------------
-- Helpers

noReply :: [Field] -> Either T.Text ()
noReply = \case
  [] -> Right ()
  f : _ -> Left $ "unexpected key: " <> decode f.key

-- | The argument of a command that can go without it.
optionalArgument :: Argument a => Maybe a -> [T.Text]
optionalArgument = maybe [] (pure . toArgument)

-- | The most arguments that MPD reads after a command's name, else it
-- answers "Too many arguments". It is @COMMAND_ARGV_MAX@ of MPD's
-- @src/command/AllCommands.cxx@, @2 + TAG_NUM_OF_ITEM_TYPES * 2@, with the
-- 35 tags of 'minimumVersion'. A newer MPD with more tags reads more.
maxArguments :: Int
maxArguments = 72
