-- | Data types of the MPD protocol.
module Reprise.Mpd.Protocol.Types
  ( -- * Songs
    Song (..)
  , SongId (..)
  , SongPos (..)
  , SongRange (..)
  , maxPriority
  , Seconds (..)
  , songKey
  , sameSong
  , isStream

    -- ** Tags
  , Tag (..)
  , tagName
  , tagFromName
  , firstTag

    -- * Paths
  , baseName
  , directoryOf

    -- * Database
  , Entry (..)
  , Directory (..)
  , Playlist (..)

    -- * Status
  , Status (..)
  , maxVolume
  , PlayerState (..)
  , SingleMode (..)
  , singleModeName
  , ConsumeMode (..)
  , consumeModeName
  , ReplayGainMode (..)
  , replayGainModeName
  , PlaylistVersion (..)

    -- * Statistics
  , Stats (..)

    -- * Outputs
  , Output (..)

    -- * Idle
  , Subsystem (..)
  , subsystemName
  , subsystemFromName

    -- * Server version
  , Version (..)
  , minimumVersion

    -- * Errors
  , MpdError (..)
  , Ack (..)
  , AckCode (..)
  , ConnectionError (..)
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Fixed
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Word
import GHC.Generics

----------------------------------------
-- Songs

-- | A song as MPD describes it, e.g. in the queue or the database.
data Song = Song
  { file :: T.Text
  -- ^ The URI of the song, relative to the music directory for local files.
  , tags :: M.Map Tag [T.Text]
  -- ^ The values of each tag, in the order MPD sent them.
  , duration :: Maybe Seconds
  , range :: Maybe SongRange
  -- ^ The part of the file that the song is, e.g. a track of a cue sheet.
  -- Two such songs have the same file.
  , lastModified :: Maybe Time.UTCTime
  , format :: Maybe T.Text
  -- ^ The audio format, e.g. @44100:16:2@.
  , position :: Maybe SongPos
  -- ^ The position in the queue, for a song in the queue.
  , songId :: Maybe SongId
  -- ^ The id in the queue, for a song in the queue.
  , priority :: Int
  -- ^ The priority in the queue, from 0, unless set, to 'maxPriority'.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The highest priority of a song in the queue.
maxPriority :: Int
maxPriority = 255

-- | A part of a file, from a point up to another one or to the end.
data SongRange = SongRange
  { start :: Seconds
  , end :: Maybe Seconds
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The id of a song in the queue. It doesn't change when the song moves.
newtype SongId = SongId Int
  deriving newtype (Eq, Ord, Show, NFData)

-- | The position of a song in the queue, from 0.
newtype SongPos = SongPos Int
  deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral, NFData)

-- | A duration or a point in time within a song. MPD sends them with a
-- precision of milliseconds.
newtype Seconds = Seconds Milli
  deriving newtype (Eq, Ord, Show, Num, Real, Fractional, RealFrac, NFData)

-- | What tells songs apart: the file, and the part of it.
songKey :: Song -> (T.Text, Maybe SongRange)
songKey song = (song.file, song.range)

-- | Whether two songs are the same file, or the same part of a file, e.g.
-- in the queue and in the database.
sameSong :: Song -> Song -> Bool
sameSong a b = songKey a == songKey b

-- | Whether a song is a stream, whose file is a URL. It isn't in the
-- database.
isStream :: Song -> Bool
isStream song = "://" `T.isInfixOf` song.file

----------------------------------------
-- Tags

-- | The tags MPD knows. MPD doesn't send a tag that its configuration
-- disables.
data Tag
  = Artist
  | ArtistSort
  | Album
  | AlbumSort
  | AlbumArtist
  | AlbumArtistSort
  | Title
  | TitleSort
  | Track
  | Name
  | Genre
  | Mood
  | Date
  | OriginalDate
  | Composer
  | ComposerSort
  | Performer
  | Conductor
  | Work
  | Movement
  | MovementNumber
  | ShowMovement
  | Ensemble
  | Location
  | Grouping
  | Comment
  | Disc
  | Label
  | MusicBrainzArtistId
  | MusicBrainzAlbumId
  | MusicBrainzAlbumArtistId
  | MusicBrainzTrackId
  | MusicBrainzReleaseTrackId
  | MusicBrainzWorkId
  | MusicBrainzReleaseGroupId
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | The name of a tag in the protocol.
tagName :: Tag -> T.Text
tagName = \case
  Artist -> "Artist"
  ArtistSort -> "ArtistSort"
  Album -> "Album"
  AlbumSort -> "AlbumSort"
  AlbumArtist -> "AlbumArtist"
  AlbumArtistSort -> "AlbumArtistSort"
  Title -> "Title"
  TitleSort -> "TitleSort"
  Track -> "Track"
  Name -> "Name"
  Genre -> "Genre"
  Mood -> "Mood"
  Date -> "Date"
  OriginalDate -> "OriginalDate"
  Composer -> "Composer"
  ComposerSort -> "ComposerSort"
  Performer -> "Performer"
  Conductor -> "Conductor"
  Work -> "Work"
  Movement -> "Movement"
  MovementNumber -> "MovementNumber"
  ShowMovement -> "ShowMovement"
  Ensemble -> "Ensemble"
  Location -> "Location"
  Grouping -> "Grouping"
  Comment -> "Comment"
  Disc -> "Disc"
  Label -> "Label"
  MusicBrainzArtistId -> "MUSICBRAINZ_ARTISTID"
  MusicBrainzAlbumId -> "MUSICBRAINZ_ALBUMID"
  MusicBrainzAlbumArtistId -> "MUSICBRAINZ_ALBUMARTISTID"
  MusicBrainzTrackId -> "MUSICBRAINZ_TRACKID"
  MusicBrainzReleaseTrackId -> "MUSICBRAINZ_RELEASETRACKID"
  MusicBrainzWorkId -> "MUSICBRAINZ_WORKID"
  MusicBrainzReleaseGroupId -> "MUSICBRAINZ_RELEASEGROUPID"

-- | The tag with the given name. MPD compares tag names without regard to
-- case, and so does this function.
tagFromName :: T.Text -> Maybe Tag
tagFromName name = M.lookup (T.toCaseFold name) tagsByName
  where
    tagsByName :: M.Map T.Text Tag
    tagsByName = M.fromList [(T.toCaseFold (tagName t), t) | t <- [minBound .. maxBound]]

-- | The first value of a tag of a song, unless it is empty.
firstTag :: Tag -> Song -> Maybe T.Text
firstTag t song = mfilter (not . T.null) $ M.lookup t song.tags >>= listToMaybe

----------------------------------------
-- Paths

-- | The last part of a path in the database.
baseName :: T.Text -> T.Text
baseName = snd . T.breakOnEnd "/"

-- | The directory of a path in the database, @""@ for the root.
directoryOf :: T.Text -> T.Text
directoryOf = T.dropEnd 1 . fst . T.breakOnEnd "/"

----------------------------------------
-- Database

-- | An entry of a directory listing.
data Entry
  = DirectoryEntry Directory
  | SongEntry Song
  | PlaylistEntry Playlist
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data Directory = Directory
  { path :: T.Text
  -- ^ Relative to the music directory.
  , lastModified :: Maybe Time.UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | A stored playlist, or a playlist file in the music directory, e.g. a
-- cue sheet.
data Playlist = Playlist
  { path :: T.Text
  -- ^ The name of a stored playlist, or the path of a file relative to the
  -- music directory.
  , lastModified :: Maybe Time.UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

----------------------------------------
-- Status

-- | The reply to @status@.
data Status = Status
  { volume :: Maybe Int
  -- ^ From 0 to 'maxVolume', or 'Nothing' if MPD has no mixer.
  , repeat :: Bool
  , random :: Bool
  , single :: SingleMode
  , consume :: ConsumeMode
  , playlistVersion :: PlaylistVersion
  , playlistLength :: Int
  , state :: PlayerState
  , currentPosition :: Maybe SongPos
  , currentId :: Maybe SongId
  , nextPosition :: Maybe SongPos
  , nextId :: Maybe SongId
  , elapsed :: Maybe Seconds
  , duration :: Maybe Seconds
  , bitrate :: Maybe Int
  -- ^ In kbit/s.
  , crossfade :: Int
  -- ^ In seconds.
  , audio :: Maybe T.Text
  -- ^ The audio format, e.g. @44100:16:2@.
  , updatingDb :: Maybe Int
  -- ^ The job id of a running database update.
  , error :: Maybe T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | The loudest volume, in percent.
maxVolume :: Int
maxVolume = 100

data PlayerState = Playing | Paused | Stopped
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data SingleMode = SingleOff | SingleOn | SingleOneshot
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | The name of a single mode in the protocol.
singleModeName :: SingleMode -> T.Text
singleModeName = \case
  SingleOff -> "0"
  SingleOn -> "1"
  SingleOneshot -> "oneshot"

data ConsumeMode = ConsumeOff | ConsumeOn | ConsumeOneshot
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | The name of a consume mode in the protocol.
consumeModeName :: ConsumeMode -> T.Text
consumeModeName = \case
  ConsumeOff -> "0"
  ConsumeOn -> "1"
  ConsumeOneshot -> "oneshot"

data ReplayGainMode = ReplayGainOff | ReplayGainTrack | ReplayGainAlbum | ReplayGainAuto
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (NFData)

-- | The name of a ReplayGain mode in the protocol.
replayGainModeName :: ReplayGainMode -> T.Text
replayGainModeName = \case
  ReplayGainOff -> "off"
  ReplayGainTrack -> "track"
  ReplayGainAlbum -> "album"
  ReplayGainAuto -> "auto"

-- | The version of the queue. MPD increments it on every change of the queue.
newtype PlaylistVersion = PlaylistVersion Word32
  deriving newtype (Eq, Ord, Show, NFData)

----------------------------------------
-- Statistics

-- | The reply to @stats@.
data Stats = Stats
  { artists :: Int
  , albums :: Int
  , songs :: Int
  , uptime :: Int
  -- ^ In seconds.
  , playtime :: Int
  -- ^ In seconds.
  , dbPlaytime :: Int
  -- ^ The length of all songs in the database, in seconds.
  , dbUpdate :: Maybe Time.UTCTime
  -- ^ The time of the last database update.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

----------------------------------------
-- Outputs

-- | An audio output.
data Output = Output
  { outputId :: Int
  , name :: T.Text
  , plugin :: T.Text
  , enabled :: Bool
  , attributes :: M.Map T.Text T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

----------------------------------------
-- Idle

-- | A part of MPD that @idle@ reports changes of.
data Subsystem
  = DatabaseSubsystem
  | UpdateSubsystem
  | StoredPlaylistSubsystem
  | -- | The queue.
    PlaylistSubsystem
  | PlayerSubsystem
  | MixerSubsystem
  | OutputSubsystem
  | OptionsSubsystem
  | PartitionSubsystem
  | StickerSubsystem
  | SubscriptionSubsystem
  | MessageSubsystem
  | NeighborSubsystem
  | MountSubsystem
  | -- | A subsystem of a newer MPD.
    OtherSubsystem T.Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The name of a subsystem in the protocol.
subsystemName :: Subsystem -> T.Text
subsystemName = \case
  DatabaseSubsystem -> "database"
  UpdateSubsystem -> "update"
  StoredPlaylistSubsystem -> "stored_playlist"
  PlaylistSubsystem -> "playlist"
  PlayerSubsystem -> "player"
  MixerSubsystem -> "mixer"
  OutputSubsystem -> "output"
  OptionsSubsystem -> "options"
  PartitionSubsystem -> "partition"
  StickerSubsystem -> "sticker"
  SubscriptionSubsystem -> "subscription"
  MessageSubsystem -> "message"
  NeighborSubsystem -> "neighbor"
  MountSubsystem -> "mount"
  OtherSubsystem name -> name

-- | The subsystem with the given name.
subsystemFromName :: T.Text -> Subsystem
subsystemFromName name = M.findWithDefault (OtherSubsystem name) name subsystemsByName
  where
    subsystemsByName :: M.Map T.Text Subsystem
    subsystemsByName =
      M.fromList
        [ (subsystemName s, s)
        | s <-
            [ DatabaseSubsystem
            , UpdateSubsystem
            , StoredPlaylistSubsystem
            , PlaylistSubsystem
            , PlayerSubsystem
            , MixerSubsystem
            , OutputSubsystem
            , OptionsSubsystem
            , PartitionSubsystem
            , StickerSubsystem
            , SubscriptionSubsystem
            , MessageSubsystem
            , NeighborSubsystem
            , MountSubsystem
            ]
        ]

----------------------------------------
-- Server version

-- | The protocol version that MPD sends when a client connects.
data Version = Version Int Int Int
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

-- | The oldest version of MPD that the library supports. It needs the
-- relative positions of 0.23, @load@ with a position from 0.23.1, and the
-- modes of @save@ from 0.24.
minimumVersion :: Version
minimumVersion = Version 0 24 0

----------------------------------------
-- Errors

-- | The exception that the operations of a connection throw.
data MpdError
  = -- | MPD refused a command.
    AckError Ack
  | -- | The reply wasn't what the command expects.
    ProtocolError T.Text
  | -- | The connection doesn't work. Close it and connect again.
    ConnectionError ConnectionError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

-- | 'displayException' describes the error for people, e.g. in a status
-- bar or a log.
instance Exception MpdError where
  displayException =
    T.unpack . \case
      AckError ack -> ack.command <> ": " <> ack.message
      ProtocolError err -> "Protocol error: " <> err
      ConnectionError err -> case err of
        ConnectFailed reason -> "Can't connect to MPD: " <> reason
        UnsupportedVersion v ->
          "MPD "
            <> showVersion v
            <> " is too old, "
            <> showVersion minimumVersion
            <> " or newer is needed"
        Closed -> "MPD closed the connection"
        Broken reason -> "The connection to MPD broke: " <> reason
        TimedOut -> "MPD didn't reply in time"
    where
      showVersion :: Version -> T.Text
      showVersion (Version a b c) = T.intercalate "." (map (T.pack . show) [a, b, c])

-- | An @ACK@ reply.
data Ack = Ack
  { code :: AckCode
  , index :: Int
  -- ^ The position of the failed command in a command list, 0 otherwise.
  , command :: T.Text
  -- ^ The name of the failed command.
  , message :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data AckCode
  = AckNotList
  | AckArg
  | AckPassword
  | AckPermission
  | AckUnknown
  | AckNoExist
  | AckPlaylistMax
  | AckSystem
  | AckPlaylistLoad
  | AckUpdateAlready
  | AckPlayerSync
  | AckExist
  | -- | A code of a newer MPD.
    AckOther Int
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)

data ConnectionError
  = -- | The connection couldn't be opened, or MPD didn't greet as expected.
    ConnectFailed T.Text
  | -- | MPD is older than 'minimumVersion'.
    UnsupportedVersion Version
  | -- | MPD closed the connection before it began a reply. MPD closes a
    -- connection that was unused for longer than its @connection_timeout@,
    -- without running the command that arrives after that.
    Closed
  | -- | An I/O error, or the connection closed in the middle of a reply.
    Broken T.Text
  | TimedOut
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData)
