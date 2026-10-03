-- | Requests and their serialization.
module MPD.Protocol.Request
  ( -- * Requests
    Request (..)
  , renderRequest
  , renderRequests

    -- * Arguments
  , Argument (..)
  , quote
  ) where

import Data.ByteString.Builder qualified as B
import Data.Fixed
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T

import MPD.Types

-- | One command line of a request, before quoting.
--
-- @since 0.1.0.0
data Request = Request
  { command :: Text
  , arguments :: [Text]
  }
  deriving stock (Eq, Show)

-- | Render a request as a line for MPD, with every argument quoted.
--
-- @since 0.1.0.0
renderRequest :: Request -> B.Builder
renderRequest r =
  T.encodeUtf8Builder r.command
    <> foldMap ((B.char7 ' ' <>) . quote) r.arguments
    <> B.char7 '\n'

-- | Render requests as one round trip: a plain line for one request, a
-- command list for more. A command list gets a reply part for each command.
--
-- @since 0.1.0.0
renderRequests :: [Request] -> B.Builder
renderRequests = \case
  [r] -> renderRequest r
  rs -> "command_list_ok_begin\n" <> foldMap renderRequest rs <> "command_list_end\n"

-- | Quote an argument. MPD accepts any argument in double quotes, with
-- backslashes before double quotes and backslashes.
--
-- @since 0.1.0.0
quote :: Text -> B.Builder
quote t = B.char7 '"' <> T.encodeUtf8Builder (T.concatMap escape t) <> B.char7 '"'
  where
    escape :: Char -> Text
    escape c
      | c == '"' || c == '\\' = T.pack ['\\', c]
      | otherwise = T.singleton c

-- | Values that are arguments of commands.
--
-- @since 0.1.0.0
class Argument a where
  toArgument :: a -> Text

-- | @since 0.1.0.0
instance Argument Text where
  toArgument = id

-- | @since 0.1.0.0
instance Argument Int where
  toArgument = T.pack . show

-- | MPD's booleans are @0@ and @1@.
--
-- @since 0.1.0.0
instance Argument Bool where
  toArgument b = if b then "1" else "0"

-- | @since 0.1.0.0
instance Argument SongPos where
  toArgument (SongPos p) = toArgument p

-- | @since 0.1.0.0
instance Argument SongId where
  toArgument (SongId i) = toArgument i

-- | @since 0.1.0.0
instance Argument Seconds where
  toArgument (Seconds s) = T.pack (showFixed True s)

-- | @since 0.1.0.0
instance Argument PlaylistVersion where
  toArgument (PlaylistVersion v) = T.pack (show v)

-- | @since 0.1.0.0
instance Argument Tag where
  toArgument = tagName

-- | @since 0.1.0.0
instance Argument SingleMode where
  toArgument = \case
    SingleOff -> "0"
    SingleOn -> "1"
    SingleOneshot -> "oneshot"

-- | @since 0.1.0.0
instance Argument ConsumeMode where
  toArgument = \case
    ConsumeOff -> "0"
    ConsumeOn -> "1"
    ConsumeOneshot -> "oneshot"

-- | @since 0.1.0.0
instance Argument ReplayGainMode where
  toArgument = \case
    ReplayGainOff -> "off"
    ReplayGainTrack -> "track"
    ReplayGainAlbum -> "album"
    ReplayGainAuto -> "auto"
