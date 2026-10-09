-- | What the header's first line shows: the title of the focused screen,
-- which scrolls if it doesn't fit, and the volume. Both the handlers, which
-- redraw it while it scrolls, and the layout need it.
module Reprise.Header
  ( titleSubject
  , shownTitle
  , titleSince
  , titleScrolls
  , headerRight
  ) where

import Data.Text qualified as T

import Reprise.Action
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Width

-- | What the title shows: the focused screen, and what the screen shows.
-- The scrolling starts again when it changes.
titleSubject :: AppState -> (ScreenName, Maybe T.Text)
titleSubject s =
  let screen = (focusedView s).screen
  in (screen, (screenInfo screen).subject s)

-- | The focused screen's title as the header shows it. The part that doesn't
-- fit next to the volume scrolls by a character for each second since the
-- title began to show its subject.
shownTitle :: AppEnv -> AppState -> T.Text
shownTitle env s =
  let (stays, rest) = (screenInfo (focusedView s).screen).title env s
  in stays <> scrollText (titleRoom s stays) (floor (s.now - titleSince s)) rest

-- | When the title began to show its subject.
titleSince :: AppState -> Double
titleSince s = maybe s.now snd s.titleShown

-- | Whether the focused screen's title scrolls, for which the header is drawn
-- again each second.
titleScrolls :: AppEnv -> AppState -> Bool
titleScrolls env s =
  let (stays, rest) = (screenInfo (focusedView s).screen).title env s
  in textWidth rest > titleRoom s stays

-- | The columns of a title's part that scrolls, with a space before the
-- volume.
titleRoom :: AppState -> T.Text -> Int
titleRoom s stays = max 0 (fst s.terminalSize - textWidth stays - textWidth (headerRight s) - 1)

-- | The right of the header's first line: the volume, or the state of the
-- connection.
headerRight :: AppState -> T.Text
headerRight s = case s.connection of
  Connecting -> "Connecting…"
  Disconnected _ -> "Disconnected"
  Connected _ -> case s.mirror.status >>= (.volume) of
    Just v -> "Volume: " <> T.pack (show v) <> "%"
    Nothing -> "Volume: n/a"
