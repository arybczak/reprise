-- | What the handlers of events and the screens share: the effects, access
-- to the state, messages and the views. The screens import this module, and
-- it imports no screen, so that "Reprise.Handler" can pass actions on to
-- the screens.
module Reprise.Handler.Core
  ( -- * Effects
    App

    -- * State
  , getS
  , getsS
  , modifyS
  , newToken

    -- * Messages
  , showMessage
  , showError
  , notAvailable
  , countSongs
  , screenText

    -- * Prompts
  , openLine

    -- * Views
  , modifyView
  , setCursor
  , jumpTo
  , restoreView
  , screenLength
  ) where

import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Keymap
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.State

-- | The effects of the handlers.
type App es = (State AppState :> es, MpdRequest :> es, UiRequest :> es)

----------------------------------------
-- State

getS :: App es => Eff es AppState
getS = get @AppState

getsS :: App es => (AppState -> a) -> Eff es a
getsS = gets @AppState

modifyS :: App es => (AppState -> AppState) -> Eff es ()
modifyS = modify @AppState

newToken :: App es => Eff es Int
newToken = state @AppState $ \s -> (s.nextToken, s & #nextToken %~ (+ 1))

----------------------------------------
-- Messages

showMessage :: App es => T.Text -> Eff es ()
showMessage = message False

notAvailable :: App es => T.Text -> Eff es ()
notAvailable what = showMessage $ what <> " isn't available yet"

showError :: App es => T.Text -> Eff es ()
showError = message True

message :: App es => Bool -> T.Text -> Eff es ()
message isError text = do
  token <- newToken
  modifyS $ #message ?~ Message text isError token
  after messageTimeout (MessageExpired token)
  where
    -- How long a status bar message stays. ncmpcpp's default
    -- @message_delay_time@.
    messageTimeout :: Double
    messageTimeout = 5

countSongs :: Int -> T.Text
countSongs n = T.pack (show n) <> if n == 1 then " song" else " songs"

screenText :: ScreenName -> T.Text
screenText screen = T.replace "_" " " (screenName screen) <> " screen"

----------------------------------------
-- Prompts

-- | Open a line prompt for a purpose, which "Reprise.Handler" runs with the
-- answer.
openLine :: App es => T.Text -> LineEdit -> LinePurpose -> Eff es ()
openLine question edit purpose = modifyS $ #prompt ?~ Prompt question (Line edit purpose)

----------------------------------------
-- Views

-- | Move the cursor of the focused view and scroll it into view.
setCursor :: App es => Int -> Eff es ()
setCursor c = modifyView $ #cursor .~ c

-- | Move the cursor to an item in the middle of the list, as every jump
-- does, so that the item's neighbours show on both sides.
jumpTo :: App es => Int -> Eff es ()
jumpTo p = do
  h <- getsS (\s -> listHeight s (focusedView s))
  -- modifyView brings the offset back into the list.
  modifyView $ (#cursor .~ p) . (#offset .~ p - h `div` 2)

-- | Bring back a cursor and an offset, e.g. after a cancelled find.
restoreView :: App es => (Int, Int) -> Eff es ()
restoreView (c, o) = modifyView $ (#cursor .~ c) . (#offset .~ o)

-- | Change the focused view, then keep its cursor in the list and visible.
-- A screen of text has no cursor, so only its offset is kept in the text.
modifyView :: App es => (View -> View) -> Eff es ()
modifyView f = do
  s <- getS
  let centered = s.config.lists.keepCursorCentered
  modifyS $
    #views % ix s.focus %~ \v ->
      let v' = f v
          n = screenLength s v'.screen
          h = max 1 (listHeight s v')
          c = max 0 (min (n - 1) v'.cursor)
          o
            | v'.screen /= QueueScreen = v'.offset
            | centered = c - h `div` 2
            | c < v'.offset = c
            | c >= v'.offset + h = c - h + 1
            | otherwise = v'.offset
      in v' & #cursor .~ c & #offset .~ max 0 (min (n - h) o)

-- | The number of items or lines of a screen.
screenLength :: AppState -> ScreenName -> Int
screenLength s = \case
  QueueScreen -> Seq.length s.mirror.queue
  HelpScreen -> length (helpLines s.keymaps)
  _ -> 0
