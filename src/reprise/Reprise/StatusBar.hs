-- | What the status bar shows. Both the layout, which draws it, and the
-- handlers, which take a click on the player's state, need it.
module Reprise.StatusBar
  ( StatusContent (..)
  , statusContent
  , playerLabel
  , onPlayerLabel
  ) where

import Data.Text qualified as T

import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Width

-- | What the status bar shows, of what there is, in this order.
data StatusContent
  = StatusPrompt Prompt
  | StatusPending PendingKeys
  | StatusMessage Message
  | StatusPlayer

statusContent :: AppState -> StatusContent
statusContent s = case (s.prompt, s.pendingKeys, s.message) of
  (Just p, _, _) -> StatusPrompt p
  (_, Just pending, _) -> StatusPending pending
  (_, _, Just m) -> StatusMessage m
  _ -> StatusPlayer

-- | The state of the player at the left of its status, while a song plays
-- or is paused.
playerLabel :: AppState -> Maybe T.Text
playerLabel s = case (s.mirror.status, currentSong s.mirror) of
  (Just st, Just _) -> case st.state of
    Playing -> Just "Playing: "
    Paused -> Just "Paused: "
    Stopped -> Nothing
  _ -> Nothing

-- | Whether a cell of the terminal, at a column and a row, is on the state
-- of the player that the status bar shows.
onPlayerLabel :: AppState -> Int -> Int -> Bool
onPlayerLabel s col row = case (statusContent s, playerLabel s) of
  (StatusPlayer, Just label) -> row == statusBarRow s && col >= 0 && col < textWidth label
  _ -> False
