-- | Seeking in the playing song: by the keys, which move a target on the
-- progress bar and seek once they stop, to a time, and at once.
module Reprise.Handler.Seek
  ( seekAction
  , commitSeek
  , seekNow
  , dropSeek
  ) where

import Effectful
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Action
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Handler.Core
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.State

-- | The pause after the last seek key press that sends the seek. It must be
-- longer than the terminal's key repeat delay, or the gap before the first
-- repeat would end the seek: X11's default delay is 660 ms, and a repeat
-- comes every 40 ms at its default rate of 25 per second.
seekCommitDelay :: Double
seekCommitDelay = 0.66 + 1 / 25

-- | The seek step grows by a second for each this many seconds that the key
-- is held, as in ncmpcpp's incremental seeking.
seekAccelerationPeriod :: Double
seekAccelerationPeriod = 2

seekAction
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => SeekStep
  -> Eff es ()
seekAction = \case
  SeekBy n -> do
    s <- getS
    case (currentDuration s, displayedElapsed s) of
      (Just d, Just e) -> do
        let (base, started) = case s.seek of
              Just sk -> (sk.target, sk.started)
              Nothing -> (e, s.now)
            held = s.now - started
            step =
              fromIntegral (abs n) + fromIntegral (floor @Double @Int (held / seekAccelerationPeriod))
            target = max 0 . min d $ if n >= 0 then base + step else base - step
        token <- newToken
        modifyS $
          #seek
            ?~ SeekState
              { target = target
              , started = started
              , songId = s.mirror.status >>= (.currentId)
              , token = token
              }
        after seekCommitDelay (SeekCommit token)
      _ -> noLength
  SeekToSecond n -> do
    dropSeek
    mutate . seekCur . SeekTo $ fromIntegral n
  SeekToPercent p -> do
    dropSeek
    s <- getS
    case currentDuration s of
      Just d -> mutate . seekCur . SeekTo $ d * fromIntegral p / 100
      Nothing -> noLength
  where
    -- MPD's status has no length while nothing plays, nor for a stream.
    noLength :: (State AppState :> es, UiRequest :> es) => Eff es ()
    noLength =
      getsS (.mirror.status) >>= \case
        Just st | st.state /= Stopped -> showError "The current song has no length"
        _ -> showMessage "Nothing plays"

-- | Send the seek that the keys moved, if its song still plays or pauses.
commitSeek
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Int
  -> Eff es ()
commitSeek token = whenCurrent (.seek) (.token) token $ \sk -> do
  s <- getS
  case s.mirror.status of
    Just st | st.state /= Stopped && st.currentId == sk.songId -> seekNow sk.target
    _ -> dropSeek

-- | Seek in the current song, and show its new time at once. A seek that
-- the keys moved ends.
seekNow :: (State AppState :> es, MpdRequest :> es) => Seconds -> Eff es ()
seekNow target = do
  dropSeek
  now <- getsS (.now)
  modifyS $ #mirror %~ seekTo now target
  mutate . seekCur $ SeekTo target

-- | Drop the seek that the keys moved, e.g. for a command that moves the
-- song itself.
dropSeek :: State AppState :> es => Eff es ()
dropSeek = modifyS $ #seek .~ Nothing

currentDuration :: AppState -> Maybe Seconds
currentDuration s = s.mirror.status >>= (.duration)
