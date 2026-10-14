-- | What changes on the screen with time, and the window title.
module Reprise.Handler.Redraw
  ( scheduleTick
  , restartTitle
  , updateWindowTitle
  ) where

import Control.Monad
import Data.Maybe
import Effectful
import Effectful.Input.Static
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Format
import Reprise.Handler.Core
import Reprise.Header
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Lyrics
import Reprise.State
import Reprise.UI.SongList

-- | Scroll the header's title from its start when it shows another subject.
restartTitle :: State AppState :> es => Eff es ()
restartTitle = do
  s <- getS
  when (fmap fst s.titleShown /= Just (titleSubject s))
    $ modifyS
    $ #titleShown ?~ (titleSubject s, s.now)

-- | Schedule a redraw for the next change of the screen with time: of the
-- elapsed time, the next whole second or the next cell of the progress bar,
-- or of the header's title while it scrolls.
scheduleTick :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es) => Eff es ()
scheduleTick = do
  env <- getAppEnv
  s <- getS
  forM_ (nextRedraw env s) $ \t -> case s.tick of
    Just (_, scheduled) | scheduled <= t -> pure ()
    _ -> do
      token <- newToken
      modifyS $ #tick ?~ (token, t)
      after (t - s.now) (Tick token)

nextRedraw :: AppEnv -> AppState -> Maybe Double
nextRedraw env s = case catMaybes [elapsedRedraw s, titleRedraw, nextLyricsLine s] of
  [] -> Nothing
  ts -> Just (minimum ts)
  where
    -- The next whole second since the title began to show its subject.
    titleRedraw :: Maybe Double
    titleRedraw = do
      guard $ titleScrolls env s
      let since = titleSince s
      pure $ since + fromIntegral (floor @Double @Int (s.now - since) + 1)

elapsedRedraw :: AppState -> Maybe Double
elapsedRedraw s = do
  st <- s.mirror.status
  guard $ st.state == Playing && isNothing s.seek
  e <- realToFrac <$> elapsedAt s.now s.mirror
  let width = fst s.terminalSize
      nextSecond = fromIntegral (floor @Double @Int e + 1) - e
  delay <- case realToFrac <$> st.duration of
    -- A stream has no length, so it has no progress bar to move.
    Nothing -> Just nextSecond
    Just d
      | e >= d -> Nothing
      | width > 0 -> Just . min nextSecond $ nextCellAt width e d - e
      | otherwise -> Just nextSecond
  pure $ s.now + delay

updateWindowTitle
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Eff es ()
updateWindowTitle = do
  env <- getAppEnv
  s <- getS
  forM_ env.config.windowTitle $ \fmt -> do
    let title = case (currentSong s.mirror, (.state) <$> s.mirror.status) of
          (Just song, Just st)
            | st /= Stopped -> renderPlain (unstyledContext env.config.songs) song fmt
          _ -> "reprise"
    when (s.windowTitle /= Just title) $ do
      modifyS $ #windowTitle ?~ title
      setTitle title
