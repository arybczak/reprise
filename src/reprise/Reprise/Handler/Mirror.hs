-- | Fetching MPD's queue and status into the mirror, and what follows a
-- change of the mirror.
module Reprise.Handler.Mirror
  ( fetchQueue
  , fetchQueueChanges
  , applyChanges
  , endEdit
  , updateMirror
  ) where

import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Effectful
import Effectful.Input.Static
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Handler.Core
import Reprise.Handler.Seek
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Queue
import Reprise.Selection
import Reprise.State

fetchQueue :: MpdRequest :> es => Eff es ()
fetchQueue = request ((,) <$> status <*> playlistInfo) QueueFetched

-- | The status and the changes of the queue come in one command list, so
-- the changes match the length in the status.
fetchQueueChanges :: (State AppState :> es, MpdRequest :> es) => Eff es ()
fetchQueueChanges =
  getsS (.mirror.queueVersion) >>= \case
    Just v -> request ((,) <$> status <*> plChanges v) QueueChangesFetched
    Nothing -> fetchQueue

applyChanges
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => (Status, [Song])
  -> Eff es ()
applyChanges (st, changes) = do
  s <- getS
  case applyQueueChanges s.now st changes s.mirror of
    Right m -> updateMirror (const m)
    Left err -> do
      showError $ "The queue is out of sync, fetching it again: " <> err
      fetchQueue

-- | End the edit that waited for its reply: fetch the changes of the queue
-- that MPD reported meanwhile, and return the input that waited.
endEdit :: (State AppState :> es, MpdRequest :> es) => Eff es (Seq.Seq HeldInput)
endEdit = do
  edit <- getsS (.pendingEdit)
  modifyS $ #pendingEdit .~ Nothing
  when (any (.queueChanged) edit) fetchQueueChanges
  pure $ maybe Seq.empty (.heldInput) edit

-- | Change the mirror and react to the changes: keep the cursors in the
-- list, follow the playing song, and report changed options.
updateMirror
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => (Mirror -> Mirror)
  -> Eff es ()
updateMirror f = do
  old <- getsS (.mirror)
  modifyS $ #mirror %~ f
  new <- getsS (.mirror)
  -- Songs that left the queue leave the selection.
  when (new.queueVersion /= old.queueVersion) $ do
    let ids = S.fromList . mapMaybe (.songId) $ toList new.queue
    modifyS $ #queueState % #selection %~ restrictTo ids
  modifyWithEnv (modifyView id)
  s <- getS
  let oldId = old.status >>= (.currentId)
      newId = new.status >>= (.currentId)
      -- The queue comes with the status, the first time too.
      firstQueue = isNothing old.queueVersion && isJust new.queueVersion
  -- A seek that the keys moved is of the song that played.
  when (oldId /= newId) dropSeek
  if
    | firstQueue -> modifyWithEnv jumpToPlaying
    | s.toggles.followPlaying && oldId /= newId -> modifyWithEnv jumpToPlaying
    | otherwise -> pure ()
  case (old.status, new.status) of
    (Just a, Just b) -> reportOptions a b
    _ -> pure ()

reportOptions :: (State AppState :> es, UiRequest :> es) => Status -> Status -> Eff es ()
reportOptions a b =
  case catMaybes changes of
    [] -> pure ()
    ms -> showMessage $ T.intercalate ", " ms
  where
    changes :: [Maybe T.Text]
    changes =
      [ changed (.repeat) $ "Repeat: " <> onOff b.repeat
      , changed (.random) $ "Random: " <> onOff b.random
      , changed (.single) $
          "Single: " <> case b.single of
            SingleOff -> "off"
            SingleOn -> "on"
            SingleOneshot -> "oneshot"
      , changed (.consume) $
          "Consume: " <> case b.consume of
            ConsumeOff -> "off"
            ConsumeOn -> "on"
            ConsumeOneshot -> "oneshot"
      , changed (.crossfade) $ "Crossfade: " <> T.pack (show b.crossfade) <> "s"
      ]

    changed :: Eq a => (Status -> a) -> T.Text -> Maybe T.Text
    changed field msg = if field a /= field b then Just msg else Nothing
