-- | Saving as a stored playlist.
module Reprise.Handler.Save
  ( saveNamed
  , saveChecked
  , saveTo
  ) where

import Control.Monad
import Data.Foldable
import Data.Text qualified as T
import Effectful
import Effectful.State.Static.Local

import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Handler.Core
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Save
import Reprise.State

-- | Ask MPD which stored playlists there are, with the songs of those to
-- save, before a save to the one of a name. An empty name saves nothing.
saveNamed :: MpdRequest :> es => SaveSource -> T.Text -> Eff es ()
saveNamed source name
  | T.null name = pure ()
  | otherwise =
      request ((,) <$> listPlaylists <*> traverse listPlaylistInfo playlists) $
        \(existing, songs) -> SaveChecked name (withSongs songs) (name `elem` existing)
  where
    playlists :: [T.Text]
    playlists = case source of
      SaveQueue -> []
      SaveItems items -> [p | SavePlaylist p <- items]

    -- The songs of each stored playlist take its place.
    withSongs :: [[Song]] -> SaveSource
    withSongs songs = case source of
      SaveQueue -> SaveQueue
      SaveItems items -> SaveItems (go items songs)
      where
        go :: [SaveItem] -> [[Song]] -> [SaveItem]
        go (SavePlaylist _ : rest) (ss : more) = map songToSave ss <> go rest more
        go (item : rest) more = item : go rest more
        go [] _ = []

-- | Save to a new stored playlist, or ask whether to replace the one of the
-- name or to append to it.
saveChecked
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => T.Text
  -> SaveSource
  -> Bool
  -> Eff es ()
saveChecked name source exists
  | Just why <- nothingToSave source = showMessage why
  | exists =
      modifyS $
        openChoice
          ("The playlist " <> name <> " exists.")
          [ ChoiceOption
              { letter = 'r'
              , name = "replace"
              , event = Just (SaveTo name source ReplacePlaylist)
              }
          , ChoiceOption
              { letter = 'a'
              , name = "append"
              , event = Just (SaveTo name source AppendToPlaylist)
              }
          ]
  | otherwise = saveTo name source CreatePlaylist

saveTo
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => T.Text
  -> SaveSource
  -> SaveMode
  -> Eff es ()
saveTo name source mode = do
  mutate $ case source of
    SaveQueue -> save name mode
    SaveItems items ->
      when (mode == ReplacePlaylist) (playlistClear name)
        *> traverse_ addItem (savedItems items)
  showMessage $
    ( case mode of
        CreatePlaylist -> "Saved " <> describeSave source <> " as " <> name
        ReplacePlaylist -> "Replaced " <> name <> " with " <> describeSave source
        AppendToPlaylist -> "Added " <> describeSave source <> " to " <> name
    )
      <> withoutParts (partsLeftOut source)
  where
    addItem :: SaveItem -> Command ()
    addItem = \case
      SaveSong uri -> playlistAdd name uri
      SaveDirectory path -> playlistAddDirectory name path
      -- 'saveNamed' put their songs in their places, and 'savedItems' left
      -- the parts out.
      SavePlaylist _ -> pure ()
      SavePart -> pure ()
