-- | The browser screen: the directories of MPD's database and the playlists,
-- which the user moves through and adds songs from.
module Reprise.Screen.Browser
  ( -- * Drawing
    browserView
  , browserTitle

    -- * Moving
  , moveBrowserCursor

    -- * Listing
  , openBrowser
  , relistBrowser
  , browserListed
  , activateItem
  , leave
  ) where

import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Format
import Reprise.Handler.Core
import Reprise.Mpd.Protocol.Command
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Style
import Reprise.UI.SongList

----------------------------------------
-- Drawing

browserView :: AppEnv -> AppState -> View -> V.Image
browserView env s v =
  let ctx =
        RowContext
          { colorMode = env.colorMode
          , lists = env.config.lists
          , songs = env.config.songs
          , display = env.config.browser.display
          , width = v.width
          }
      visible = Seq.take (listHeight env s v) (Seq.drop v.offset s.browser.items)
      row i item =
        let flags = RowFlags {playing = False, selected = False, found = False, cursor = i == v.cursor}
        in case item of
             ParentItem -> renderOtherRow ctx flags [Span Nothing ".."]
             EntryItem (DirectoryEntry d) ->
               renderOtherRow ctx flags [Span Nothing ("[" <> baseName d.path <> "]")]
             EntryItem (PlaylistEntry p) ->
               renderOtherRow ctx flags $ playlistPrefix p <> [Span Nothing (baseName p.path)]
             EntryItem (SongEntry song) -> renderRow ctx flags song
  in V.vertCat $ zipWith row [v.offset ..] (toList visible)
  where
    -- A field of the prefix reads the playlist's path as the file.
    playlistPrefix :: Playlist -> [Span Style]
    playlistPrefix p =
      renderFormat
        (RenderContext env.config.lists.tagSeparator [])
        Song
          { file = p.path
          , tags = M.empty
          , duration = Nothing
          , range = Nothing
          , lastModified = p.lastModified
          , format = Nothing
          , position = Nothing
          , songId = Nothing
          , priority = 0
          }
        env.config.browser.playlistPrefix

-- | The title in the header: what the browser lists.
browserTitle :: AppState -> T.Text
browserTitle s = "Browse: /" <> maybe "" locationPath s.browser.location

----------------------------------------
-- Moving

moveBrowserCursor :: MoveTarget -> AppEnv -> AppState -> AppState
moveBrowserCursor t env s = moveListCursor itemSong s.browser.items t env s
  where
    itemSong :: BrowserItem -> Maybe Song
    itemSong = \case
      EntryItem (SongEntry song) -> Just song
      _ -> Nothing

----------------------------------------
-- Listing

-- | List the root the first time the browser shows.
openBrowser :: App es => Eff es ()
openBrowser = do
  location <- getsS (.browser.location)
  case location of
    Nothing -> list (InDirectory "") Nothing
    Just _ -> pure ()

-- | List again what the browser lists, e.g. after a new connection. Before
-- its first listing, only a browser that shows lists the root.
relistBrowser :: App es => Eff es ()
relistBrowser = do
  s <- getS
  case s.browser.location of
    Just location -> list location Nothing
    Nothing
      | (focusedView s).screen == BrowserScreen -> list (InDirectory "") Nothing
      | otherwise -> pure ()

-- | Enter the directory or open the playlist under the cursor, or go up
-- from @..@.
activateItem :: App es => Eff es ()
activateItem = do
  s <- getS
  forM_ (Seq.lookup (focusedView s).cursor s.browser.items) $ \case
    ParentItem -> leave
    EntryItem (DirectoryEntry d) -> list (InDirectory d.path) Nothing
    EntryItem (PlaylistEntry p) -> list (InPlaylist p.path) Nothing
    EntryItem (SongEntry _) -> notAvailable "Playing a song from the browser"

-- | Go up to the directory of what the browser lists, with the cursor on
-- where it came from. It goes up from a listing on its way, too, so that
-- keys typed ahead of a reply add up.
leave :: App es => Eff es ()
leave = do
  b <- getsS (.browser)
  forM_ (maybe b.location (Just . (.location)) b.listing) $ \location ->
    forM_ (parentOf location) $ \up -> list up (Just location)
  where
    parentOf :: Location -> Maybe Location
    parentOf = \case
      InDirectory "" -> Nothing
      InDirectory path -> Just $ InDirectory (directoryOf path)
      InPlaylist path -> Just $ InDirectory (directoryOf path)

    directoryOf :: T.Text -> T.Text
    directoryOf = T.dropEnd 1 . fst . T.breakOnEnd "/"

list :: App es => Location -> Maybe Location -> Eff es ()
list location cursorOn = do
  token <- newToken
  modifyS $ #browser % #listing ?~ Listing token location cursorOn
  case location of
    InDirectory path -> request (lsInfo path) (BrowserListed token)
    InPlaylist name -> request (map SongEntry <$> listPlaylistInfo name) (BrowserListed token)

-- | Show the entries of the latest listing. The reply to a listing that a
-- newer one replaced changes nothing.
browserListed :: App es => Int -> [Entry] -> Eff es ()
browserListed token entries =
  getsS (.browser.listing) >>= \case
    Just l | l.token == token -> do
      let items =
            Seq.fromList $
              [ParentItem | l.location /= InDirectory ""] <> map EntryItem entries
          found = l.cursorOn >>= \c -> Seq.findIndexL (isAt c) items
      modifyS $ #browser .~ BrowserState (Just l.location) items Nothing
      modifyWithEnv $ placeCursor found
    _ -> keepScreen
  where
    isAt :: Location -> BrowserItem -> Bool
    isAt location item = case (location, item) of
      (InDirectory path, EntryItem (DirectoryEntry d)) -> d.path == path
      (InPlaylist path, EntryItem (PlaylistEntry p)) -> p.path == path
      _ -> False

-- | Put the browser's cursor on an item in the middle of the list, or on the
-- first item, also while the view shows another screen.
placeCursor :: Maybe Int -> AppEnv -> AppState -> AppState
placeCursor found env s
  | (focusedView s).screen == BrowserScreen = maybe (restoreView (0, 0)) jumpTo found env s
  | otherwise =
      let h = listHeight env s (focusedView s & #screen .~ BrowserScreen)
          position = maybe (0, 0) (\i -> (i, max 0 (i - h `div` 2))) found
      in s & #views % ix s.focus % #positions % at BrowserScreen ?~ position

locationPath :: Location -> T.Text
locationPath = \case
  InDirectory path -> path
  InPlaylist path -> path

-- | The last part of a path.
baseName :: T.Text -> T.Text
baseName = snd . T.breakOnEnd "/"
