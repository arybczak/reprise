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
  , browserChanged
  , browserListed
  , browserFailed
  , activateItem
  , leave
  ) where

import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
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
    Nothing -> list (InDirectory "") AtTop
    Just _ -> pure ()

-- | List again what the browser lists, with the cursor on the same item,
-- e.g. after a new connection. A listing on its way is requested again, as
-- its reply may be lost or out of date. Before its first listing, only a
-- browser that shows lists the root.
relistBrowser :: App es => Eff es ()
relistBrowser = do
  s <- getS
  case (s.browser.listing, s.browser.location) of
    (Just l, _) -> list l.location l.cursor
    (Nothing, Just location) ->
      list location . maybe AtTop (StayOn . itemKey) $
        Seq.lookup (fst (browserPosition s)) s.browser.items
    (Nothing, Nothing)
      | (focusedView s).screen == BrowserScreen -> list (InDirectory "") AtTop
      | otherwise -> pure ()

-- | List again after a change of the database, or of the stored playlists,
-- which show at the root and open as playlists.
browserChanged :: App es => [Subsystem] -> Eff es ()
browserChanged subsystems = do
  location <- getsS (.browser.location)
  let storedPlaylists = case location of
        Just (InDirectory "") -> True
        Just (InPlaylist _) -> True
        _ -> False
  when
    ( isJust location
        && ( DatabaseSubsystem `elem` subsystems
               || (StoredPlaylistSubsystem `elem` subsystems && storedPlaylists)
           )
    )
    relistBrowser

-- | Enter the directory or open the playlist under the cursor, or go up
-- from @..@.
activateItem :: App es => Eff es ()
activateItem = do
  s <- getS
  forM_ (Seq.lookup (focusedView s).cursor s.browser.items) $ \case
    ParentItem -> leave
    EntryItem (DirectoryEntry d) -> list (InDirectory d.path) AtTop
    EntryItem (PlaylistEntry p) -> list (InPlaylist p.path) AtTop
    EntryItem (SongEntry _) -> notAvailable "Playing a song from the browser"

-- | Go up to the directory of what the browser lists, with the cursor on
-- where it came from. It goes up from a listing on its way, too, so that
-- keys typed ahead of a reply add up.
leave :: App es => Eff es ()
leave = do
  b <- getsS (.browser)
  forM_ (maybe b.location (Just . (.location)) b.listing) $ \location ->
    forM_ (parentOf location) $ \up -> list up . JumpTo $ case location of
      InDirectory path -> DirectoryKey path
      InPlaylist path -> PlaylistKey path

-- | The directory that a directory or a playlist is in.
parentOf :: Location -> Maybe Location
parentOf = \case
  InDirectory "" -> Nothing
  InDirectory path -> Just $ InDirectory (directoryOf path)
  InPlaylist path -> Just $ InDirectory (directoryOf path)
  where
    directoryOf :: T.Text -> T.Text
    directoryOf = T.dropEnd 1 . fst . T.breakOnEnd "/"

list :: App es => Location -> ListingCursor -> Eff es ()
list location cursor = do
  token <- newToken
  modifyS $ #browser % #listing ?~ Listing token location cursor
  case location of
    InDirectory path -> requestOr (lsInfo path) (BrowserFailed token) (BrowserListed token)
    InPlaylist name ->
      requestOr
        (map SongEntry <$> listPlaylistInfo name)
        (BrowserFailed token)
        (BrowserListed token)

-- | Go up from a directory or a playlist that is gone, until one exists, as
-- ncmpcpp does. Another error shows, and the browser stays as it was.
browserFailed :: App es => Int -> MpdError -> Eff es ()
browserFailed token err =
  getsS (.browser.listing) >>= \case
    Just l | l.token == token -> case (err, parentOf l.location) of
      (AckError ack, Just up) | ack.code == AckNoExist -> list up AtTop
      _ -> do
        modifyS $ #browser % #listing .~ Nothing
        showError . T.pack $ displayException err
    _ -> keepScreen

-- | Show the entries of the latest listing. The reply to a listing that a
-- newer one replaced changes nothing.
browserListed :: App es => Int -> [Entry] -> Eff es ()
browserListed token entries =
  getsS (.browser.listing) >>= \case
    Just l | l.token == token -> do
      let items =
            Seq.fromList $
              [ParentItem | l.location /= InDirectory ""] <> map EntryItem entries
      modifyS $ #browser .~ BrowserState (Just l.location) items Nothing
      modifyWithEnv $ placeCursor l.cursor
    _ -> keepScreen

-- | Put the browser's cursor where a listing says, also while the view shows
-- another screen.
placeCursor :: ListingCursor -> AppEnv -> AppState -> AppState
placeCursor cursor env s =
  let (c, o) = browserPosition s
      h = listHeight env s (focusedView s & #screen .~ BrowserScreen)
      indexOf k = Seq.findIndexL ((== k) . itemKey) s.browser.items
      (c', o') = case cursor of
        AtTop -> (0, 0)
        JumpTo k -> maybe (0, 0) (\i -> (i, i - h `div` 2)) (indexOf k)
        StayOn k -> (fromMaybe c (indexOf k), o)
  in if (focusedView s).screen == BrowserScreen
       then restoreView (c', o') env s
       else s & #views % ix s.focus % #positions % at BrowserScreen ?~ (c', max 0 o')

-- | The cursor and the offset of the browser in the focused view, which
-- remembers them while it shows another screen.
browserPosition :: AppState -> (Int, Int)
browserPosition s
  | v.screen == BrowserScreen = (v.cursor, v.offset)
  | otherwise = M.findWithDefault (0, 0) BrowserScreen v.positions
  where
    v :: View
    v = focusedView s

itemKey :: BrowserItem -> ItemKey
itemKey = \case
  ParentItem -> ParentKey
  EntryItem (DirectoryEntry d) -> DirectoryKey d.path
  EntryItem (SongEntry song) -> SongKey song.file song.range
  EntryItem (PlaylistEntry p) -> PlaylistKey p.path

locationPath :: Location -> T.Text
locationPath = \case
  InDirectory path -> path
  InPlaylist path -> path

-- | The last part of a path.
baseName :: T.Text -> T.Text
baseName = snd . T.breakOnEnd "/"
