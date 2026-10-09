-- | The browser screen: the directories of MPD's database and the playlists,
-- which the user moves through and adds songs from.
module Reprise.Screen.Browser
  ( -- * Drawing
    browserView

    -- * Verbs
  , browserVerb

    -- * Listing
  , openBrowser
  , relistBrowser
  , browserChanged
  , browserListed
  , browserFailed
  , locateSong

    -- * Updating
  , browserDirectory
  ) where

import Control.Monad
import Data.Foldable
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time
import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Collation
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Event
import Reprise.Exception
import Reprise.Find
import Reprise.Format
import Reprise.Groups
import Reprise.Handler.Core
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Save
import Reprise.Selection
import Reprise.State
import Reprise.Style
import Reprise.UI.SongList

----------------------------------------
-- Drawing

browserView :: AppEnv -> AppState -> View -> V.Image
browserView env s v =
  let ctx = rowContext env s.toggles.browserDisplay v.width
      titles = titleRow env s v ctx
      h = listHeight env s v
      visible = Seq.take h (Seq.drop v.offset s.browser.items)
      found = typedMatches s $ Seq.take h (Seq.drop v.offset s.browser.rows)
      row (i, isFound) item =
        let flags =
              RowFlags
                { queued = case item of
                    EntryItem (SongEntry song) -> songKey song `S.member` s.mirror.queued
                    _ -> False
                , playing = False
                , selected = isSelected (itemKey item) s.browser.selection
                , found = isFound
                , cursor = i == v.cursor
                }
        in case rowContent env item of
             SongRow song -> renderRow ctx flags song
             OtherRow spans -> renderOtherRow ctx flags spans
  in V.vertCat $ titles <> zipWith row (zip [v.offset ..] found) (toList visible)

-- | What the row of an item shows. A row of another item than a song spans
-- the columns.
data RowContent = SongRow Song | OtherRow [Span Style]

rowContent :: AppEnv -> BrowserItem -> RowContent
rowContent env = \case
  ParentItem -> OtherRow [Span Nothing ".."]
  EntryItem (DirectoryEntry d) -> OtherRow [Span Nothing ("[" <> baseName d.path <> "]")]
  EntryItem (PlaylistEntry p) -> OtherRow $ playlistPrefix p <> [Span Nothing (baseName p.path)]
  EntryItem (SongEntry song) -> SongRow song
  where
    -- A field of the prefix reads the playlist's path as the file.
    playlistPrefix :: Playlist -> [Span Style]
    playlistPrefix p =
      renderFormat
        (plainContext env.config.lists)
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

-- | The text of each item as finds match it, in a display.
itemRows :: AppEnv -> Display -> Seq.Seq BrowserItem -> Seq.Seq Folded
itemRows env display = fmap $ \item -> foldText $ case rowContent env item of
  SongRow song -> rowText env.config.lists env.config.songs display song
  OtherRow spans -> spansText spans

----------------------------------------
-- Verbs

-- | How the browser does a verb, if it does it.
browserVerb :: App es => Action -> Maybe (Eff es ())
browserVerb = \case
  Move t -> Just $ modifyWithEnv (moveListCursor t)
  JumpToPlaying ->
    Just $
      getsS (currentSong . (.mirror)) >>= \case
        Nothing -> showMessage "No song is playing"
        Just song -> locateSong song
  Activate -> Just activateItem
  Parent -> Just leave
  Save -> Just $ askSaveName =<< getsS browserToSave
  NextSortMode -> Just nextSortMode
  Add p -> Just $ addMarked p
  AddAndPlay -> Just addAndPlay
  AddOrRemove -> Just addOrRemove
  Select t -> Just $ selectInBrowser t
  Toggle ToggleDisplay -> Just toggleBrowserDisplay
  _ -> Nothing

-- | Show the songs in the other display, which finds match too.
toggleBrowserDisplay :: App es => Eff es ()
toggleBrowserDisplay = do
  toggleDisplay #browserDisplay
  env <- getAppEnv
  s <- getS
  modifyS $ #browser % #rows .~ itemRows env s.toggles.browserDisplay s.browser.items

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
-- its reply may be lost or out of date. A browser that never showed lists
-- nothing.
relistBrowser :: App es => Eff es ()
relistBrowser = do
  s <- getS
  case (s.browser.listing, s.browser.location) of
    (Just l, _) -> list l.location l.cursor
    (Nothing, Just location) ->
      list location . maybe AtTop (StayOn . itemKey) $
        Seq.lookup (fst (screenPosition BrowserScreen s)) s.browser.items
    (Nothing, Nothing) -> pure ()

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

-- | Go up to the directory of what the browser lists, with the cursor on
-- where it came from. It goes up from a listing on its way, too, so that
-- keys typed ahead of a reply add up.
leave :: App es => Eff es ()
leave = do
  b <- getsS (.browser)
  forM_ (maybe b.location (Just . (.location)) b.listing) goUp

-- | Go up to the directory of a directory or a playlist, with the cursor on
-- it.
goUp :: App es => Location -> Eff es ()
goUp location =
  forM_ (parentOf location) $ \up -> list up . JumpTo $ case location of
    InDirectory path -> DirectoryKey path
    InPlaylist path -> PlaylistKey path

-- | List the directory of a song, with the cursor on the song, as ncmpcpp's
-- @jump_to_browser@ does. A stream isn't in the database.
locateSong :: App es => Song -> Eff es ()
locateSong song
  | isStream song = showError "The song isn't in MPD's database"
  | otherwise =
      list (InDirectory (directoryOf song.file)) (JumpTo (SongKey song.file song.range))

-- | The directory that a directory or a playlist is in.
parentOf :: Location -> Maybe Location
parentOf = \case
  InDirectory "" -> Nothing
  InDirectory path -> Just $ InDirectory (directoryOf path)
  InPlaylist path -> Just $ InDirectory (directoryOf path)

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
browserFailed token err = whenCurrent (.browser.listing) (.token) token $ \l ->
  case (err, parentOf l.location) of
    (AckError ack, Just up) | ack.code == AckNoExist -> list up AtTop
    _ -> do
      modifyS $ #browser % #listing .~ Nothing
      showError $ exceptionText err

-- | Show the entries of the latest listing. The reply to a listing that a
-- newer one replaced changes nothing. The selection stays in a listing of
-- the same, without the items that are gone.
browserListed :: App es => Int -> [Entry] -> Eff es ()
browserListed token entries = whenCurrent (.browser.listing) (.token) token $ \l -> do
  env <- getAppEnv
  s <- getS
  let items = arrange env s.toggles.browserSort l.location entries
      same = s.browser.location == Just l.location
      selection
        | same = restrictTo (S.fromList (map itemKey (toList items))) s.browser.selection
        | otherwise = noSelection
  modifyS $
    #browser
      .~ BrowserState
        (Just l.location)
        entries
        items
        (itemRows env s.toggles.browserDisplay items)
        selection
        Nothing
  modifyWithEnv $ placeCursor l.cursor

----------------------------------------
-- Selection

selectInBrowser :: App es => SelectTarget -> Eff es ()
selectInBrowser t = do
  items <- getsS (.browser.items)
  selectInList
    (#browser % #selection)
    (selectable <$> items)
    (getsS (.browser.rows))
    countItems
    t
  where
    -- @..@ can't be selected.
    selectable :: BrowserItem -> (Maybe ItemKey, Maybe Song)
    selectable = \case
      ParentItem -> (Nothing, Nothing)
      item@(EntryItem (SongEntry song)) -> (Just (itemKey item), Just song)
      item -> (Just (itemKey item), Nothing)

----------------------------------------
-- Adding

-- | Enter the directory or open the playlist under the cursor, go up from
-- @..@, or play the song. @..@ goes up from the listing that it is in, not
-- from one on its way, so that enter held on it goes up once: until the
-- reply, the screen still shows it.
activateItem :: App es => Eff es ()
activateItem = do
  s <- getS
  forM_ (cursorItem s) $ \case
    (_, ParentItem) -> forM_ s.browser.location goUp
    (_, EntryItem (DirectoryEntry d)) -> list (InDirectory d.path) AtTop
    (_, EntryItem (PlaylistEntry p)) -> list (InPlaylist p.path) AtTop
    item@(_, EntryItem (SongEntry _)) -> addAndPlayItems [item]

-- | Add the marked items to the queue.
addMarked :: App es => AddPosition -> Eff es ()
addMarked p = do
  s <- getS
  forM_ s.browser.location $ \location -> case markedItems s of
    [] -> pure ()
    items -> do
      mutate . addItems location items $ case p of
        AddEnd -> Nothing
        AddNext -> Just (AfterCurrent 0)
        AddBeginning -> Just (At 0)
      env <- getAppEnv
      showMessage $ addedText env items

-- | Add the marked items and play the first. A single song that is already
-- in the queue plays there instead.
addAndPlay :: App es => Eff es ()
addAndPlay = getsS markedItems >>= addAndPlayItems

addAndPlayItems :: App es => [(Int, BrowserItem)] -> Eff es ()
addAndPlayItems items = do
  s <- getS
  forM_ s.browser.location $ \location -> case items of
    [] -> pure ()
    [(_, EntryItem (SongEntry song))] | i : _ <- queuedIds song s -> mutate $ playId i
    _ -> do
      -- In one command list, which no other client's command interrupts, so
      -- the songs start at the length even if the mirror missed a change.
      let n = SongPos (queueLength s.mirror)
      mutate $ addItems location items (Just (At n)) *> play (Just n)

-- | Add the item under the cursor, or remove its song if it is in the
-- queue, then move down.
addOrRemove :: App es => Eff es ()
addOrRemove = do
  s <- getS
  env <- getAppEnv
  forM_ ((,) <$> s.browser.location <*> cursorItem s) $ \case
    (_, (_, ParentItem)) -> pure ()
    (_, (_, EntryItem (SongEntry song))) | ids@(_ : _) <- queuedIds song s -> do
      mutate $ traverse_ deleteId ids
      showMessage $ "Removed: " <> songText env song
    (location, item) -> do
      mutate $ addItems location [item] Nothing
      showMessage $ addedText env [item]
  modifyWithEnv $ moveListCursor MoveDown

-- | The commands that add items to the queue in their order, at the end or
-- from a position. Each item at a position goes before the ones after it,
-- so they go in reverse. The songs of a playlist are loaded from it in runs
-- of its positions.
addItems :: Location -> [(Int, BrowserItem)] -> Maybe Position -> Command ()
addItems location items pos = traverse_ ($ pos) $ case pos of
  Nothing -> commands
  Just _ -> reverse commands
  where
    commands :: [Maybe Position -> Command ()]
    commands = case location of
      InPlaylist name ->
        [ load name (Just $ Range (SongPos a) (Just (SongPos (b + 1))))
        | -- The playlist's songs come after @..@.
        (a, b) <- runs [i - 1 | (i, EntryItem _) <- items]
        ]
      InDirectory _ ->
        [ case entry of
            DirectoryEntry d -> add d.path
            SongEntry song -> add song.file
            PlaylistEntry p -> load p.path Nothing
        | (_, EntryItem entry) <- items
        ]

-- | What a save saves: the marked items.
browserToSave :: AppState -> SaveSource
browserToSave s =
  SaveItems
    [ case entry of
        SongEntry song -> songToSave song
        DirectoryEntry d -> SaveDirectory d.path
        PlaylistEntry p -> SavePlaylist p.path
    | (_, EntryItem entry) <- markedItems s
    ]

-- | The ids of a song's copies in the queue.
queuedIds :: Song -> AppState -> [SongId]
queuedIds song s =
  [ i
  | queued <- toList s.mirror.queue
  , sameSong queued song
  , Just i <- [queued.songId]
  ]

-- | The items that an action applies to, in their order: the selected
-- items, or the item under the cursor without a selection. @..@ is never
-- one.
markedItems :: AppState -> [(Int, BrowserItem)]
markedItems s =
  filter ((/= ParentItem) . snd) $
    case selectedPositions (Just . itemKey <$> s.browser.items) s.browser.selection of
      [] -> toList $ cursorItem s
      ps -> [(i, item) | i <- ps, Just item <- [Seq.lookup i s.browser.items]]

cursorItem :: AppState -> Maybe (Int, BrowserItem)
cursorItem s =
  let c = (focusedView s).cursor
  in (c,) <$> Seq.lookup c s.browser.items

-- | What the status bar says after an add.
addedText :: AppEnv -> [(Int, BrowserItem)] -> T.Text
addedText env = \case
  [(_, EntryItem (SongEntry song))] -> "Added: " <> songText env song
  [(_, EntryItem (DirectoryEntry d))] -> "Added /" <> d.path
  [(_, EntryItem (PlaylistEntry p))] -> "Loaded " <> p.path
  items -> "Added " <> countItems (length items)

-- | A song as the status bar shows it.
songText :: AppEnv -> Song -> T.Text
songText env song =
  spansText $
    renderFormat (plainContext env.config.lists) song env.config.statusBar.song

-- | The directory to update in the database for "update current": the one
-- that the browser lists, or the one that its playlist is in. Nothing is the
-- whole database.
browserDirectory :: AppState -> Maybe T.Text
browserDirectory s = case s.browser.location of
  Just (InDirectory "") -> Nothing
  Just (InDirectory path) -> Just path
  Just location@(InPlaylist _) -> case parentOf location of
    Just (InDirectory path) | not (T.null path) -> Just path
    _ -> Nothing
  Nothing -> Nothing

----------------------------------------
-- Sorting

-- | Sort the entries by the next sort mode, with the cursor on the same item.
nextSortMode :: App es => Eff es ()
nextSortMode = do
  modifyS $ #toggles % #browserSort %~ cycleNext
  env <- getAppEnv
  s <- getS
  forM_ s.browser.location $ \location -> do
    let current = Seq.lookup (fst (screenPosition BrowserScreen s)) s.browser.items
        items = arrange env s.toggles.browserSort location s.browser.entries
    modifyS $
      (#browser % #items .~ items)
        . (#browser % #rows .~ itemRows env s.toggles.browserDisplay items)
    modifyWithEnv . placeCursor $ maybe AtTop (StayOn . itemKey) current
  showMessage $ "Sort: " <> sortByName s.toggles.browserSort

-- | The items of a listing: @..@ unless at the root, then the entries. The
-- songs of a playlist stay in its order.
arrange :: AppEnv -> SortBy -> Location -> [Entry] -> Seq.Seq BrowserItem
arrange env by location entries =
  Seq.fromList $ [ParentItem | location /= InDirectory ""] <> map EntryItem sorted
  where
    sorted :: [Entry]
    sorted = case location of
      InPlaylist _ -> entries
      InDirectory _ -> sortEntries env by entries

-- | Entries in the order of a sort mode, as in ncmpcpp. Except without a
-- sort, directories come first, then songs, then playlists.
sortEntries :: AppEnv -> SortBy -> [Entry] -> [Entry]
sortEntries env by entries = case by of
  SortByNone -> entries
  _ -> L.sortOn (\e -> (kind e, key e)) entries
  where
    kind :: Entry -> Int
    kind = \case
      DirectoryEntry _ -> 0
      SongEntry _ -> 1
      PlaylistEntry _ -> 2

    key :: Entry -> SortKey
    key e = case by of
      SortByName -> ByText (collated (name e))
      SortByMtime -> ByTime . Down $ case e of
        DirectoryEntry d -> d.lastModified
        SongEntry song -> song.lastModified
        PlaylistEntry p -> p.lastModified
      SortByFormat -> ByText . collated $ case e of
        SongEntry song -> renderPlain (plainContext env.config.lists) song env.config.browser.sort.format
        _ -> name e
      _ -> NoKey

    name :: Entry -> T.Text
    name =
      baseName . \case
        DirectoryEntry d -> d.path
        SongEntry song -> song.file
        PlaylistEntry p -> p.path

    collated :: T.Text -> CollationKey
    collated = collationKey env.collator env.config.lists.ignoreLeadingThe

-- | What entries of one kind sort by. 'L.sortOn' computes it once for each.
data SortKey
  = NoKey
  | ByText CollationKey
  | ByTime (Down (Maybe UTCTime))
  deriving stock (Eq, Ord)

----------------------------------------
-- Helpers

-- | Put the browser's cursor where a listing says, also while the view shows
-- another screen.
placeCursor :: ListingCursor -> AppEnv -> AppState -> AppState
placeCursor cursor env s = case cursor of
  AtTop -> setScreenPosition BrowserScreen (0, 0) env s
  JumpTo k -> case indexOf k of
    Just i -> jumpScreenTo BrowserScreen i env s
    Nothing -> setScreenPosition BrowserScreen (0, 0) env s
  StayOn k ->
    let (c, o) = screenPosition BrowserScreen s
    in setScreenPosition BrowserScreen (fromMaybe c (indexOf k), o) env s
  where
    indexOf :: ItemKey -> Maybe Int
    indexOf k = Seq.findIndexL ((== k) . itemKey) s.browser.items

itemKey :: BrowserItem -> ItemKey
itemKey = \case
  ParentItem -> ParentKey
  EntryItem (DirectoryEntry d) -> DirectoryKey d.path
  EntryItem (SongEntry song) -> SongKey song.file song.range
  EntryItem (PlaylistEntry p) -> PlaylistKey p.path
