-- | The browser screen: the directories of MPD's database and the playlists,
-- which the user moves through and adds songs from.
module Reprise.Screen.Browser
  ( -- * Drawing
    browserView

    -- * For the handlers
  , browserOps

    -- * Listing
  , relistBrowser
  , browserChanged
  , browserListed
  , browserFailed
  , decodersFetched
  , localListed
  , locateSong

    -- * Adding local files
  , localWalked
  , localAdded
  , localAddFailed

    -- * Updating
  , updateDatabase
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
import Effectful.Input.Static
import Effectful.State.Static.Local
import Graphics.Vty qualified as V
import Optics.Core
import System.FilePath

import Reprise.Action
import Reprise.Collation
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Exception
import Reprise.Find
import Reprise.Format
import Reprise.Groups
import Reprise.Handler.Core
import Reprise.LocalFiles
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Types
import Reprise.Save
import Reprise.Selection
import Reprise.State
import Reprise.Style
import Reprise.TagLib
import Reprise.UI.SongList

----------------------------------------
-- Drawing

browserView :: AppEnv -> AppState -> View -> V.Image
browserView env s v =
  let ctx = rowContext env s.toggles.browserDisplay v.width
      titles = titleRow env s v ctx
      visible = visibleItems env s v s.browser.items
      found = typedMatches s $ visibleItems env s v s.browser.rows
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
        (plainContext env.config.songs)
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
itemRows env display = fmap $ \item -> case rowContent env item of
  SongRow song -> songFindText env display song
  OtherRow spans -> foldText (spansText spans)

----------------------------------------
-- Verbs

browserOps
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => ScreenOps es
browserOps =
  ScreenOps
    { verb = browserVerb
    , rows = Just $ getsS (.browser.rows)
    , shown = switchTo BrowserScreen >> openBrowser
    }

-- | How the browser does a verb, if it does it.
browserVerb
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Action
  -> Maybe (Eff es ())
browserVerb = \case
  Move t -> Just $ modifyWithEnv (moveListCursor t)
  JumpToPlaying ->
    Just $
      getsS (currentSong . (.mirror)) >>= \case
        Nothing -> noCurrentSong
        Just song -> void $ locateSong song
  Activate -> Just activateItem
  Parent -> Just leave
  Save ->
    Just $
      getS >>= \s ->
        -- MPD saves only the directories of its database.
        if maybe False isLocal s.browser.location
          && not (null [() | (_, EntryItem (DirectoryEntry _)) <- markedItems s])
          then showMessage "A local directory can't be saved as a playlist"
          else askSaveName (browserToSave s)
  NextSortMode -> Just nextSortMode
  Add p -> Just $ addMarked p
  AddAndPlay -> Just addAndPlay
  AddOrRemove -> Just addOrRemove
  Select t -> Just $ selectInBrowser t
  Toggle ToggleDisplay -> Just toggleBrowserDisplay
  Update UpdateCurrent ->
    Just $
      getsS (.browser.location) >>= \case
        Just (InLocal _) -> showMessage "Local files aren't in MPD's database"
        _ -> updateDatabase =<< getsS browserDirectory
  ChangeBrowseMode -> Just changeBrowseMode
  _ -> Nothing

-- | Show the songs in the other display, which finds match too.
toggleBrowserDisplay
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Eff es ()
toggleBrowserDisplay = do
  toggleDisplay #browserDisplay
  modifyWithEnv $ \env s -> setItems s.browser.items env s

-- | Set the items that the browser lists, with their text as finds match
-- it, which is of the display.
setItems :: Seq.Seq BrowserItem -> AppEnv -> AppState -> AppState
setItems items env s =
  s
    & #browser % #items .~ items
    & #browser % #rows .~ itemRows env s.toggles.browserDisplay items

----------------------------------------
-- Listing

-- | List the root the first time the browser shows.
openBrowser :: (State AppState :> es, MpdRequest :> es, UiRequest :> es) => Eff es ()
openBrowser = do
  location <- getsS (.browser.location)
  case location of
    Nothing -> list (InDirectory "") AtTop
    Just _ -> pure ()

-- | List again what the browser lists, with the cursor on the same item,
-- e.g. after a new connection. A listing on its way is requested again, as
-- its reply may be lost or out of date. A browser that never showed lists
-- nothing.
relistBrowser :: (State AppState :> es, MpdRequest :> es, UiRequest :> es) => Eff es ()
relistBrowser = do
  s <- getS
  case (s.browser.listing, s.browser.location) of
    (Just l, _) -> list l.location l.cursor
    (Nothing, Just location) -> list location (stayOnCursor s)
    (Nothing, Nothing) -> pure ()

-- | List again after a change of the database, or of the stored playlists,
-- which show at the root and open as playlists. Neither changes a local
-- directory.
browserChanged
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => [Subsystem]
  -> Eff es ()
browserChanged subsystems = do
  location <- getsS (.browser.location)
  let (database, storedPlaylists) = case location of
        Just (InDirectory "") -> (True, True)
        Just (InDirectory _) -> (True, False)
        Just (InPlaylist _) -> (True, True)
        Just (InLocal _) -> (False, False)
        Nothing -> (False, False)
  when
    ( (DatabaseSubsystem `elem` subsystems && database)
        || (StoredPlaylistSubsystem `elem` subsystems && storedPlaylists)
    )
    relistBrowser

-- | Go up to the directory of what the browser lists, with the cursor on
-- where it came from. It goes up from a listing on its way, too, so that
-- keys typed ahead of a reply add up.
leave :: (State AppState :> es, MpdRequest :> es, UiRequest :> es) => Eff es ()
leave = do
  b <- getsS (.browser)
  forM_ (maybe b.location (Just . (.location)) b.listing) goUp

-- | Go up to the directory of a directory or a playlist, with the cursor on
-- it.
goUp :: (State AppState :> es, MpdRequest :> es, UiRequest :> es) => Location -> Eff es ()
goUp location =
  forM_ (parentOf location) $ \up -> list up . JumpTo $ case location of
    InDirectory path -> DirectoryKey path
    InPlaylist path -> PlaylistKey path
    InLocal path -> DirectoryKey path

-- | List the directory of a song, with the cursor on the song, as ncmpcpp's
-- @jump_to_browser@ does. A stream isn't in the database, and fails.
locateSong
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Song
  -> Eff es Bool
locateSong song
  | isStream song = do
      showError "The song isn't in MPD's database"
      pure False
  | otherwise = do
      -- MPD keeps a local file in the queue by its absolute path.
      let directory
            | "/" `T.isPrefixOf` song.file =
                InLocal . T.pack $ takeDirectory (T.unpack song.file)
            | otherwise = InDirectory (directoryOf song.file)
      list directory (JumpTo (SongKey song.file song.range))
      pure True

-- | The directory that a directory or a playlist is in.
parentOf :: Location -> Maybe Location
parentOf = \case
  InDirectory "" -> Nothing
  InDirectory path -> Just $ InDirectory (directoryOf path)
  InPlaylist path -> Just $ InDirectory (directoryOf path)
  InLocal "/" -> Nothing
  InLocal path -> Just . InLocal . T.pack $ takeDirectory (T.unpack path)

-- | List a directory or a playlist. A local directory lists once MPD said
-- which files it decodes. A listing in the other browse mode keeps what
-- the browser listed, for a change of the mode back.
list
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Location
  -> ListingCursor
  -> Eff es ()
list location cursor = do
  token <- newToken
  modifyS $ \s ->
    -- A listing on its way keeps where it puts the cursor, as the screen
    -- still shows the one before it.
    let current = case (s.browser.listing, s.browser.location) of
          (Just l, _) -> Just (l.location, l.cursor)
          (Nothing, Just shown) -> Just (shown, returnToCursor s)
          (Nothing, Nothing) -> Nothing
    in s
         & #browser % #listing ?~ Listing token location cursor
         & #browser % #otherMode %~ \other -> case current of
           Just c | isLocal (fst c) /= isLocal location -> Just c
           _ -> other
  case location of
    InDirectory path -> requestOr (lsInfo path) (BrowserFailed token) (BrowserListed token)
    InPlaylist name ->
      requestOr
        (map SongEntry <$> listPlaylistInfo name)
        (BrowserFailed token)
        (BrowserListed token)
    InLocal dir ->
      getsS (.browser.suffixes) >>= \case
        Just suffixes -> listLocal token suffixes (T.unpack dir)
        Nothing -> requestOr decoders (BrowserFailed token) (DecodersFetched token)

-- | Keep the suffixes of MPD's decoders, then list the local directory that
-- waited for them.
decodersFetched
  :: (State AppState :> es, UiRequest :> es) => Int -> [Decoder] -> Eff es ()
decodersFetched token ds = do
  let suffixes = suffixesOf ds
  modifyS $ #browser % #suffixes ?~ suffixes
  whenCurrent (.browser.listing) (.token) token $ \l -> case l.location of
    InLocal dir -> listLocal token suffixes (T.unpack dir)
    _ -> pure ()

-- | Show the directories and the music files of a local directory. Hidden
-- entries are left out, as ncmpcpp does by default.
localListed
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Int
  -> Either ListingError [LocalFile]
  -> Eff es ()
localListed token result = whenCurrent (.browser.listing) (.token) token $ \l ->
  case (result, l.location) of
    (Left DirectoryMissing, location) | Just up <- parentOf location -> list up AtTop
    (Left err, _) -> do
      modifyS $ #browser % #listing .~ Nothing
      showError $ case err of
        DirectoryMissing -> "The directory is gone"
        DirectoryUnreadable why -> why
    (Right files, InLocal dir) -> do
      suffixes <- getsS (.browser.suffixes)
      let visible = filter (not . ("." `T.isPrefixOf`) . (.name)) files
          path :: LocalFile -> T.Text
          path f = T.pack (T.unpack dir </> T.unpack f.name)
      browserListed token $
        [DirectoryEntry (Directory (path f) (Just f.lastModified)) | f <- visible, f.isDirectory]
          <> [ SongEntry (localSong (path f) f)
             | f <- visible
             , not f.isDirectory
             , Just s <- [suffixes]
             , isMusic s f.name f.tags
             ]
    (Right _, _) -> pure ()

-- | The song of a local file, with the tags that TagLib read.
localSong :: T.Text -> LocalFile -> Song
localSong path f =
  Song
    { file = path
    , tags = maybe M.empty mpdTags f.tags
    , duration = fromIntegral <$> (mfilter (> 0) . (.seconds) =<< f.tags)
    , range = Nothing
    , lastModified = Just f.lastModified
    , format = Nothing
    , position = Nothing
    , songId = Nothing
    , priority = 0
    }

-- | Switch between MPD's database and local files, and list what the other
-- mode listed last: at first the root, or the home directory.
changeBrowseMode
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Eff es ()
changeBrowseMode = do
  env <- getAppEnv
  b <- getsS (.browser)
  let local = maybe False isLocal $ maybe b.location (Just . (.location)) b.listing
      other = if local then InDirectory "" else InLocal (T.pack env.home)
  -- After a listing of the other mode failed, the place that it kept for
  -- a change back is of this mode.
  uncurry list . fromMaybe (other, AtTop) $
    mfilter ((/= local) . isLocal . fst) b.otherMode

-- | Go up from a directory or a playlist that is gone, until one exists, as
-- ncmpcpp does. Another error shows, and the browser stays as it was. A
-- listing that a lost connection failed stays, so that the next connection
-- lists it again.
browserFailed
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Int
  -> MpdError
  -> Eff es ()
browserFailed token err = whenCurrent (.browser.listing) (.token) token $ \l ->
  case (err, parentOf l.location) of
    (AckError ack, Just up) | ack.code == AckNoExist -> list up AtTop
    (ConnectionError _, _) -> showError $ exceptionText err
    _ -> do
      modifyS $ #browser % #listing .~ Nothing
      showError $ exceptionText err

-- | Show the entries of the latest listing. The reply to a listing that a
-- newer one replaced changes nothing. The selection stays in a listing of
-- the same, without the items that are gone.
browserListed
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Int
  -> [Entry]
  -> Eff es ()
browserListed token entries = whenCurrent (.browser.listing) (.token) token $ \l -> do
  env <- getAppEnv
  s <- getS
  let items = arrange env s.toggles.browserSort l.location entries
      same = s.browser.location == Just l.location
      selection
        | same = restrictTo (S.fromList (map itemKey (toList items))) s.browser.selection
        | otherwise = noSelection
  modifyS $ \st ->
    st
      & #browser % #location ?~ l.location
      & #browser % #entries .~ entries
      & #browser % #selection .~ selection
      & #browser % #listing .~ Nothing
  modifyWithEnv $ setItems items
  modifyWithEnv $ placeCursor l.cursor

----------------------------------------
-- Selection

selectInBrowser
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => SelectTarget
  -> Eff es ()
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
activateItem
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Eff es ()
activateItem = do
  s <- getS
  forM_ (cursorItem s) $ \case
    (_, ParentItem) -> forM_ s.browser.location goUp
    (_, EntryItem (DirectoryEntry d)) -> list (directoryIn s.browser.location d.path) AtTop
    (_, EntryItem (PlaylistEntry p)) -> list (InPlaylist p.path) AtTop
    item@(_, EntryItem (SongEntry _)) -> addAndPlayItems [item]

-- | Add the marked items to the queue.
addMarked
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => AddPosition
  -> Eff es ()
addMarked p = do
  s <- getS
  forM_ s.browser.location $ \location -> case markedItems s of
    [] -> pure ()
    items
      | isLocal location -> addLocal items (positionOf s p) False
      | otherwise -> do
          mutate . addItems location items $ positionOf s p
          env <- getAppEnv
          showMessage $ addedText env items

-- | Add the marked items and play the first. A single song that is already
-- in the queue plays there instead.
addAndPlay
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Eff es ()
addAndPlay = getsS markedItems >>= addAndPlayItems

addAndPlayItems
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => [(Int, BrowserItem)]
  -> Eff es ()
addAndPlayItems items = do
  s <- getS
  forM_ s.browser.location $ \location -> case items of
    [] -> pure ()
    [(_, EntryItem (SongEntry song))] | i : _ <- queuedIds song s -> mutate $ playId i
    _ | isLocal location -> addLocal items Nothing True
    _ -> do
      -- In one command list, which no other client's command interrupts, so
      -- the songs start at the length even if the mirror missed a change.
      let n = SongPos (queueLength s.mirror)
      mutate $ addItems location items (Just (At n)) *> play (Just n)

-- | Add the item under the cursor, or remove its song if it is in the
-- queue, then move down.
addOrRemove
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Eff es ()
addOrRemove = do
  s <- getS
  env <- getAppEnv
  forM_ ((,) <$> s.browser.location <*> cursorItem s) $ \case
    (_, (_, ParentItem)) -> pure ()
    (_, (_, EntryItem (SongEntry song))) | ids@(_ : _) <- queuedIds song s -> do
      mutate $ traverse_ deleteId ids
      showMessage $ "Removed: " <> songText env song
    (location, item)
      | isLocal location -> addLocal [item] Nothing False
      | otherwise -> do
          mutate $ addItems location [item] Nothing
          showMessage $ addedText env [item]
  modifyWithEnv $ moveListCursor MoveDown

-- | Where an add goes, or 'Nothing' for the end. The cursor of the queue
-- is on no song in an empty queue, and may be past the end of a queue that
-- shrank.
positionOf :: AppState -> AddPosition -> Maybe Position
positionOf s = \case
  AddEnd -> Nothing
  AddNext -> Just (AfterCurrent 0)
  AddBeginning -> Just (At 0)
  AddCursor ->
    Just . At . SongPos . min (queueLength s.mirror) . (+ 1) . fst $
      screenPosition QueueScreen s

-- | Add local items after the walk of their directories, as MPD doesn't add
-- a local directory, and play from the first, for add and play. Songs
-- alone go the same way, so that a file that MPD can't read leaves out only
-- itself. An add waits for the adds before it, so that the songs go in in
-- the order of the keys.
addLocal
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => [(Int, BrowserItem)]
  -> Maybe Position
  -> Bool
  -> Eff es ()
addLocal items p andPlay = do
  token <- newToken
  s <- getS
  let entries = [e | (_, EntryItem e) <- items]
      -- The adds before it add their songs first, and the mirror may have
      -- some of them already.
      queued = queueLength s.mirror - sum ((.added) <$> s.browser.localAdds)
      start =
        modifyS $
          #browser
            % #localAdds
            %~ ( Seq.|>
                   LocalAdd
                     { token = token
                     , entries = entries
                     , position = p
                     , playFrom = if andPlay then Just (SongPos queued) else Nothing
                     , files = Nothing
                     , added = 0
                     , skipped = 0
                     }
               )
  case ([T.unpack d.path | DirectoryEntry d <- entries], s.browser.suffixes) of
    ([], _) -> start >> localWalked token []
    (dirs, Just suffixes) -> start >> walkLocal token suffixes dirs
    -- A new connection asks for them again.
    (_, Nothing) -> showError "MPD's decoders aren't known yet"

-- | Add the songs and the music files of the walked directories, in the
-- order of the items, once the adds before them are done.
localWalked
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Int
  -> [[FilePath]]
  -> Eff es ()
localWalked token walkedFiles = do
  modifyS $
    #browser % #localAdds % mapped %~ \la ->
      if la.token == token then walked la else la
  whenCurrent firstLocalAdd (.token) token sendLocalAdd
  where
    walked :: LocalAdd -> LocalAdd
    walked la =
      let files = merge la.entries walkedFiles
      in la & #files ?~ maybe files (const (reverse files)) la.position

    merge :: [Entry] -> [[FilePath]] -> [T.Text]
    merge entries ws = case (entries, ws) of
      (DirectoryEntry _ : es, w : rest) -> map T.pack w <> merge es rest
      (SongEntry s : es, _) -> s.file : merge es ws
      (_ : es, _) -> merge es ws
      ([], _) -> []

-- | The add that MPD adds now.
firstLocalAdd :: AppState -> Maybe LocalAdd
firstLocalAdd s = Seq.lookup 0 s.browser.localAdds

-- | Send the adds of the files that are left of the first add, which go in
-- at a position in reverse, as each goes before the ones after it. An add
-- whose walk didn't end yet is sent when it ends.
sendLocalAdd
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => LocalAdd
  -> Eff es ()
sendLocalAdd la = case la.files of
  Nothing -> pure ()
  Just [] -> finishLocalAdd la
  Just files -> do
    modifyS $ #browser % #localAdds % ix 0 .~ la
    requestOr
      (traverse_ (`add` la.position) files)
      (LocalAddFailed la.token)
      (const (LocalAdded la.token))

localAdded
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Int
  -> Eff es ()
localAdded token = whenCurrent firstLocalAdd (.token) token $ \la ->
  finishLocalAdd $ la & #added %~ (+ maybe 0 length la.files) & #files ?~ []

-- | A file that MPD can't read is left out, and the adds after it are sent
-- again. Another error ends the add.
localAddFailed
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Int
  -> MpdError
  -> Eff es ()
localAddFailed token err = whenCurrent firstLocalAdd (.token) token $ \la -> case err of
  AckError ack
    | ack.code == AckNoExist ->
        sendLocalAdd $
          la
            & #added %~ (+ ack.index)
            & #skipped %~ (+ 1)
            & #files % _Just %~ drop (ack.index + 1)
  _ -> do
    showError $ exceptionText err
    nextLocalAdd la

finishLocalAdd
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => LocalAdd
  -> Eff es ()
finishLocalAdd la = do
  forM_ la.playFrom $ \from -> when (la.added > 0) . mutate $ play (Just from)
  env <- getAppEnv
  showMessage $ case (la.entries, la.skipped) of
    ([SongEntry song], 0) -> "Added: " <> songText env song
    _ ->
      "Added "
        <> countSongs la.added
        <> if la.skipped > 0
          then ", left out " <> T.pack (show la.skipped) <> " that MPD can't read"
          else ""
  nextLocalAdd la

-- | Drop the first add and send the next one. Its songs go in after the
-- ones that the first added.
nextLocalAdd
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => LocalAdd
  -> Eff es ()
nextLocalAdd la = do
  modifyS $
    #browser
      % #localAdds
      %~ fmap (#playFrom % _Just %~ (+ fromIntegral la.added))
      . Seq.drop 1
  getsS firstLocalAdd >>= traverse_ sendLocalAdd

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
        [ load name (Just (runRange run))
        | -- The playlist's songs come after @..@.
        run <- runs [i - 1 | (i, EntryItem _) <- items]
        ]
      InDirectory _ ->
        [ case entry of
            DirectoryEntry d -> add d.path
            SongEntry song -> add song.file
            PlaylistEntry p -> load p.path Nothing
        | (_, EntryItem entry) <- items
        ]
      InLocal _ -> [add song.file | (_, EntryItem (SongEntry song)) <- items]

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

-- | The items that an action applies to, in their order. @..@ is never one.
markedItems :: AppState -> [(Int, BrowserItem)]
markedItems s =
  [ (i, item)
  | i <-
      marked (Just . itemKey <$> s.browser.items) s.browser.selection (focusedView s).cursor
  , Just item <- [Seq.lookup i s.browser.items]
  , item /= ParentItem
  ]

-- | The item under the browser's cursor, with its index, also while the view
-- shows another screen.
cursorItem :: AppState -> Maybe (Int, BrowserItem)
cursorItem s =
  let c = fst (screenPosition BrowserScreen s)
  in (c,) <$> Seq.lookup c s.browser.items

-- | Keep the cursor on its item in a new order or listing of the same.
stayOnCursor :: AppState -> ListingCursor
stayOnCursor = maybe AtTop (StayOn . itemKey . snd) . cursorItem

-- | Put the cursor back on its item, with the list scrolled as it is.
returnToCursor :: AppState -> ListingCursor
returnToCursor s = case cursorItem s of
  Just (_, item) -> ReturnTo (itemKey item) (snd (screenPosition BrowserScreen s))
  Nothing -> AtTop

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
    renderFormat (plainContext env.config.songs) song env.config.statusBar.song

-- | Update a directory in the database, or with Nothing the whole database.
updateDatabase
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Maybe T.Text
  -> Eff es ()
updateDatabase path = do
  mutate . void $ update path
  showMessage $ "Updating " <> maybe "the database" ("/" <>) path

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
  Just (InLocal _) -> Nothing
  Nothing -> Nothing

-- | A directory in the browse mode of a location.
directoryIn :: Maybe Location -> T.Text -> Location
directoryIn = \case
  Just (InLocal _) -> InLocal
  _ -> InDirectory

isLocal :: Location -> Bool
isLocal = \case
  InLocal _ -> True
  _ -> False

----------------------------------------
-- Sorting

-- | Sort the entries by the next sort mode, with the cursor on the same item.
nextSortMode :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es) => Eff es ()
nextSortMode = do
  modifyS $ #toggles % #browserSort %~ cycleNext
  env <- getAppEnv
  s <- getS
  forM_ s.browser.location $ \location -> do
    modifyWithEnv . setItems $ arrange env s.toggles.browserSort location s.browser.entries
    modifyWithEnv . placeCursor $ stayOnCursor s
  showMessage $ "Sort: " <> sortByName s.toggles.browserSort

-- | The items of a listing: @..@ unless at a root, then the entries. The
-- songs of a playlist stay in its order.
arrange :: AppEnv -> SortBy -> Location -> [Entry] -> Seq.Seq BrowserItem
arrange env by location entries =
  Seq.fromList $ [ParentItem | isJust (parentOf location)] <> map EntryItem sorted
  where
    sorted :: [Entry]
    sorted = case location of
      InPlaylist _ -> entries
      InDirectory _ -> sortEntries env by entries
      InLocal _ -> sortEntries env by entries

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
        SongEntry song -> renderPlain (plainContext env.config.songs) song env.config.browser.sort.format
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
  ReturnTo k o -> case indexOf k of
    Just i -> setScreenPosition BrowserScreen (i, o) env s
    Nothing -> setScreenPosition BrowserScreen (0, 0) env s
  where
    indexOf :: ItemKey -> Maybe Int
    indexOf k = Seq.findIndexL ((== k) . itemKey) s.browser.items

itemKey :: BrowserItem -> ItemKey
itemKey = \case
  ParentItem -> ParentKey
  EntryItem (DirectoryEntry d) -> DirectoryKey d.path
  EntryItem (SongEntry song) -> SongKey song.file song.range
  EntryItem (PlaylistEntry p) -> PlaylistKey p.path
