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

    -- * Renaming
  , renameTo
  , renameByFormat
  , contentsListed
  , localContentsListed
  , renameIn
  , mountsListed
  , directoriesRenamed
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
import Data.Void
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
import Reprise.File
import Reprise.Find
import Reprise.Format
import Reprise.Groups
import Reprise.Handler.Core
import Reprise.LineEdit
import Reprise.LocalFiles
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Command hiding (currentSong)
import Reprise.Mpd.Protocol.Connection hiding (run)
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
  Rename -> Just askRename
  RenameByFormat -> Just askRenameByFormat
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
      browserListed token $ localEntries suffixes dir files
    (Right _, _) -> pure ()

-- | The entries of the files of a local directory: its directories and its
-- music files. Hidden files are left out, as ncmpcpp does by default.
localEntries :: Maybe Suffixes -> T.Text -> [LocalFile] -> [Entry]
localEntries suffixes dir files =
  [DirectoryEntry (Directory (path f) (Just f.lastModified)) | f <- visible, f.isDirectory]
    <> [ SongEntry (localSong (path f) f)
       | f <- visible
       , not f.isDirectory
       , Just s <- [suffixes]
       , isMusic s f.name f.tags
       ]
  where
    visible :: [LocalFile]
    visible = filter (not . ("." `T.isPrefixOf`) . (.name)) files

    path :: LocalFile -> T.Text
    path f = T.pack (T.unpack dir </> T.unpack f.name)

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
      switch =
        uncurry list . fromMaybe (other, AtTop) $
          mfilter ((/= local) . isLocal . fst) b.otherMode
  if local
    then switch
    else whenOnSocket "Local files need a connection to MPD's socket" switch

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
  landOn <- getsS (.browser.landOn)
  forM_ landOn $ \(location, k) ->
    when (location == l.location && any ((== k) . itemKey) items) $ do
      modifyS $ #browser % #landOn .~ Nothing
      modifyWithEnv $ placeCursor (JumpTo k)

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
  env <- getAppEnv
  forM_ s.browser.location $ \location -> case markedItems s of
    [] -> pure ()
    items
      | isLocal location -> addLocal items (addPosition s p) False
      | otherwise -> addAt p (addItems location items) (addedText env items)

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
  env <- getAppEnv
  let single = case items of
        [(_, EntryItem (SongEntry song))] -> Just song
        _ -> Nothing
  forM_ s.browser.location $ \location -> case items of
    [] -> pure ()
    _
      | isLocal location && all (null . (`queuedIds` s)) single ->
          addLocal items Nothing True
    _ -> addAndPlayAt single (addItems location items) (addedText env items)

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
          mutate . addCommands Nothing $ addItems location [item]
          showMessage $ addedText env [item]
  modifyWithEnv $ moveListCursor MoveDown

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

-- | The commands that add items to the queue, each at a position. The songs
-- of a playlist are loaded from it in runs of its positions.
addItems :: Location -> [(Int, BrowserItem)] -> [Maybe Position -> Command ()]
addItems location items = case location of
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
addedText env items = case traverse song items of
  Just songs -> addedSongs env songs
  Nothing -> case items of
    [(_, EntryItem (DirectoryEntry d))] -> "Added /" <> d.path
    [(_, EntryItem (PlaylistEntry p))] -> "Loaded " <> p.path
    _ -> "Added " <> countItems (length items)
  where
    song :: (Int, BrowserItem) -> Maybe Song
    song = \case
      (_, EntryItem (SongEntry s)) -> Just s
      _ -> Nothing

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

-- | Run an action on a connection to MPD's socket, where MPD plays local
-- files and shows the paths of its storages, else show the error of a
-- connection over TCP, or that there is none.
whenOnSocket
  :: (State AppState :> es, UiRequest :> es) => T.Text -> Eff es () -> Eff es ()
whenOnSocket overTcp action =
  getsS (.connection) >>= \case
    Connected _ (UnixAddress _) -> action
    Connected _ _ -> showError overTcp
    _ -> showError "Not connected to MPD"

----------------------------------------
-- Renaming

-- | Ask for a new name of the directory under the cursor, starting from its
-- name. The selection stays out of it, as one name is for one directory.
askRename :: (State AppState :> es, UiRequest :> es) => Eff es ()
askRename = do
  s <- getS
  case (s.browser.location, cursorItem s) of
    (Just location, Just (_, EntryItem (DirectoryEntry d))) ->
      whenRenamable location
        $ modifyS
        $ openLine
          "Rename to: "
          (LineEdit (T.pack (takeFileName (T.unpack d.path))) "")
          (ForRename location d.path)
    _ -> showMessage "There is no directory under the cursor"

-- | Rename a directory of a listing, by its path there, to a name that the
-- user typed. A name that isn't one of a file is refused, also when the
-- rule of the names that reprise makes leaves out one of its characters.
-- The same name renames nothing.
renameTo
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Location
  -> T.Text
  -> T.Text
  -> Eff es ()
renameTo location path name = case nameFault name of
  Just fault -> showError $ "A name " <> fault
  Nothing
    | new == path -> pure ()
    | otherwise -> renameIn location [(path, new)]
  where
    new :: T.Text
    new = renamedPath path name

-- | Why a name of a directory is refused, if it is, e.g. "can't be empty".
nameFault :: T.Text -> Maybe T.Text
nameFault name
  | T.null name = Just "can't be empty"
  | name `elem` [".", ".."] = Just $ "can't be " <> name
  | Just c <- T.find (`elem` forbiddenInNames) name =
      Just $ "can't contain " <> T.singleton c
  | otherwise = Nothing

-- | The path of a directory after a rename to a name.
renamedPath :: T.Text -> T.Text -> T.Text
renamedPath path name = T.pack $ replaceFileName (T.unpack path) (T.unpack name)

-- | Ask for the format to rename the marked directories by, starting from
-- @browser.rename_format@.
askRenameByFormat
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Eff es ()
askRenameByFormat = do
  s <- getS
  env <- getAppEnv
  let items = markedItems s
      directories = [d.path | (_, EntryItem (DirectoryEntry d)) <- items]
      selecting =
        not . null $
          selectedPositions (Just . itemKey <$> s.browser.items) s.browser.selection
  case s.browser.location of
    Just location
      | not (null directories) && length directories == length items ->
          whenRenamable location
            $ modifyS
            $ openLine
              "Rename by format: "
              (LineEdit (printFormat absurd env.config.browser.renameFormat) "")
              (ForRenameByFormat location directories)
    _
      | selecting -> showMessage "The selection holds more than directories"
      | otherwise -> showMessage "There is no directory under the cursor"

-- | List the contents of directories of a listing, by their paths there,
-- to rename them by a format that the user typed.
renameByFormat
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Location
  -> [T.Text]
  -> T.Text
  -> Eff es ()
renameByFormat location paths text = case parsePlainFormat text of
  Left err ->
    showError $
      "At character " <> T.pack (show (err.position + 1)) <> " of the format: " <> err.message
  Right format -> do
    token <- newToken
    let start =
          modifyS $
            #browser
              % #formatRename
              ?~ FormatRename {token = token, location = location, format = format, paths = paths}
    case location of
      InLocal _ ->
        getsS (.browser.suffixes) >>= \case
          Just suffixes -> start >> listLocalContents token suffixes (map T.unpack paths)
          Nothing -> showError "MPD's decoders aren't known yet"
      _ -> do
        start
        requestOr
          (traverse lsInfo paths)
          (ContentsListed token . Left . exceptionText)
          (ContentsListed token . Right)

-- | Ask to rename the directories of MPD's database by their entries.
contentsListed
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Int
  -> Either T.Text [[Entry]]
  -> Eff es ()
contentsListed token result = whenCurrent (.browser.formatRename) (.token) token $ \r -> do
  modifyS $ #browser % #formatRename .~ Nothing
  either showError (askRenameTo r . zip r.paths) result

-- | Ask to rename the local directories by their files.
localContentsListed
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Int
  -> [Either ListingError [LocalFile]]
  -> Eff es ()
localContentsListed token results = whenCurrent (.browser.formatRename) (.token) token $ \r -> do
  modifyS $ #browser % #formatRename .~ Nothing
  suffixes <- getsS (.browser.suffixes)
  let listed = zip r.paths results
  case [(path, err) | (path, Left err) <- listed] of
    (path, err) : _ -> showError $ case err of
      DirectoryMissing -> baseName path <> " is gone"
      DirectoryUnreadable why -> why
    [] ->
      askRenameTo r [(path, localEntries suffixes path files) | (path, Right files) <- listed]

-- | Ask to rename directories by their entries, unless a directory can't
-- have the name of the format, or two would have the same name. A
-- directory that has the name already stays out.
askRenameTo
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => FormatRename
  -> [(T.Text, [Entry])]
  -> Eff es ()
askRenameTo r contents = do
  env <- getAppEnv
  case formatRenames env r.format contents of
    Left err -> showError err
    Right renames -> case filter (uncurry (/=)) renames of
      []
        | length renames == 1 -> showMessage "The directory has this name already"
        | otherwise -> showMessage "The directories have these names already"
      -- Only the new name, as the old one too would push it out of the
      -- status bar.
      changed@((_, new) : rest) ->
        let question
              | null rest = "Rename to " <> baseName new <> "?"
              | otherwise =
                  "Rename "
                    <> T.pack (show (length changed))
                    <> " directories, e.g. to "
                    <> baseName new
                    <> "?"
        in modifyS $ confirm question (Confirmed (ConfirmRename r.location changed))

-- | The path of each directory and its new path, by the name that the
-- format gives each of its songs. A directory must hold only songs, and
-- they must give the same name.
formatRenames
  :: AppEnv
  -> Format Void
  -> [(T.Text, [Entry])]
  -> Either T.Text [(T.Text, T.Text)]
formatRenames env format contents = do
  renames <- traverse rename contents
  case [new | new : _ : _ <- L.group (L.sort (map snd renames))] of
    new : _ -> Left $ "Two directories would be named " <> baseName new
    [] -> Right renames
  where
    rename :: (T.Text, [Entry]) -> Either T.Text (T.Text, T.Text)
    rename (path, entries) = do
      let named = baseName path
      songs <- forM entries $ \case
        SongEntry song -> Right song
        DirectoryEntry _ -> Left $ named <> " holds a directory"
        PlaylistEntry _ -> Left $ named <> " holds a playlist"
      case L.nub $ map (\song -> renderPlain (plainContext env.config.songs) song format) songs of
        [] -> Left $ named <> " holds no songs"
        [name] -> do
          let cleaned = T.pack $ fileName nameLimit name
          forM_ (nameFault cleaned) $ \fault ->
            Left $ "The name for " <> named <> " " <> fault
          Right (path, renamedPath path cleaned)
        _ -> Left $ "The songs of " <> named <> " give different names"

-- | Run an action if the directories of a listing can be renamed: MPD shows
-- where those of its database are only on its socket.
whenRenamable
  :: (State AppState :> es, UiRequest :> es)
  => Location
  -> Eff es ()
  -> Eff es ()
whenRenamable location action = case location of
  InDirectory _ ->
    whenOnSocket "Renaming in MPD's database needs a connection to MPD's socket" action
  _ -> action

-- | Rename directories of a listing, by their paths and their new paths
-- there. Those of MPD's database are renamed once MPD said where they are.
renameIn
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Location
  -> [(T.Text, T.Text)]
  -> Eff es ()
renameIn location renames = whenRenamable location $ case location of
  InLocal _ -> startRenames location renames [(T.unpack old, T.unpack new) | (old, new) <- renames]
  _ -> request listMounts (MountsListed location renames)

-- | Rename directories of MPD's database in the storages that hold them,
-- which must be local.
mountsListed
  :: (State AppState :> es, UiRequest :> es)
  => Location
  -> [(T.Text, T.Text)]
  -> [Mount]
  -> Eff es ()
mountsListed location renames mounts =
  either showError (startRenames location renames) $ traverse onDisk renames
  where
    onDisk :: (T.Text, T.Text) -> Either T.Text (FilePath, FilePath)
    onDisk (old, new) = do
      forM_ [old, new] $ \path ->
        when (any ((== path) . (.path)) mounts) . Left $ path <> " is a mount point of MPD"
      -- Both are in the same directory, so in the same storage.
      let deepest = listToMaybe $ L.sortOn (Down . T.length . (.path)) (filter (holds old) mounts)
      case (deepest, (.storage) =<< deepest) of
        (Just m, Just dir)
          | "/" `T.isPrefixOf` dir -> Right (inStorage m dir old, inStorage m dir new)
        (_, Just uri) -> Left $ old <> " is on " <> uri <> ", which isn't local"
        (_, Nothing) -> Left $ "MPD doesn't say where " <> old <> " is"

    holds :: T.Text -> Mount -> Bool
    holds path m = T.null m.path || (m.path <> "/") `T.isPrefixOf` path

    inStorage :: Mount -> T.Text -> T.Text -> FilePath
    inStorage m dir path =
      T.unpack dir
        </> T.unpack (if T.null m.path then path else T.drop (T.length m.path + 1) path)

-- | Rename directories of a listing, by their paths and their new paths
-- there, and on the disk.
startRenames
  :: (State AppState :> es, UiRequest :> es)
  => Location
  -> [(T.Text, T.Text)]
  -> [(FilePath, FilePath)]
  -> Eff es ()
startRenames location renames onDisk = do
  modifyS $ #browser % #renaming ?~ Renaming {location = location, renames = renames}
  renameDirectories onDisk

-- | After the renames: list the directories under their new names, with the
-- cursor on the first one. MPD's database has them once its update found
-- them.
directoriesRenamed
  :: (State AppState :> es, MpdRequest :> es, UiRequest :> es)
  => Int
  -> Maybe T.Text
  -> Eff es ()
directoriesRenamed n failure = do
  renaming <- getsS (.browser.renaming)
  forM_ renaming $ \r -> do
    modifyS $ #browser % #renaming .~ Nothing
    forM_ (listToMaybe r.renames) $ \(_, first) -> when (n > 0) $ case r.location of
      InLocal _ -> list r.location (JumpTo (DirectoryKey first))
      _ -> do
        modifyS $ #browser % #landOn ?~ (r.location, DirectoryKey first)
        mutate . void . update $ case r.location of
          InDirectory "" -> Nothing
          InDirectory path -> Just path
          _ -> Nothing
    case (failure, r.renames) of
      (Nothing, [(old, new)]) ->
        showMessage $ "Renamed " <> nameOf old <> " to " <> nameOf new
      (Nothing, _) -> showMessage $ "Renamed " <> countDirectories n
      (Just err, _)
        | n == 0 -> showError err
        | otherwise ->
            showError $
              "Renamed "
                <> T.pack (show n)
                <> " of "
                <> countDirectories (length r.renames)
                <> ", then: "
                <> err
  where
    nameOf :: T.Text -> T.Text
    nameOf = T.pack . takeFileName . T.unpack

    countDirectories :: Int -> T.Text
    countDirectories k = T.pack (show k) <> if k == 1 then " directory" else " directories"

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
