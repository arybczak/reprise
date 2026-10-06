-- | What the handlers of events and the screens share: the effects, access
-- to the state and the settings, messages, prompts and the views. The
-- screens import this module, and it imports no screen, so that
-- "Reprise.Handler" can pass actions on to the screens.
module Reprise.Handler.Core
  ( -- * Effects
    App

    -- * State
  , getS
  , getsS
  , modifyS
  , getAppEnv
  , modifyWithEnv
  , newToken

    -- * Messages
  , showMessage
  , showError
  , notAvailable
  , countSongs
  , countItems
  , capitalize
  , screenText

    -- * Prompts
  , openLine

    -- * Views
  , modifyView
  , setCursor
  , jumpTo
  , moveListCursor
  , restoreView
  , screenLength
  , scrollLines
  , showSongScreen
  , goBack
  , songUnderCursor

    -- * Selection
  , selectInList
  , selectFound
  ) where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful
import Effectful.Input.Static
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Event
import Reprise.Find
import Reprise.Groups
import Reprise.Keymap
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.Selection
import Reprise.State

-- | The effects of the handlers.
type App es =
  (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)

----------------------------------------
-- State

getS :: App es => Eff es AppState
getS = get @AppState

getsS :: App es => (AppState -> a) -> Eff es a
getsS = gets @AppState

modifyS :: App es => (AppState -> AppState) -> Eff es ()
modifyS = modify @AppState

getAppEnv :: App es => Eff es AppEnv
getAppEnv = input

-- | Change the state with a function that reads the settings too.
modifyWithEnv :: App es => (AppEnv -> AppState -> AppState) -> Eff es ()
modifyWithEnv f = getAppEnv >>= modifyS . f

newToken :: App es => Eff es Int
newToken = state @AppState $ \s -> (s.nextToken, s & #nextToken %~ (+ 1))

----------------------------------------
-- Messages

showMessage :: App es => T.Text -> Eff es ()
showMessage = message False

notAvailable :: App es => T.Text -> Eff es ()
notAvailable what = showMessage $ what <> " isn't available yet"

showError :: App es => T.Text -> Eff es ()
showError = message True

message :: App es => Bool -> T.Text -> Eff es ()
message isError text = do
  token <- newToken
  modifyS $ #message ?~ Message text isError token
  after messageTimeout (MessageExpired token)
  where
    -- How long a status bar message stays. ncmpcpp's default
    -- @message_delay_time@.
    messageTimeout :: Double
    messageTimeout = 5

countSongs :: Int -> T.Text
countSongs n = T.pack (show n) <> if n == 1 then " song" else " songs"

countItems :: Int -> T.Text
countItems n = T.pack (show n) <> if n == 1 then " item" else " items"

capitalize :: T.Text -> T.Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> t

screenText :: ScreenName -> T.Text
screenText screen = T.replace "_" " " (screenName screen) <> " screen"

----------------------------------------
-- Prompts

-- | Open a line prompt for a purpose, which "Reprise.Handler" runs with the
-- answer.
openLine :: T.Text -> LineEdit -> LinePurpose -> AppState -> AppState
openLine question edit purpose = #prompt ?~ Prompt question (Line edit purpose)

----------------------------------------
-- Views

-- | Move the cursor of the focused view and scroll it into view.
setCursor :: Int -> AppEnv -> AppState -> AppState
setCursor c = modifyView (#cursor .~ c)

-- | Move the cursor to an item in the middle of the list, as every jump
-- does, so that the item's neighbours show on both sides.
jumpTo :: Int -> AppEnv -> AppState -> AppState
jumpTo p env s =
  let h = listHeight env s (focusedView s)
  in -- modifyView brings the offset back into the list.
     modifyView ((#cursor .~ p) . (#offset .~ p - h `div` 2)) env s

-- | Move the cursor of the focused list. The songs of its items tell albums
-- and artists apart.
moveListCursor
  :: (a -> Maybe Song) -> Seq.Seq a -> MoveTarget -> AppEnv -> AppState -> AppState
moveListCursor songOf items t env s =
  let h = max 1 (listHeight env s (focusedView s))
      c = (focusedView s).cursor
  in case t of
       MoveUp -> setCursor (c - 1) env s
       MoveDown -> setCursor (c + 1) env s
       MovePageUp -> modifyView (paged (-h)) env s
       MovePageDown -> modifyView (paged h) env s
       MoveFirst -> setCursor 0 env s
       MoveLast -> setCursor (Seq.length items - 1) env s
       MovePreviousAlbum -> jumpTo (previousGroup (fmap albumKey . songOf) items c) env s
       MoveNextAlbum -> jumpTo (nextGroup (fmap albumKey . songOf) items c) env s
       MovePreviousArtist -> jumpTo (previousGroup (fmap artistKey . songOf) items c) env s
       MoveNextArtist -> jumpTo (nextGroup (fmap artistKey . songOf) items c) env s
  where
    -- The cursor stays on its row, as in ncmpcpp, unless the start or the
    -- end of the list stops the scrolling.
    paged :: Int -> View -> View
    paged delta = (#cursor %~ (+ delta)) . (#offset %~ (+ delta))

----------------------------------------
-- Selection

-- | Change the selection of the focused list, as a select action says. The
-- list's items have keys, which the selection holds, and songs, which tell
-- albums and artists apart. An item without a key can't be selected. A
-- screen selects what its find found, and moves after a select itself.
selectInList
  :: forall k es
   . (App es, Ord k)
  => Lens' AppState (Selection k)
  -> Seq.Seq (Maybe k, Maybe Song)
  -> SelectTarget
  -> Eff es ()
selectInList selection items = \case
  SelectItem _ -> do
    c <- getsS ((.cursor) . focusedView)
    forM_ (Seq.lookup c items >>= fst) $ \k -> modifyS $ selection %~ toggleKey k
  SelectRange ->
    getsS (selectRange itemKeys . view selection) >>= \case
      Nothing -> showMessage "Select the first and the last item of the range first"
      Just sel -> do
        modifyS $ selection .~ sel
        showMessage "Range selected"
  SelectInvert -> do
    modifyS $ selection %~ invert itemKeys
    showMessage "Selection inverted"
  SelectNone -> do
    modifyS $ selection %~ deselectAll
    showMessage "Selection cleared"
  SelectAlbum -> selectGroup albumKey "Album"
  SelectArtist -> selectGroup artistKey "Artist"
  SelectFound -> pure ()
  where
    itemKeys :: Seq.Seq (Maybe k)
    itemKeys = fst <$> items

    -- The items next to each other around the cursor with its song's key.
    selectGroup :: (App es, Eq g) => (Song -> g) -> T.Text -> Eff es ()
    selectGroup key name = do
      c <- getsS ((.cursor) . focusedView)
      forM_ (Seq.lookup c items >>= snd) $ \song -> do
        let same i = (key <$> (Seq.lookup i items >>= snd)) == Just (key song)
            earlier = takeWhile same [c - 1, c - 2 .. 0]
            later = takeWhile same [c + 1 .. Seq.length items - 1]
            group = earlier <> [c] <> later
        modifyS $ selection %~ addKeys [k | i <- group, Just k <- [Seq.lookup i items >>= fst]]
        showMessage $ name <> " around the cursor selected"

-- | Select the items that the last find's pattern matches. The rows are the
-- text of the items as finds match it.
selectFound
  :: (App es, Ord k)
  => Lens' AppState (Selection k)
  -> Seq.Seq (Maybe k)
  -> Seq.Seq Folded
  -> (Int -> T.Text)
  -- ^ How many items, e.g. @3 songs@.
  -> Eff es ()
selectFound selection itemKeys rows count = do
  s <- getS
  case compilePattern <$> s.findPattern of
    Nothing -> showMessage "Nothing was found yet"
    Just (Left err) -> showError (capitalize err)
    Just (Right p) -> case matchAll p rows of
      Left err -> showError (capitalize err)
      Right found -> do
        let ks = [k | (Just k, True) <- zip (toList itemKeys) (toList found)]
        modifyS $ selection %~ addKeys ks
        showMessage $ count (length ks) <> " found and selected"

-- | Bring back a cursor and an offset, e.g. after a cancelled find.
restoreView :: (Int, Int) -> AppEnv -> AppState -> AppState
restoreView (c, o) = modifyView $ (#cursor .~ c) . (#offset .~ o)

-- | Change the focused view, then keep its cursor in the list and visible.
-- A screen of text has no cursor, so only its offset is kept in the text.
modifyView :: (View -> View) -> AppEnv -> AppState -> AppState
modifyView f env s =
  s
    & #views % ix s.focus %~ \v ->
      let v' = f v
          n = screenLength env s v'.screen
          h = max 1 (listHeight env s v')
          c = max 0 (min (n - 1) v'.cursor)
          o
            | v'.screen `elem` textScreens = v'.offset
            | env.config.lists.keepCursorCentered = c - h `div` 2
            | c < v'.offset = c
            | c >= v'.offset + h = c - h + 1
            | otherwise = v'.offset
      in v' & #cursor .~ c & #offset .~ max 0 (min (n - h) o)

-- | The screens of text without items, which a move scrolls.
textScreens :: [ScreenName]
textScreens = [LyricsScreen, SongInfoScreen, HelpScreen]

-- | Scroll the focused screen of text.
scrollLines :: App es => MoveTarget -> Eff es ()
scrollLines t = do
  env <- getAppEnv
  s <- getS
  let v = focusedView s
      h = max 1 (listHeight env s v)
  case t of
    MoveUp -> scroll (-1)
    MoveDown -> scroll 1
    MovePageUp -> scroll (-h)
    MovePageDown -> scroll h
    MoveFirst -> modifyWithEnv . modifyView $ #offset .~ 0
    MoveLast -> modifyWithEnv . modifyView $ #offset .~ screenLength env s v.screen
    _ -> showMessage $ "The " <> screenText v.screen <> " has no " <> renderAction (Move t)
  where
    scroll :: App es => Int -> Eff es ()
    scroll delta = modifyWithEnv . modifyView $ #offset %~ (+ delta)

-- | The number of items or lines of a screen.
screenLength :: AppEnv -> AppState -> ScreenName -> Int
screenLength env s = \case
  QueueScreen -> Seq.length s.mirror.queue
  BrowserScreen -> Seq.length s.browser.items
  LyricsScreen -> length (lyricsRows (fst s.terminalSize) s.lyrics)
  SongInfoScreen -> length (songInfoRows env s (fst s.terminalSize))
  HelpScreen -> length (helpLines env.keymaps)
  OutputsScreen -> maybe 0 Seq.length s.outputs
  _ -> 0

-- | Show a screen of a song, e.g. its lyrics, for the song under the cursor,
-- from the top, after an action that opens it for the song.
showSongScreen :: App es => ScreenName -> (Song -> Eff es ()) -> Eff es ()
showSongScreen target open = do
  s <- getS
  let screen = (focusedView s).screen
  if
    | Nothing <- screenDisplay s screen ->
        showMessage $ "The " <> screenText screen <> " has no songs"
    | Just song <- songUnderCursor s -> do
        open song
        modifyWithEnv . modifyView $ (#offset .~ 0) . switchScreen target
    | otherwise -> showMessage "There is no song under the cursor"

-- | Show the screen that the view showed before.
goBack :: App es => Eff es ()
goBack =
  getsS ((.previous) . focusedView) >>= \case
    Just previous -> modifyWithEnv . modifyView $ switchScreen previous
    Nothing -> showMessage "There is no screen to go back to"

-- | The song under the cursor of the focused list.
songUnderCursor :: AppState -> Maybe Song
songUnderCursor s =
  let v = focusedView s
  in case v.screen of
       QueueScreen -> Seq.lookup v.cursor s.mirror.queue
       BrowserScreen -> case Seq.lookup v.cursor s.browser.items of
         Just (EntryItem (SongEntry song)) -> Just song
         _ -> Nothing
       _ -> Nothing
