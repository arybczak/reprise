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
  , whenCurrent

    -- * Messages
  , showMessage
  , showError
  , notAvailable
  , capitalize
  , screenText
  , screenHasNo
  , noCurrentSong
  , onOff

    -- * Toggles
  , toggleSetting
  , toggleDisplay
  , cycleNext

    -- * Prompts
  , openLine
  , openChoice
  , confirm

    -- * Saving
  , askSaveName
  , describeSave
  , nothingToSave

    -- * Views
  , modifyView
  , switchTo
  , setCursor
  , jumpTo
  , jumpScreenTo
  , screenPosition
  , setScreenPosition
  , moveListCursor
  , restoreView
  , scrollLines
  , showSongScreen
  , songUnderCursor
  , goBack

    -- * Selection
  , selectInList
  , withFindPattern
  ) where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Map.Strict qualified as M
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
import Reprise.LineEdit
import Reprise.Mpd.Protocol.Types
import Reprise.Save
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

-- | Run what the event of a request or of a timer does, if it is of the
-- newest one: the state holds what it is for, with its token. An event of
-- one that a newer one replaced changes nothing on the screen.
whenCurrent
  :: App es => (AppState -> Maybe a) -> (a -> Int) -> Int -> (a -> Eff es ()) -> Eff es ()
whenCurrent current tokenOf token k =
  getsS current >>= \case
    Just a | tokenOf a == token -> k a
    _ -> keepScreen

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

capitalize :: T.Text -> T.Text
capitalize t = case T.uncons t of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> t

screenText :: ScreenName -> T.Text
screenText screen = screenWords screen <> " screen"

-- | Say that a screen has no such thing, e.g. no songs or no action.
screenHasNo :: App es => ScreenName -> T.Text -> Eff es ()
screenHasNo screen what = showMessage $ "The " <> screenText screen <> " has no " <> what

-- | MPD has no current song, e.g. before anything played. A song that
-- stopped stays current.
noCurrentSong :: App es => Eff es ()
noCurrentSong = showMessage "There is no current song"

onOff :: Bool -> T.Text
onOff b = if b then "on" else "off"

----------------------------------------
-- Toggles

-- | Turn a setting of reprise on or off, and say which it is now.
toggleSetting :: App es => T.Text -> Lens' Toggles Bool -> Eff es ()
toggleSetting name field = do
  modifyS $ #toggles % field %~ not
  v <- getsS (view (#toggles % field))
  showMessage $ name <> ": " <> onOff v

-- | Show the songs of a list in the other display.
toggleDisplay :: App es => Lens' Toggles Display -> Eff es ()
toggleDisplay field = do
  modifyS $
    #toggles % field %~ \case
      Classic -> Columns
      Columns -> Classic
  modifyWithEnv (modifyView id)
  d <- getsS (view (#toggles % field))
  showMessage $ "Display: " <> displayName d

-- | The next value, after the last the first.
cycleNext :: (Eq a, Bounded a, Enum a) => a -> a
cycleNext a = if a == maxBound then minBound else succ a

----------------------------------------
-- Prompts

-- | Open a line prompt for a purpose, which "Reprise.Handler" runs with the
-- answer.
openLine :: T.Text -> LineEdit -> LinePurpose -> AppState -> AppState
openLine question edit purpose = #prompt ?~ Prompt question (Line edit purpose Nothing)

-- | Open a choice between options, each picked by a letter of its name.
openChoice :: T.Text -> [ChoiceOption] -> AppState -> AppState
openChoice question options = #prompt ?~ Prompt question (Choice options)

-- | Ask a yes or no question, and send the event on yes.
confirm :: T.Text -> AppEvent -> AppState -> AppState
confirm question onYes =
  openChoice question [ChoiceOption 'y' "yes" (Just onYes), ChoiceOption 'n' "no" Nothing]

----------------------------------------
-- Saving

-- | Ask for the name of the stored playlist to save to, which
-- "Reprise.Handler" saves to.
askSaveName :: App es => SaveSource -> Eff es ()
askSaveName source
  | Just why <- nothingToSave source = showMessage why
  | otherwise =
      modifyS $
        openLine ("Save " <> describeSave source <> " as: ") (LineEdit "" "") (ForSave source)

-- | What a save saves, as the status bar says it.
describeSave :: SaveSource -> T.Text
describeSave = \case
  SaveQueue -> "the queue"
  SaveItems items
    | all isSong saved -> countSongs (length saved)
    | otherwise -> countItems (length saved)
    where
      saved :: [SaveItem]
      saved = savedItems items

      isSong :: SaveItem -> Bool
      isSong = \case
        SaveSong _ -> True
        _ -> False

-- | Why a save has nothing to save, if it hasn't.
nothingToSave :: SaveSource -> Maybe T.Text
nothingToSave = \case
  SaveItems items
    | null (savedItems items) ->
        Just $
          if null items
            then "There is nothing to save"
            else "Parts of files, e.g. the tracks of a cue sheet, can't be saved"
  _ -> Nothing

----------------------------------------
-- Views

-- | Move the cursor of the focused view and scroll it into view.
setCursor :: Int -> AppEnv -> AppState -> AppState
setCursor c = modifyView (#cursor .~ c)

-- | Move the cursor to an item in the middle of the list, as every jump
-- does, so that the item's neighbours show on both sides.
jumpTo :: Int -> AppEnv -> AppState -> AppState
jumpTo p env s = jumpScreenTo (focusedView s).screen p env s

-- | 'jumpTo' in the list of a screen, also while the view shows another
-- screen.
jumpScreenTo :: ScreenName -> Int -> AppEnv -> AppState -> AppState
jumpScreenTo screen p env s =
  let h = listHeight env s (focusedView s & #screen .~ screen)
  in -- modifyView brings the offset back into the list when the view shows
     -- the screen.
     setScreenPosition screen (p, p - h `div` 2) env s

-- | The cursor and the offset of a screen in the focused view, which
-- remembers them while it shows another screen.
screenPosition :: ScreenName -> AppState -> (Int, Int)
screenPosition screen s
  | v.screen == screen = (v.cursor, v.offset)
  | otherwise = M.findWithDefault (0, 0) screen v.positions
  where
    v :: View
    v = focusedView s

-- | Set the cursor and the offset of a screen in the focused view, also
-- while the view shows another screen.
setScreenPosition :: ScreenName -> (Int, Int) -> AppEnv -> AppState -> AppState
setScreenPosition screen (c, o) env s
  | (focusedView s).screen == screen = restoreView (c, o) env s
  | otherwise = s & #views % ix s.focus % #positions % at screen ?~ (c, max 0 o)

-- | Move the cursor of the focused list. The songs of its items tell albums
-- and artists apart.
moveListCursor :: MoveTarget -> AppEnv -> AppState -> AppState
moveListCursor t env s =
  let v = focusedView s
      info = screenInfo v.screen
      h = max 1 (listHeight env s v)
      n = info.size env s v
      c = v.cursor
      -- What tells the groups of the item at an index apart.
      keyOf :: (Song -> k) -> Int -> Maybe k
      keyOf key = fmap key . info.songAt s
  in case t of
       MoveUp -> setCursor (c - 1) env s
       MoveDown -> setCursor (c + 1) env s
       MovePageUp -> modifyView (paged (-h)) env s
       MovePageDown -> modifyView (paged h) env s
       MoveFirst -> setCursor 0 env s
       MoveLast -> setCursor (n - 1) env s
       MovePreviousAlbum -> jumpTo (previousGroup (keyOf albumKey) n c) env s
       MoveNextAlbum -> jumpTo (nextGroup (keyOf albumKey) n c) env s
       MovePreviousArtist -> jumpTo (previousGroup (keyOf artistKey) n c) env s
       MoveNextArtist -> jumpTo (nextGroup (keyOf artistKey) n c) env s
  where
    -- The cursor stays on its row, as in ncmpcpp, unless the start or the
    -- end of the list stops the scrolling.
    paged :: Int -> View -> View
    paged delta = (#cursor %~ (+ delta)) . (#offset %~ (+ delta))

----------------------------------------
-- Selection

-- | Change the selection of the focused list, as a select action says. The
-- list's items have keys, which the selection holds, and songs, which tell
-- albums and artists apart. An item without a key can't be selected.
selectInList
  :: forall k es
   . (App es, Ord k)
  => Lens' AppState (Selection k)
  -> Seq.Seq (Maybe k, Maybe Song)
  -> Eff es (Seq.Seq Folded)
  -- ^ The text of the items as finds match it.
  -> (Int -> T.Text)
  -- ^ How many items, e.g. @3 songs@.
  -> SelectTarget
  -> Eff es ()
selectInList selection items findRows count = \case
  SelectItem move -> do
    c <- getsS ((.cursor) . focusedView)
    forM_ (Seq.lookup c items >>= fst) $ \k -> modifyS $ selection %~ toggleKey k
    forM_ move $ modifyWithEnv . moveListCursor
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
  SelectFound -> withFindPattern $ \_ p -> do
    rows <- findRows
    case matchAll p rows of
      Left err -> showError (capitalize err)
      Right found -> do
        let ks = [k | (Just k, True) <- zip (toList itemKeys) (toList found)]
        modifyS $ selection %~ addKeys ks
        showMessage $ count (length ks) <> " found and selected"
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

-- | Run with the pattern of the last find, and its text, or say why there
-- is none.
withFindPattern :: App es => (T.Text -> Pattern -> Eff es ()) -> Eff es ()
withFindPattern k =
  getsS (.findPattern) >>= \case
    Nothing -> showMessage "Nothing was found yet"
    Just text -> either (showError . capitalize) (k text) (compilePattern text)

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
          info = screenInfo v'.screen
          n = info.size env s v'
          h = max 1 (listHeight env s v')
          c = max 0 (min (n - 1) v'.cursor)
          o
            | Lines <- info.content = v'.offset
            | env.config.lists.keepCursorCentered = c - h `div` 2
            | c < v'.offset = c
            | c >= v'.offset + h = c - h + 1
            | otherwise = v'.offset
      in v' & #cursor .~ c & #offset .~ max 0 (min (n - h) o)

-- | Show another screen in the focused view.
switchTo :: App es => ScreenName -> Eff es ()
switchTo = modifyWithEnv . modifyView . switchScreen

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
    MoveLast -> modifyWithEnv . modifyView $ #offset .~ (screenInfo v.screen).size env s v
    _ -> screenHasNo v.screen $ renderAction (Move t)
  where
    scroll :: App es => Int -> Eff es ()
    scroll delta = modifyWithEnv . modifyView $ #offset %~ (+ delta)

-- | Show a screen of a song, e.g. its lyrics, for the song under the cursor,
-- from the top, after an action that opens it for the song, if it can.
showSongScreen :: App es => ScreenName -> (Song -> Eff es Bool) -> Eff es ()
showSongScreen target open = do
  s <- getS
  let screen = (focusedView s).screen
  case (screenInfo screen).content of
    Songs _
      | Just song <- songUnderCursor s -> do
          opened <- open song
          when opened . modifyWithEnv . modifyView $ (#offset .~ 0) . switchScreen target
      | otherwise -> showMessage "There is no song under the cursor"
    _ -> screenHasNo screen "songs"

-- | The song under the cursor of the focused list.
songUnderCursor :: AppState -> Maybe Song
songUnderCursor s =
  let v = focusedView s
  in (screenInfo v.screen).songAt s v.cursor

-- | Show the screen that the view showed before.
goBack :: App es => Eff es ()
goBack =
  getsS ((.previous) . focusedView) >>= \case
    Just previous -> switchTo previous
    Nothing -> showMessage "There is no screen to go back to"
