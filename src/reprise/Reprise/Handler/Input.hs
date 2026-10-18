-- | The keys, in the keymaps and in the prompts, and the mouse.
module Reprise.Handler.Input
  ( handleKey
  , handleWheel
  , handleClick
  , clickSong
  , askPassword
  ) where

import Control.Monad
import Data.Foldable
import Data.Maybe
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
import Reprise.Exception
import Reprise.Handler.Action
import Reprise.Handler.Core
import Reprise.Handler.Find
import Reprise.Handler.Save
import Reprise.Handler.Seek
import Reprise.Header
import Reprise.History
import Reprise.Keymap
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Browser
import Reprise.State
import Reprise.StatusBar

-- | Handle a key, in a prompt or in the keymaps. The option of a choice may
-- be an event, which the key returns, for the handler of events.
handleKey
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => KeySpec
  -> Eff es (Maybe AppEvent)
handleKey k = do
  env <- getAppEnv
  s <- getS
  noteInput
  case s.prompt of
    _ | k == alwaysQuit -> Nothing <$ quit
    Just p -> handlePromptKey p k
    Nothing ->
      Nothing <$ case s.pendingKeys of
        Just pending
          | isCancel k -> modifyS $ #pendingKeys .~ Nothing
          | otherwise -> continue pending.layers (pending.keys <> [k])
        Nothing -> continue (startLayers (focusedView s).screen env.keymaps) [k]
  where
    continue
      :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
      => Layers
      -> [KeySpec]
      -> Eff es ()
    continue layers keys = case lookupKey layers k of
      Bound a -> do
        modifyS $ #pendingKeys .~ Nothing
        runAction a
      Pending layers' -> modifyS $ #pendingKeys ?~ PendingKeys keys layers'
      Unbound -> do
        modifyS $ #pendingKeys .~ Nothing
        when (length keys > 1) . showMessage $
          T.unwords (map renderKeySpec keys) <> " is not bound"

-- | A step of the wheel over the volume changes it, as ncmpcpp's does. Over
-- the focused view, it moves the cursor of a list, or scrolls text, by its
-- lines, as the screen's moves do. Elsewhere, in a prompt, or over a screen
-- without moves, it does nothing.
handleWheel
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => MoveTarget
  -> Int
  -> Int
  -> Eff es ()
handleWheel t col row = do
  env <- getAppEnv
  s <- getS
  let ScrollLines n = env.config.mouse.scrollLines
      VolumeStep step = env.config.mouse.volumeStep
  if
    | isJust s.prompt -> keepScreen
    | onVolume s col row ->
        runAction . Volume . VolumeBy $ if t == MoveUp then step else negate step
    | fmap fst (viewAt s col row) == Just s.focus
    , Just moveOnce <- screenVerb (focusedView s).screen (Move t) -> do
        noteInput
        replicateM_ n moveOnce
    | otherwise -> keepScreen

-- | A left click on the state of the player in the status bar pauses it,
-- or plays it again. A left click on the progress bar, while a song plays
-- or pauses, seeks to where its cell begins. A click on an item of a list
-- moves the cursor to it, and a right click activates it then. All are as
-- in ncmpcpp. The list doesn't take clicks while a prompt or the panel of
-- a key sequence is open, which the panel would cover.
handleClick
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => MouseButton
  -> Int
  -> Int
  -> Eff es ()
handleClick button col row = do
  env <- getAppEnv
  s <- getS
  let width = fst s.terminalSize
  if
    | button == LeftButton, onPlayerLabel s col row -> runAction Pause
    | button == LeftButton
    , row == progressBarRow s
    , col >= 0 && col < width
    , Just d <- progressDuration s ->
        seekNow (cellTime width col d)
    | isNothing s.prompt
    , isNothing s.pendingKeys
    , Just i <- listItemAt env s col row ->
        clickItem button i
    | otherwise -> keepScreen

-- | A click on a song of the queue that waited for an edit, which the edit
-- may have moved or deleted. The keys before it may have shown another
-- screen, or opened a prompt or a key sequence, which take no clicks.
clickSong
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => MouseButton
  -> SongId
  -> Eff es ()
clickSong button songId = do
  s <- getS
  case Seq.findIndexL ((== Just songId) . (.songId)) s.mirror.queue of
    Just i
      | (focusedView s).screen == QueueScreen
      , isNothing s.prompt
      , isNothing s.pendingKeys ->
          clickItem button i
    _ -> keepScreen

-- | A click on an item of a list moves the cursor to it, and a right click
-- activates it.
clickItem
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => MouseButton
  -> Int
  -> Eff es ()
clickItem button i = do
  noteInput
  modifyWithEnv (setCursor i)
  when (button == RightButton) $ runAction Activate

-- | The user did something, which shows the cursor until it is idle again.
noteInput :: (State AppState :> es, UiRequest :> es) => Eff es ()
noteInput = do
  s <- getS
  modifyS $ #lastInput .~ s.now
  unless s.cursorTimer $ do
    modifyS $ #cursorTimer .~ True
    after cursorHideDelay HideCursor

isCancel :: KeySpec -> Bool
isCancel k = k `elem` cancelKeys

handlePromptKey
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Prompt
  -> KeySpec
  -> Eff es (Maybe AppEvent)
handlePromptKey p k = case p.input of
  Choice options
    | Just o <- find (\o -> k == char o.letter) options -> do
        close
        maybe (Nothing <$ showMessage "Cancelled") (pure . Just) o.event
    | isCancel k -> do
        close
        showMessage "Cancelled"
        pure Nothing
    | otherwise -> pure Nothing
  Line edit purpose recall
    | k == plain Enter -> do
        close
        let text = lineEditText edit
        when (purpose /= ForPassword) $ do
          modifyS $ #history %~ remember text
          saveToHistory text
        answer purpose text
        pure Nothing
    | isCancel k -> do
        close
        case purpose of
          ForFind f -> modifyWithEnv (restoreView f.origin)
          ForPassword -> answerPassword Nothing
          _ -> pure ()
        pure Nothing
    | otherwise -> do
        history <- getsS (.history)
        forM_ (lineKey history edit purpose recall) $ \(edit', recall') -> do
          purpose' <- case purpose of
            ForFind f -> ForFind <$> findAsYouType f (lineEditText edit')
            other -> pure other
          modifyS $ #prompt ?~ Prompt p.question (Line edit' purpose' recall')
        pure Nothing
  where
    close :: State AppState :> es => Eff es ()
    close = modifyS $ #prompt .~ Nothing

    -- The line after a key that edits it or recalls a line of the history.
    -- The history leaves out the password, and the password the history.
    lineKey
      :: [T.Text] -> LineEdit -> LinePurpose -> Maybe Recall -> Maybe (LineEdit, Maybe Recall)
    lineKey history edit purpose recall
      | purpose == ForPassword = edited
      | k `elem` [plain ArrowUp, ctrl 'p'] =
          fmap Just <$> recallOlder history edit recall
      | k `elem` [plain ArrowDown, ctrl 'n'] =
          recallNewer history <$> recall
      | k == plain PageUp = fmap Just <$> recallOldest history edit recall
      -- As bash's end-of-history, back to the typed line.
      | k == plain PageDown = (\r -> (r.typed, Nothing)) <$> recall
      | otherwise = edited
      where
        -- An edit makes the recalled line the typed one.
        edited :: Maybe (LineEdit, Maybe Recall)
        edited = (,Nothing) <$> editLine k edit

-- | Run what a line prompt asked for. An empty line of @:@ does nothing.
answer
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => LinePurpose
  -> T.Text
  -> Eff es ()
answer purpose text = case purpose of
  ForFind f -> acceptFind f text
  ForSave source -> saveNamed source (T.strip text)
  ForRename location path -> renameTo location path text
  ForRenameByFormat location paths -> renameByFormat location paths text
  ForPassword -> answerPassword (Just text)
  ForCommand
    | T.null (T.strip text) -> pure ()
    | otherwise -> either showError runAction (parseAction text)
  ForArgument name
    | T.null (T.strip text) -> pure ()
    | otherwise -> either showError runAction (parseAction (name <> " " <> text))

-- | Ask for the password after MPD refused a command or the password.
askPassword :: MpdError -> AppState -> AppState
askPassword err = openLine (reason <> ". Password: ") emptyLineEdit ForPassword
  where
    reason :: T.Text
    reason = case err of
      AckError ack
        | ack.code == AckPassword -> "Wrong password"
        | otherwise -> "MPD refused " <> ack.command
      _ -> exceptionText err
