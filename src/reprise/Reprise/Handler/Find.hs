-- | Finding in the lists and in the screens of text: as the user types, and
-- the next and the previous match of the last pattern.
module Reprise.Handler.Find
  ( startFind
  , findAsYouType
  , acceptFind
  , findAgain
  ) where

import Control.Monad
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Effectful
import Effectful.Input.Static
import Effectful.State.Static.Local
import Optics.Core

import Reprise.Effect.MpdRequest
import Reprise.Effect.UiRequest
import Reprise.Find
import Reprise.Handler.Core
import Reprise.Handler.Screens
import Reprise.LineEdit
import Reprise.State

startFind :: Direction -> AppState -> AppState
startFind direction s =
  let v = focusedView s
  in openLine
       question
       emptyLineEdit
       (ForFind Finding {direction = direction, origin = (v.cursor, v.offset), note = Nothing})
       s
  where
    question :: T.Text
    question = case direction of
      Forward -> "Find forward: "
      Backward -> "Find backward: "

-- | Move to the first match from where the find started, on every key, so
-- that the result doesn't depend on how the pattern was typed. Returns the
-- find with a note on what it found.
findAsYouType
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Finding
  -> T.Text
  -> Eff es Finding
findAsYouType f text = do
  rows <- focusedRows
  start <- searchStart f.direction f.origin
  note <-
    if T.null text
      then modifyWithEnv (restoreView f.origin) >> pure Nothing
      else case compilePattern text of
        -- The cursor stays until the pattern is complete again.
        Left err -> pure (Just err)
        Right p -> case search p f.direction start rows of
          Left err -> pure (Just err)
          Right Nothing -> modifyWithEnv (restoreView f.origin) >> pure (Just "no match")
          Right (Just found) -> do
            modifyWithEnv (jumpTo found.index)
            pure (wrapNote f.direction found)
  pure $ f & #note .~ note

-- | Keep the pattern for the next and the previous match, in every screen.
-- An empty pattern forgets the last one, and what it found.
acceptFind
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Finding
  -> T.Text
  -> Eff es ()
acceptFind f text
  | T.null text = do
      modifyS $ \s ->
        s
          & #findPattern .~ Nothing
          & #foundOn .~ Nothing
      showMessage "Find cleared"
  | otherwise = case compilePattern text of
      Left _ -> do
        modifyWithEnv (restoreView f.origin)
        showError $ "Invalid pattern: " <> text
      Right _ -> do
        screen <- getsS ((.screen) . focusedView)
        modifyS $ \s ->
          s
            & #findPattern ?~ text
            & #foundOn ?~ screen
        forM_ f.note (showMessage . capitalize)

-- | Move to the next or the previous match of the last pattern in rows of
-- the focused list.
findAgain
  :: (State AppState :> es, Input AppEnv :> es, UiRequest :> es)
  => Eff es (Seq.Seq Folded)
  -> Direction
  -> Eff es ()
findAgain getRows direction = withFindPattern $ \text p -> do
  rows <- getRows
  v <- getsS focusedView
  modifyS $ #foundOn ?~ v.screen
  start <- searchStart direction (v.cursor, v.offset)
  case search p direction start rows of
    Left err -> showError (capitalize err)
    Right Nothing -> showMessage $ "No match for " <> text
    Right (Just found) -> do
      modifyWithEnv (jumpTo found.index)
      forM_ (wrapNote direction found) (showMessage . capitalize)

-- | The row that a search starts after, from a cursor and an offset of the
-- focused view: the cursor of a list. A screen of text has no cursor; a
-- find puts it on its match. While that shows, a search starts there, else
-- at the edge of the view where it goes from.
searchStart
  :: (State AppState :> es, Input AppEnv :> es)
  => Direction
  -> (Int, Int)
  -> Eff es Int
searchStart direction (c, o) = do
  env <- getAppEnv
  s <- getS
  let v = focusedView s
      h = listHeight env s v
  pure $ case (screenInfo v.screen).content of
    Lines
      | c < o || c >= o + h -> case direction of
          Forward -> o - 1
          Backward -> o + h
    _ -> c

wrapNote :: Direction -> Found -> Maybe T.Text
wrapNote direction found
  | found.wrapped = Just $ case direction of
      Forward -> "wrapped around to the top"
      Backward -> "wrapped around to the bottom"
  | otherwise = Nothing

-- | The rows of the focused list as finds match them. A find opens only on
-- a screen with rows, and its prompt keeps the screen until it closes.
focusedRows
  :: (State AppState :> es, Input AppEnv :> es, MpdRequest :> es, UiRequest :> es)
  => Eff es (Seq.Seq Folded)
focusedRows = do
  screen <- getsS ((.screen) . focusedView)
  fromMaybe (pure Seq.empty) (screenOps screen).rows
