-- | The lyrics screen: the lyrics of a song as text that scrolls, which a
-- worker reads from the directory of lyrics.
module Reprise.Screen.Lyrics
  ( lyricsView
  , showLyrics
  , lyricsLoaded
  ) where

import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Effect.UiRequest
import Reprise.Handler.Core
import Reprise.Lyrics
import Reprise.State
import Reprise.Style

lyricsView :: AppEnv -> AppState -> View -> V.Image
lyricsView env s v =
  V.vertCat . map (V.text' (toAttr env.colorMode mempty)) . take v.height . drop v.offset $
    lyricsLines v.width s.lyrics

-- | Show the lyrics of the song under the cursor, from the top. On the
-- lyrics screen, go back to the screen that showed them.
showLyrics :: App es => Eff es ()
showLyrics = do
  s <- getS
  let screen = (focusedView s).screen
  if
    | screen == LyricsScreen ->
        modifyWithEnv . modifyView $ switchScreen s.lyrics.returnTo
    | Nothing <- screenDisplay s screen ->
        showMessage $ "The " <> screenText screen <> " has no songs"
    | Just song <- songUnderCursor s -> do
        token <- newToken
        modifyS $ #lyrics .~ LyricsState (Just song) token Nothing screen
        fetchLyrics token song
        modifyWithEnv . modifyView $ (#offset .~ 0) . switchScreen LyricsScreen
    | otherwise -> showMessage "There is no song under the cursor"

-- | The lyrics of the request with the token. Those of a song that the
-- screen showed before are dropped.
lyricsLoaded :: App es => Int -> LyricsResult -> Eff es ()
lyricsLoaded token result = do
  current <- getsS (.lyrics.token)
  if token == current
    then modifyS $ #lyrics % #result ?~ result
    else keepScreen
