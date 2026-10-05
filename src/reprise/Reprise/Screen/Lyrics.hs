-- | The lyrics screen: the lyrics of a song as text that scrolls, which a
-- worker reads from the directory of lyrics, or fetches.
module Reprise.Screen.Lyrics
  ( lyricsView
  , showLyrics
  , refetchLyrics
  , lyricsFetching
  , lyricsLoaded
  ) where

import Effectful
import Graphics.Vty qualified as V
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Handler.Core
import Reprise.Lyrics
import Reprise.Mpd.Protocol.Types
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
        request song False screen
        modifyWithEnv . modifyView $ (#offset .~ 0) . switchScreen LyricsScreen
    | otherwise -> showMessage "There is no song under the cursor"

-- | Fetch the lyrics of the song on the screen again, and store them anew.
refetchLyrics :: App es => Eff es ()
refetchLyrics = do
  env <- getAppEnv
  s <- getS
  case s.lyrics.song of
    Nothing -> showMessage "There is no song to fetch the lyrics of"
    Just song
      | null env.config.lyrics.fetchers ->
          showMessage "There is nowhere to fetch the lyrics from: lyrics.fetchers is empty"
      | otherwise -> do
          request song True s.lyrics.returnTo
          modifyWithEnv . modifyView $ #offset .~ 0

-- | Ask for the lyrics of a song, which the screen shows when they come.
request :: App es => Song -> Bool -> ScreenName -> Eff es ()
request song refetch returnTo = do
  token <- newToken
  modifyS $ #lyrics .~ LyricsState (Just song) token ReadingLyrics returnTo
  fetchLyrics token (LyricsRequest song refetch)

-- | The lyrics of the request with the token aren't stored, so they are
-- being fetched.
lyricsFetching :: App es => Int -> Eff es ()
lyricsFetching token = forRequest token $ modifyS (#lyrics % #status .~ FetchingLyrics)

-- | The lyrics of the request with the token. Those of a song that the
-- screen showed before are dropped.
lyricsLoaded :: App es => Int -> LyricsResult -> Eff es ()
lyricsLoaded token result = forRequest token $ do
  modifyS $ #lyrics % #status .~ ShowingLyrics result
  case result of
    LyricsFound (Fetched fetcher) _ -> showMessage $ "Fetched the lyrics from " <> fetcher
    _ -> pure ()

-- | Run what a reply of the worker does, if it is of the newest request.
forRequest :: App es => Int -> Eff es () -> Eff es ()
forRequest token k = do
  current <- getsS (.lyrics.token)
  if token == current then k else keepScreen
