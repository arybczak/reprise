-- | The lyrics screen: the lyrics of a song as text that scrolls, which a
-- worker reads from the directory of lyrics, or fetches.
module Reprise.Screen.Lyrics
  ( lyricsView
  , scrollLyrics
  , showLyrics
  , refetchLyrics
  , editLyrics
  , lyricsEdited
  , lyricsFetching
  , lyricsLoaded
  , updateLyrics
  , toggleLyricsFollowing
  ) where

import Control.Monad
import Data.Maybe
import Data.Text qualified as T
import Effectful
import Graphics.Vty qualified as V
import Optics.Core
import System.FilePath

import Reprise.Action
import Reprise.Config
import Reprise.Effect.UiRequest
import Reprise.Handler.Core
import Reprise.Lyrics
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Style

-- | The line being sung has the style of the song that plays in a list.
lyricsView :: AppEnv -> AppState -> View -> V.Image
lyricsView env s v =
  V.vertCat . map row . take v.height . drop (lyricsOffset s v) $
    lyricsRows v.width s.lyrics
  where
    row :: (Maybe Int, T.Text) -> V.Image
    row (line, text) =
      let style = if isJust line && line == sung then env.config.lists.playingStyle else mempty
      in V.text' (toAttr env.colorMode style) text

    sung :: Maybe Int
    sung = sungLine s

-- | Scroll the lyrics from where they show, which stops following the song.
scrollLyrics :: App es => MoveTarget -> Eff es ()
scrollLyrics t = do
  s <- getS
  modifyS $ #lyrics % #following .~ False
  modifyWithEnv . modifyView $ #offset .~ lyricsOffset s (focusedView s)
  scrollLines t

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

-- | Edit the stored lyrics of the song on the screen: the times if they
-- show, else the text. Without either, the text, which the editor makes.
editLyrics :: App es => Eff es ()
editLyrics = do
  env <- getAppEnv
  s <- getS
  case (s.lyrics.song, env.editor) of
    (Nothing, _) -> showMessage "There are no lyrics to edit"
    (_, Nothing) ->
      showMessage "There is no editor: set editor.command in the config, or $VISUAL or $EDITOR"
    (Just song, Just command) -> do
      let file = case s.lyrics.status of
            ShowingLyrics (LyricsFound _ lyrics) | isJust lyrics.timed -> timedLyricsFileName song
            _ -> lyricsFileName song
      editFile command (env.lyricsDirectory </> file)

-- | The editor of a file exited. If the file is of the lyrics on the
-- screen, they show again as the editor left them.
lyricsEdited :: App es => FilePath -> Maybe T.Text -> Eff es ()
lyricsEdited file failure = do
  env <- getAppEnv
  s <- getS
  forM_ failure showError
  forM_ s.lyrics.song $ \song ->
    when
      ( file
          `elem` map ((env.lyricsDirectory </>) . ($ song)) [lyricsFileName, timedLyricsFileName]
      ) $
      request song False s.lyrics.returnTo

-- | Ask for the lyrics of a song, which the screen shows when they come.
request :: App es => Song -> Bool -> ScreenName -> Eff es ()
request song refetch returnTo = do
  token <- newToken
  modifyS $
    #lyrics
      %~ (#song ?~ song)
      . (#token .~ token)
      . (#status .~ ReadingLyrics)
      . (#following .~ True)
      . (#returnTo .~ returnTo)
  fetchLyrics token (LyricsRequest song refetch)

-- | Turn following the song that plays on or off. On, the screen shows the
-- lyrics of the song that plays at once.
toggleLyricsFollowing :: App es => Eff es ()
toggleLyricsFollowing = do
  modifyS $ #toggles % #lyricsFollowPlaying %~ not
  s <- getS
  showMessage $
    "Lyrics follow playing: " <> if s.toggles.lyricsFollowPlaying then "on" else "off"
  forM_ (currentSong s.mirror) $ \playing ->
    when (s.toggles.lyricsFollowPlaying && not (maybe False (sameSong playing) s.lyrics.song)) $
      request playing False s.lyrics.returnTo

-- | Fetch the lyrics of each new song that plays in the background, if the
-- config says so, and show them on the lyrics screen while it follows the
-- song that plays. It runs after every event.
updateLyrics :: App es => Eff es ()
updateLyrics = do
  env <- getAppEnv
  s <- getS
  let playing = currentSong s.mirror
      changed = not (sameAs playing s.lyrics.playing)
  when (env.config.lyrics.fetchInBackground && not (null env.config.lyrics.fetchers)) $
    forM_ playing $ \p ->
      unless (sameAs (Just p) s.lyrics.inBackground) $ do
        modifyS $ #lyrics % #inBackground ?~ p
        fetchLyricsInBackground p
  when changed $ do
    modifyS $ #lyrics % #playing .~ playing
    -- Only a new song moves the screen, so that it can show another.
    forM_ playing $ \p ->
      when
        ( s.toggles.lyricsFollowPlaying
            && (focusedView s).screen == LyricsScreen
            && not (sameAs (Just p) s.lyrics.song)
        )
        $ request p False s.lyrics.returnTo
  where
    sameAs :: Maybe Song -> Maybe Song -> Bool
    sameAs a b = case (a, b) of
      (Just x, Just y) -> sameSong x y
      (Nothing, Nothing) -> True
      _ -> False

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
