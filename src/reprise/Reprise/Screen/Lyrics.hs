-- | The lyrics screen: the lyrics of a song as text that scrolls, which a
-- worker reads from the directory of lyrics, or fetches.
module Reprise.Screen.Lyrics
  ( lyricsView
  , lyricsVerb
  , showLyrics
  , editLyricsFile
  , lyricsEdited
  , lyricsFetching
  , lyricsLoaded
  , updateLyrics
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
import Reprise.Event
import Reprise.Handler.Core
import Reprise.Lyrics
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Style

-- | The line being sung also has the style of the song that plays in a list.
lyricsView :: AppEnv -> AppState -> View -> V.Image
lyricsView env s v =
  V.vertCat . map row . take v.height . drop (lyricsOffset s v) $
    lyricsRows v.width s.lyrics
  where
    row :: (Maybe Int, T.Text) -> V.Image
    row (line, text) =
      let playing = if isJust line && line == sung then env.config.lists.playingStyle else mempty
      in V.text' (toAttr env.colorMode (env.config.styles.text <> playing)) text

    sung :: Maybe Int
    sung = sungLine s

-- | How the lyrics screen does a verb, if it does it.
lyricsVerb :: App es => Action -> Maybe (Eff es ())
lyricsVerb = \case
  Move t -> Just $ scrollLyrics t
  JumpToPlaying -> Just jumpToPlayingLyrics
  EditLyrics -> Just editLyrics
  RefetchLyrics -> Just refetchLyrics
  Toggle ToggleFollowPlaying -> Just toggleLyricsFollowing
  _ -> Nothing

-- | Scroll the lyrics from where they show, which stops following the song.
scrollLyrics :: App es => MoveTarget -> Eff es ()
scrollLyrics t = do
  s <- getS
  modifyS $ #lyrics % #following .~ False
  modifyWithEnv . modifyView $ #offset .~ lyricsOffset s (focusedView s)
  scrollLines t

-- | Show the lyrics of the song under the cursor, from the top.
showLyrics :: App es => Eff es ()
showLyrics = showSongScreen LyricsScreen $ \song -> request song False

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
          request song True
          modifyWithEnv . modifyView $ #offset .~ 0

-- | Edit the stored lyrics of the song on the screen: the times if they
-- show, else the text. Without stored lyrics, a choice asks which file to
-- make. Lyrics on their way would be stored over the file, so they are
-- waited for.
editLyrics :: App es => Eff es ()
editLyrics = do
  env <- getAppEnv
  s <- getS
  case (s.lyrics.song, env.editor) of
    (Nothing, _) -> showMessage "There are no lyrics to edit"
    (_, Nothing) -> showMessage noEditor
    (Just song, Just command) -> case s.lyrics.status of
      ShowingLyrics (LyricsFound _ lyrics) ->
        editFile command . (env.lyricsDirectory </>) $
          if isJust lyrics.timed then timedLyricsFileName song else lyricsFileName song
      ShowingLyrics _ ->
        let option :: Char -> T.Text -> (Song -> FilePath) -> ChoiceOption
            option letter name file =
              ChoiceOption letter name (Just (EditLyricsFile (env.lyricsDirectory </> file song)))
        in modifyS $
             #prompt
               ?~ Prompt
                 "Edit which lyrics?"
                 (Choice [option 's' "synced" timedLyricsFileName, option 'u' "unsynced" lyricsFileName])
      _ -> showMessage "The lyrics are still loading"

-- | Edit a file of lyrics that the user chose.
editLyricsFile :: App es => FilePath -> Eff es ()
editLyricsFile file = do
  env <- getAppEnv
  maybe (showMessage noEditor) (`editFile` file) env.editor

noEditor :: T.Text
noEditor = "There is no editor: set editor.command in the config, or $VISUAL or $EDITOR"

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
      )
      $ request song False

-- | Ask for the lyrics of a song, which the screen shows when they come.
request :: App es => Song -> Bool -> Eff es ()
request song refetch = do
  token <- newToken
  modifyS $
    #lyrics
      %~ (#song ?~ song)
      . (#token .~ token)
      . (#status .~ ReadingLyrics)
      . (#following .~ True)
  fetchLyrics token (LyricsRequest song refetch)

-- | Follow the line being sung again, or show the lyrics of the song that
-- plays if the screen shows another song's.
jumpToPlayingLyrics :: App es => Eff es ()
jumpToPlayingLyrics = do
  s <- getS
  case currentSong s.mirror of
    Nothing -> showMessage "No song is playing"
    Just playing
      | maybe False (sameSong playing) s.lyrics.song -> modifyS $ #lyrics % #following .~ True
      | otherwise -> request playing False

-- | Turn following the song that plays on or off. On, the screen shows the
-- lyrics of the song that plays at once.
toggleLyricsFollowing :: App es => Eff es ()
toggleLyricsFollowing = do
  toggleSetting "Lyrics follow playing" #lyricsFollowPlaying
  s <- getS
  forM_ (currentSong s.mirror) $ \playing ->
    when (s.toggles.lyricsFollowPlaying && not (maybe False (sameSong playing) s.lyrics.song)) $
      request playing False

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
        $ request p False
  where
    sameAs :: Maybe Song -> Maybe Song -> Bool
    sameAs a b = case (a, b) of
      (Just x, Just y) -> sameSong x y
      (Nothing, Nothing) -> True
      _ -> False

-- | The lyrics of the request with the token aren't stored, so a fetcher
-- with the name is asked for them.
lyricsFetching :: App es => Int -> T.Text -> Eff es ()
lyricsFetching token fetcher =
  forRequest token $ modifyS (#lyrics % #status .~ FetchingLyrics fetcher)

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
