-- | The whole screen: the header, the main view, the progress bar and the
-- status bar, with the which-key panel over the bottom of the main view.
module Reprise.UI.Layout
  ( renderScreen
  , promptCursor
  ) where

import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V

import Reprise.Action
import Reprise.Config
import Reprise.Format
import Reprise.Header
import Reprise.Keymap
import Reprise.Keys
import Reprise.LineEdit
import Reprise.Mpd.Mirror
import Reprise.Mpd.Protocol.Types
import Reprise.Screen.Browser
import Reprise.Screen.Help
import Reprise.Screen.Lyrics
import Reprise.Screen.Outputs
import Reprise.Screen.Queue
import Reprise.Screen.SongInfo
import Reprise.Screen.Visualizer
import Reprise.State
import Reprise.StatusBar
import Reprise.Style
import Reprise.UI.SongList
import Reprise.Width

-- | The screen as one image of the terminal's size.
renderScreen :: AppEnv -> AppState -> V.Image
renderScreen env s
  | w <= 0 || h <= 0 = V.emptyImage
  | otherwise =
      V.crop w h . V.vertCat $
        [ headerTitle env s
        , headerLine env s
        , withPanel env s (mainView env s)
        , progressBar env s
        , statusBar env s
        ]
  where
    (w, h) = s.terminalSize

----------------------------------------
-- Header

headerTitle :: AppEnv -> AppState -> V.Image
headerTitle env s = line env s env.config.styles.header.normal left right
  where
    left :: [Span Style]
    left = [Span (Just env.config.styles.header.title) (shownTitle env s)]

    right :: [Span Style]
    right = [Span (Just env.config.styles.header.volume) (headerRight s)]

headerLine :: AppEnv -> AppState -> V.Image
headerLine env s =
  let (w, _) = s.terminalSize
      lineAttr = attr env env.config.styles.header.line
      flagsText = flags s
      rule n = V.charFill lineAttr '─' n 1
  in if T.null flagsText
       then rule w
       else
         -- The brackets are part of the line, and the line goes on for a
         -- column after them, as in ncmpcpp.
         let flagsImage =
               V.horizCat
                 [ V.text' lineAttr "["
                 , V.text' (attr env env.config.styles.header.flags) flagsText
                 , V.text' lineAttr "]"
                 ]
             margin = 1
         in V.crop w 1 $
              V.horizCat
                [ rule (max 0 (w - V.imageWidth flagsImage - margin))
                , flagsImage
                , rule margin
                ]

flags :: AppState -> T.Text
flags s = case s.mirror.status of
  Nothing -> ""
  Just st ->
    T.pack $
      concat
        [ ['r' | st.repeat]
        , ['z' | st.random]
        , ['s' | st.single /= SingleOff]
        , ['c' | st.consume /= ConsumeOff]
        , ['x' | st.crossfade > 0]
        , ['U' | isJust st.updatingDb]
        ]

----------------------------------------
-- Main view

mainView :: AppEnv -> AppState -> V.Image
mainView env s =
  let v = focusedView s
      (w, _) = s.terminalSize
      h = mainHeight s.terminalSize
  in V.resize w h $ case v.screen of
       QueueScreen -> queueView env s v
       BrowserScreen -> browserView env s v
       VisualizerScreen -> visualizerView env s v
       LyricsScreen -> lyricsView env s v
       SongInfoScreen -> songInfoView env s v
       HelpScreen -> helpView env s v
       OutputsScreen -> outputsView env s v
       SearchEngineScreen -> V.emptyImage
       MediaLibraryScreen -> V.emptyImage
       PlaylistEditorScreen -> V.emptyImage

-- | The which-key panel over the bottom rows of the main view, while a key
-- sequence is pending.
withPanel :: AppEnv -> AppState -> V.Image -> V.Image
withPanel env s mainImage = case s.pendingKeys of
  Nothing -> mainImage
  Just pending ->
    let h = mainHeight s.terminalSize
        panel = whichKeyPanel env s h (whichKeyEntries pending.layers)
        panelHeight = min h (V.imageHeight panel)
    in V.cropBottom (h - panelHeight) mainImage V.<-> V.cropTop panelHeight panel

-- | The entries in columns, in at most a number of rows. Entries that don't
-- fit make the last cell say how many more there are, which the help
-- screen lists.
whichKeyPanel :: AppEnv -> AppState -> Int -> [WhichKeyEntry] -> V.Image
whichKeyPanel env s maxRows entries =
  let (w, _) = s.terminalSize
      cells = map cell entries
      cellWidth = maximum (1 : map fst cells) + gap
      columns = max 1 (w `div` cellWidth)
      rows = max 1 (min maxRows ((length cells + columns - 1) `div` columns))
      room = rows * columns
      shown
        | length cells <= room = cells
        | otherwise = take (room - 1) cells <> [more (length cells - room + 1)]
      byColumn = chunks rows shown
      columnImage c = V.vertCat [V.resizeWidth cellWidth img | (_, img) <- c]
  in V.resize w rows $ V.horizCat (map columnImage byColumn)
  where
    gap :: Int
    gap = 2

    more :: Int -> (Int, V.Image)
    more n =
      let img = V.text' (attr env mempty) ("+" <> T.pack (show n) <> " more")
      in (V.imageWidth img, img)

    cell :: WhichKeyEntry -> (Int, V.Image)
    cell e =
      let keyText = renderKeySpec e.key
          -- The screen's own entries are bold.
          style = if e.fromScreen then boldStyle else mempty
          img =
            V.text' (attr env (env.config.styles.value <> style)) keyText
              V.<|> V.text' (attr env style) ("  " <> e.description)
      in (V.imageWidth img, img)

    chunks :: Int -> [a] -> [[a]]
    chunks n xs = case splitAt n xs of
      (c, []) -> [c | not (null c)]
      (c, rest) -> c : chunks n rest

----------------------------------------
-- Progress bar

progressBar :: AppEnv -> AppState -> V.Image
progressBar env s =
  let (w, _) = s.terminalSize
      cfg = env.config.progressBar
      remainingAttr = attr env env.config.styles.progressBar.normal
      elapsedAttr = attr env env.config.styles.progressBar.elapsed
      filled = do
        d <- progressDuration s
        e <- displayedElapsed s
        pure $ progressCells w (realToFrac e) (realToFrac d)
  in case filled of
       Nothing -> V.charFill remainingAttr cfg.chars.remaining w 1
       Just done ->
         let current = if done < w then 1 else 0
         in V.horizCat
              [ V.charFill elapsedAttr cfg.chars.elapsed done 1
              , V.charFill elapsedAttr cfg.chars.current current 1
              , V.charFill remainingAttr cfg.chars.remaining (w - done - current) 1
              ]

----------------------------------------
-- Status bar

statusBar :: AppEnv -> AppState -> V.Image
statusBar env s = case statusContent s of
  StatusPrompt (Prompt question (Choice options)) ->
    line
      env
      s
      styles.normal
      (Span Nothing (question <> " [") : choices options <> [Span Nothing "]"])
      []
  StatusPrompt (Prompt question (Line edit purpose _)) ->
    let p = promptLine s question edit purpose
    in line
         env
         s
         styles.normal
         [Span Nothing question, Span Nothing p.shown]
         [Span Nothing p.note]
  StatusPending pending -> textLine (T.unwords (map renderKeySpec pending.keys) <> " -")
  StatusMessage m ->
    line
      env
      s
      styles.normal
      [Span (if m.isError then Just styles.error else Nothing) m.text]
      []
  StatusPlayer -> playerStatus env s
  where
    styles :: StatusBarStyles
    styles = env.config.styles.statusBar

    textLine :: T.Text -> V.Image
    textLine t = line env s styles.normal [Span Nothing t] []

    -- The names of the options, with the letter that picks each in bold.
    choices :: [ChoiceOption] -> [Span Style]
    choices options =
      L.intercalate
        [Span Nothing "/"]
        [ [Span Nothing before, Span (Just boldStyle) (T.take 1 rest), Span Nothing (T.drop 1 rest)]
        | o <- options
        , let (before, rest) = T.breakOn (T.singleton o.letter) o.name
        ]

-- | What of a line prompt shows: the part of the line that fits next to
-- the question and the note, the column of the cursor in the status bar,
-- and the note.
data PromptLine = PromptLine
  { shown :: T.Text
  , cursorColumn :: Int
  , note :: T.Text
  }

promptLine :: AppState -> T.Text -> LineEdit -> LinePurpose -> PromptLine
promptLine s question edit purpose =
  let w = fst s.terminalSize
      fullNote = case purpose of
        ForCommand -> actionHint (lineEditText edit)
        ForFind f -> fromMaybe "" f.note
        ForSave _ -> ""
        ForPassword -> ""
      shownEdit = case purpose of
        ForPassword -> LineEdit (stars edit.before) (stars edit.after)
        _ -> edit
      -- The line and a column for the cursor come first, then a space and
      -- as much of the note as fits.
      noteRoom = w - textWidth question - textWidth (lineEditText shownEdit) - 2
      note
        | T.null fullNote || noteRoom < textWidth ellipsis = ""
        | otherwise = " " <> truncateToWidth noteRoom fullNote
      room = max 1 (w - textWidth question - textWidth note)
      (shown, column) = visibleLine room shownEdit
  in PromptLine shown (textWidth question + column) note
  where
    stars :: T.Text -> T.Text
    stars = T.map (const '*')

-- | Where the terminal's cursor shows: in a line prompt, at the cursor of
-- its line.
promptCursor :: AppState -> Maybe (Int, Int)
promptCursor s = case s.prompt of
  Just (Prompt question (Line edit purpose _)) ->
    Just ((promptLine s question edit purpose).cursorColumn, snd s.terminalSize - 1)
  _ -> Nothing

playerStatus :: AppEnv -> AppState -> V.Image
playerStatus env s = case (s.mirror.status, currentSong s.mirror, playerLabel s) of
  (Just st, Just song, Just label) ->
    let e = fromMaybe 0 (displayedElapsed s)
        time = case st.duration of
          Just d
            | cfg.showRemainingTime ->
                "[-" <> formatDuration (max 0 (d - e)) <> "/" <> formatDuration d <> "]"
            | otherwise -> "[" <> formatDuration e <> "/" <> formatDuration d <> "]"
          Nothing -> "[" <> formatDuration e <> "]"
        bitrate = case st.bitrate of
          Just b | s.toggles.showBitrate && b > 0 -> T.pack (show b) <> " kbps "
          _ -> ""
        right = [Span Nothing bitrate, Span (Just styles.time) time]
        room = max 0 (fst s.terminalSize - textWidth label - spansWidth right - 1)
        songSpans = renderFormat (renderContext env.config.songs env.config.styles) song cfg.song
        shown
          | spansWidth songSpans <= room = songSpans
          | otherwise = [Span Nothing (scrollText room (floor e) (spansText songSpans))]
    in line env s styles.normal (Span (Just styles.state) label : shown) right
  _ -> line env s styles.normal [] []
  where
    cfg :: StatusBarConfig
    cfg = env.config.statusBar

    styles :: StatusBarStyles
    styles = env.config.styles.statusBar

----------------------------------------
-- Helpers

-- | A line of the terminal's width with spans on the left and on the right,
-- over a base style.
line :: AppEnv -> AppState -> Style -> [Span Style] -> [Span Style] -> V.Image
line env s base left right =
  let (w, _) = s.terminalSize
      rightWidth = min w (spansWidth right)
      leftWidth = max 0 (w - rightWidth)
      a = attr env
  in V.horizCat
       [ padded a (base, mempty) AlignLeft leftWidth (fitSpans leftWidth left)
       , padded a (base, mempty) AlignRight rightWidth (fitSpans rightWidth right)
       ]

attr :: AppEnv -> Style -> V.Attr
attr env = toAttr env.colorMode
