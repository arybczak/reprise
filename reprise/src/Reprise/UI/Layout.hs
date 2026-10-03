-- | The whole screen: the header, the main view, the progress bar and the
-- status bar, with the which-key panel over the bottom of the main view.
module Reprise.UI.Layout
  ( renderScreen
  , formatTotal
  ) where

import Data.Foldable
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Word
import Graphics.Vty qualified as V
import MPD.Types
import Optics.Core

import Reprise.Action
import Reprise.Config
import Reprise.Format
import Reprise.Handler
import Reprise.Keymap
import Reprise.Keys
import Reprise.Mpd.Mirror
import Reprise.State
import Reprise.Style
import Reprise.UI.SongList

-- | The screen as one image of the terminal's size.
renderScreen :: AppState -> V.Image
renderScreen s
  | w <= 0 || h <= 0 = V.emptyImage
  | otherwise =
      V.crop w h . V.vertCat $
        [ headerTitle s
        , headerLine s
        , withPanel s (mainView s)
        , progressBar s
        , statusBar s
        ]
  where
    (w, h) = s.terminalSize

----------------------------------------
-- Header

headerTitle :: AppState -> V.Image
headerTitle s = line s s.config.header.style left right
  where
    left :: [Span Style]
    left = [Span Nothing (screenTitle s (focusedView s).screen)]

    right :: [Span Style]
    right = [Span (Just s.config.header.volumeStyle) state]

    state :: T.Text
    state = case s.connection of
      Connecting -> "Connecting…"
      Disconnected _ -> "Disconnected"
      Connected _ -> case s.mirror.status >>= (.volume) of
        Just v -> "Volume: " <> T.pack (show v) <> "%"
        Nothing -> "Volume: n/a"

screenTitle :: AppState -> ScreenName -> T.Text
screenTitle s = \case
  QueueScreen ->
    let q = s.mirror.queue
        total = sum (mapMaybe (.duration) (toList q))
        remaining = case (currentPosition s.mirror, displayedElapsed s) of
          (Just p, e) ->
            sum (mapMaybe (.duration) (toList (Seq.drop p q))) - fromMaybe 0 e
          _ -> total
        count = T.pack (show (Seq.length q)) <> if Seq.length q == 1 then " song" else " songs"
        times =
          [formatTotal total | total > 0]
            <> [formatTotal remaining <> " left" | s.config.queue.showRemainingTime, remaining > 0]
    in "Queue (" <> T.intercalate ", " (count : times) <> ")"
  other -> T.toTitle (T.replace "_" " " (screenName other))

-- | A short total, e.g. @1h 23m@.
formatTotal :: Seconds -> T.Text
formatTotal secs =
  let total = floor secs :: Int
      (d, r1) = total `divMod` 86400
      (h, r2) = r1 `divMod` 3600
      (m, sec) = r2 `divMod` 60
      parts = [(d, "d"), (h, "h"), (m, "m"), (sec, "s")]
      significant = take 2 $ dropWhile ((== 0) . fst) parts
  in case significant of
       [] -> "0s"
       _ -> T.unwords [T.pack (show n) <> unit | (n, unit) <- significant, n > 0]

headerLine :: AppState -> V.Image
headerLine s =
  let (w, _) = s.terminalSize
      flagsText = flags s
      flagsImage
        | T.null flagsText = V.emptyImage
        | otherwise = V.text' (attr s s.config.header.flagsStyle) ("[" <> flagsText <> "]")
      lineWidth = max 0 (w - V.imageWidth flagsImage)
  in V.charFill (attr s s.config.header.lineStyle) '─' lineWidth 1 V.<|> flagsImage

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

mainView :: AppState -> V.Image
mainView s =
  let v = focusedView s
      (w, _) = s.terminalSize
      h = mainHeight s.terminalSize
  in V.resize w h $ case v.screen of
       QueueScreen -> queueView s v
       _ -> V.emptyImage

queueView :: AppState -> View -> V.Image
queueView s v =
  let ctx =
        RowContext
          { colorMode = s.colorMode
          , lists = s.config.lists
          , songs = s.config.songs
          , display = s.toggles.queueDisplay
          , width = v.width
          }
      titles
        | s.toggles.queueDisplay == Columns && s.config.songs.columns.showTitles =
            [renderTitles ctx]
        | otherwise = []
      playingId = s.mirror.status >>= (.currentId)
      visible = Seq.take (listHeight s v) (Seq.drop v.offset s.mirror.queue)
      row i song =
        renderRow
          ctx
          RowFlags
            { playing = isJust playingId && song.songId == playingId
            , selected = False
            , cursor = i == v.cursor && cursorVisible s
            }
          song
  in V.vertCat $ titles <> zipWith row [v.offset ..] (toList visible)

-- | The which-key panel over the bottom rows of the main view, while a key
-- sequence is pending.
withPanel :: AppState -> V.Image -> V.Image
withPanel s mainImage = case s.pendingKeys of
  Nothing -> mainImage
  Just pending ->
    let panel = whichKeyPanel s (whichKeyEntries pending.layers)
        h = mainHeight s.terminalSize
        panelHeight = min h (V.imageHeight panel)
    in V.cropBottom (h - panelHeight) mainImage V.<-> V.cropTop panelHeight panel

whichKeyPanel :: AppState -> [WhichKeyEntry] -> V.Image
whichKeyPanel s entries =
  let (w, _) = s.terminalSize
      cells = map cell entries
      cellWidth = maximum (1 : map fst cells) + gap
      columns = max 1 (w `div` cellWidth)
      rows = max 1 ((length cells + columns - 1) `div` columns)
      byColumn = chunks rows cells
      columnImage c = V.vertCat [V.resizeWidth cellWidth img | (_, img) <- c]
  in V.resize w rows $ V.horizCat (map columnImage byColumn)
  where
    gap :: Int
    gap = 2

    cell :: WhichKeyEntry -> (Int, V.Image)
    cell e =
      let keyText = renderKeySpec e.key
          style = if e.fromScreen then boldStyle else mempty
          img =
            V.text' (attr s (s.config.styles.value <> style)) keyText
              V.<|> V.text' (attr s style) ("  " <> e.description)
      in (V.imageWidth img, img)

    -- The screen's own entries.
    boldStyle :: Style
    boldStyle = mempty & #attributes .~ S.singleton Bold

    chunks :: Int -> [a] -> [[a]]
    chunks n xs = case splitAt n xs of
      (c, []) -> [c | not (null c)]
      (c, rest) -> c : chunks n rest

----------------------------------------
-- Progress bar

progressBar :: AppState -> V.Image
progressBar s =
  let (w, _) = s.terminalSize
      cfg = s.config.progressBar
      remainingAttr = attr s cfg.style
      elapsedAttr = attr s cfg.elapsedStyle
      fraction = do
        st <- s.mirror.status
        guardMaybe (st.state /= Stopped)
        d <- st.duration
        guardMaybe (d > 0)
        e <- displayedElapsed s
        pure (realToFrac (min e d / d) :: Double)
  in case fraction of
       Nothing -> V.charFill remainingAttr cfg.chars.remaining w 1
       Just f ->
         let done = min w (floor (f * fromIntegral w))
             current = if done < w then 1 else 0
         in V.horizCat
              [ V.charFill elapsedAttr cfg.chars.elapsed done 1
              , V.charFill elapsedAttr cfg.chars.current current 1
              , V.charFill remainingAttr cfg.chars.remaining (w - done - current) 1
              ]
  where
    guardMaybe :: Bool -> Maybe ()
    guardMaybe b = if b then Just () else Nothing

----------------------------------------
-- Status bar

statusBar :: AppState -> V.Image
statusBar s = case (s.prompt, s.pendingKeys, s.message) of
  (Just (Confirm question _), _, _) -> plain (question <> " [y/n]")
  (_, Just pending, _) -> plain (T.unwords (map renderKeySpec pending.keys) <> " -")
  (_, _, Just m) -> line s cfg.style [Span (if m.isError then Just errorStyle else Nothing) m.text] []
  _ -> playerStatus s
  where
    cfg :: StatusBarConfig
    cfg = s.config.statusBar

    plain :: T.Text -> V.Image
    plain t = line s cfg.style [Span Nothing t] []

    errorStyle :: Style
    errorStyle = mempty & #foreground ?~ Color red

    -- The number of red in the terminal's color chart.
    red :: Word8
    red = 1

playerStatus :: AppState -> V.Image
playerStatus s = case (s.mirror.status, currentSong s.mirror) of
  (Just st, Just song)
    | st.state /= Stopped ->
        let label = case st.state of
              Playing -> "Playing: "
              _ -> "Paused: "
            e = fromMaybe 0 (displayedElapsed s)
            time = case st.duration of
              Just d
                | cfg.showRemainingTime ->
                    "[-" <> formatDuration (max 0 (d - e)) <> "/" <> formatDuration d <> "]"
                | otherwise -> "[" <> formatDuration e <> "/" <> formatDuration d <> "]"
              Nothing -> "[" <> formatDuration e <> "]"
            bitrate = case st.bitrate of
              Just b | s.toggles.showBitrate && b > 0 -> T.pack (show b) <> " kbps "
              _ -> ""
            right = [Span Nothing bitrate, Span (Just cfg.timeStyle) time]
            room = max 0 (fst s.terminalSize - textWidth label - spansWidth right - 1)
            songSpans = renderFormat ctx song cfg.song
            shown
              | spansWidth songSpans <= room = songSpans
              | otherwise = [Span Nothing (scroll room (floor e) (spansText songSpans))]
        in line s cfg.style (Span (Just cfg.stateStyle) label : shown) right
  _ -> line s cfg.style [] []
  where
    cfg :: StatusBarConfig
    cfg = s.config.statusBar

    ctx :: RenderContext Style
    ctx =
      RenderContext
        s.config.lists.tagSeparator
        [Span (Just s.config.lists.missingTagStyle) s.config.lists.missingTag]

-- | Text that doesn't fit, scrolled by one character for each second.
scroll :: Int -> Int -> T.Text -> T.Text
scroll room seconds t =
  let looped = t <> separator
      n = T.length looped
      start = seconds `mod` max 1 n
  in takeWidth room (T.drop start looped <> looped)
  where
    separator :: T.Text
    separator = " ** "

----------------------------------------
-- Helpers

-- | A line of the terminal's width with spans on the left and on the right,
-- over a base style.
line :: AppState -> Style -> [Span Style] -> [Span Style] -> V.Image
line s base left right =
  let (w, _) = s.terminalSize
      rightWidth = min w (spansWidth right)
      leftWidth = max 0 (w - rightWidth)
      a = attr s
  in V.horizCat
       [ padded a (base, mempty) AlignLeft leftWidth (fitSpans leftWidth left)
       , padded a (base, mempty) AlignRight rightWidth (fitSpans rightWidth right)
       ]

attr :: AppState -> Style -> V.Attr
attr s = toAttr s.colorMode
