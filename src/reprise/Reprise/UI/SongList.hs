-- | Rows of songs, in the classic display or in columns, and rows of other
-- items of a list, e.g. directories.
module Reprise.UI.SongList
  ( -- * Rows
    RowContext (..)
  , RowFlags (..)
  , renderRow
  , renderOtherRow
  , renderTitles
  , rowText

    -- * Spans
  , spansImage
  , padded

    -- * Columns
  , columnWidths
  ) where

import Data.List qualified as L
import Data.Maybe
import Data.Text qualified as T
import Graphics.Vty qualified as V

import Reprise.Config
import Reprise.Format
import Reprise.Mpd.Protocol.Types
import Reprise.Style

data RowContext = RowContext
  { colorMode :: ColorMode
  , lists :: ListsConfig
  , songs :: SongsConfig
  , display :: Display
  , width :: Int
  }

-- | What the row is, which lays styles over its own.
data RowFlags = RowFlags
  { queued :: Bool
  -- ^ The song is in the queue, which screens other than the queue show.
  , playing :: Bool
  , selected :: Bool
  , found :: Bool
  , cursor :: Bool
  }

-- | A row of the given width.
renderRow :: RowContext -> RowFlags -> Song -> V.Image
renderRow ctx flags song = case ctx.display of
  Classic ->
    let left = render ctx.songs.classic.left
        right = render ctx.songs.classic.right
        rightWidth = spansWidth right
        gap = if rightWidth > 0 then 1 else 0
        leftWidth = max 0 (ctx.width - rightWidth - gap)
    in V.horizCat
         [ padded attr styles AlignLeft leftWidth (fitSpans leftWidth left)
         , padded attr styles AlignLeft gap []
         , padded attr styles AlignLeft (ctx.width - leftWidth - gap) right
         ]
  Columns ->
    V.horizCat . L.intersperse (padded attr styles AlignLeft 1 []) $
      zipWith column ctx.songs.columns.list (columnWidths ctx.width ctx.songs.columns.list)
  where
    attr :: Style -> V.Attr
    attr = toAttr ctx.colorMode

    render :: Format Style -> [Span Style]
    render = renderFormat (renderContext ctx.lists) song

    -- A column's color tells what the column is, so the marker of a
    -- missing tag takes the column's style rather than its own.
    column :: Column -> Int -> V.Image
    column c w =
      let base = ctx.lists.style <> c.style
          columnContext =
            RenderContext ctx.lists.tagSeparator [Span Nothing ctx.lists.missingTag]
      in padded attr (base, overlay) c.align w . fitSpans w $
           renderFormat columnContext song c.format

    styles :: (Style, Style)
    styles = (ctx.lists.style, overlay)

    overlay :: Style
    overlay = overlayStyle ctx.lists flags

-- | A row of an item that isn't a song, e.g. a directory.
renderOtherRow :: RowContext -> RowFlags -> [Span Style] -> V.Image
renderOtherRow ctx flags =
  padded
    (toAttr ctx.colorMode)
    (ctx.lists.style, overlayStyle ctx.lists flags)
    AlignLeft
    ctx.width
    . fitSpans ctx.width

-- | The styles that a row's state lays over its own.
overlayStyle :: ListsConfig -> RowFlags -> Style
overlayStyle lists flags =
  mconcat
    [ if flags.queued then lists.queuedStyle else mempty
    , if flags.playing then lists.playingStyle else mempty
    , if flags.selected then lists.selectedStyle else mempty
    , if flags.found then lists.foundStyle else mempty
    , if flags.cursor then lists.cursorStyle else mempty
    ]

-- | The text of a song's row without styles, which find matches. A missing
-- tag is empty rather than its marker.
rowText :: ListsConfig -> SongsConfig -> Display -> Song -> T.Text
rowText lists songs display song = T.unwords . map render $ case display of
  Classic -> [songs.classic.left, songs.classic.right]
  Columns -> map (.format) songs.columns.list
  where
    render :: Format Style -> T.Text
    render = spansText . renderFormat (RenderContext lists.tagSeparator []) song

-- | The titles of the columns.
renderTitles :: RowContext -> V.Image
renderTitles ctx =
  V.horizCat . L.intersperse (padded attr (ctx.lists.style, mempty) AlignLeft 1 []) $
    zipWith title ctx.songs.columns.list (columnWidths ctx.width ctx.songs.columns.list)
  where
    attr :: Style -> V.Attr
    attr = toAttr ctx.colorMode

    title :: Column -> Int -> V.Image
    title c w =
      padded attr (ctx.lists.style <> c.style, mempty) c.align w $
        fitSpans w [Span Nothing c.title]

renderContext :: ListsConfig -> RenderContext Style
renderContext lists = RenderContext lists.tagSeparator [Span (Just lists.missingTagStyle) lists.missingTag]

-- | Spans in a cell of exactly the given width, aligned and padded with
-- spaces. Each span's style is the base, its own style, then the overlay.
padded :: (Style -> V.Attr) -> (Style, Style) -> Align -> Int -> [Span Style] -> V.Image
padded attr (base, overlay) align w spans =
  let fill = V.charFill (attr (base <> overlay)) ' ' (max 0 (w - spansWidth spans)) 1
      content = spansImage attr base overlay spans
  in case align of
       AlignLeft -> content V.<|> fill
       AlignRight -> fill V.<|> content

spansImage :: (Style -> V.Attr) -> Style -> Style -> [Span Style] -> V.Image
spansImage attr base overlay = V.horizCat . map image
  where
    image :: Span Style -> V.Image
    image s = V.text' (attr (base <> fromMaybe mempty s.style <> overlay)) (sanitize s.text)

-- | Control characters would break the terminal's layout.
sanitize :: T.Text -> T.Text
sanitize = T.map $ \c -> if c < ' ' then ' ' else c

-- | The widths of the columns for a row width. Fixed columns get their
-- width, relative ones share the rest by their percentages, and a space
-- separates the columns.
columnWidths :: Int -> [Column] -> [Int]
columnWidths width columns =
  let separators = max 0 (length columns - 1)
      fixed = sum [w | FixedWidth w <- map (.width) columns]
      rest = max 0 (width - separators - fixed)
      percents = sum [p | RelativeWidth p <- map (.width) columns]
      shares =
        [ if percents > 0 then rest * p `div` percents else 0
        | RelativeWidth p <- map (.width) columns
        ]
      leftover = rest - sum shares
      lastRelative = length shares - 1
      relative = zipWith (\i s -> if i == lastRelative then s + leftover else s) [0 ..] shares
  in assign relative columns
  where
    assign :: [Int] -> [Column] -> [Int]
    assign rel = \case
      [] -> []
      c : cs -> case c.width of
        FixedWidth w -> w : assign rel cs
        RelativeWidth _ -> case rel of
          r : rs -> r : assign rs cs
          [] -> 0 : assign [] cs
