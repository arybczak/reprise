-- | What the song info screen shows of a song, as ncmpcpp does: its file,
-- its audio, its ReplayGain and its tags.
module Reprise.SongInfo
  ( InfoLine (..)
  , songInfoLines
  , infoRows
  ) where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Time
import System.FilePath
import Text.Read

import Reprise.Format
import Reprise.Mpd.Protocol.Types
import Reprise.Width

data InfoLine
  = InfoBlank
  | -- | A label and its value, which a song can be without, e.g. a tag.
    InfoField T.Text (Maybe T.Text)
  deriving stock (Eq, Show)

-- | The lines of a song, by the separator of a tag's values, the bitrate if
-- the song plays, as MPD has it of no other song, and the comments of its
-- file.
songInfoLines :: T.Text -> Maybe Int -> [(T.Text, T.Text)] -> Song -> [InfoLine]
songInfoLines separator bitrate comments song =
  L.intercalate [InfoBlank] . filter (not . null) $ [file, audio, replayGain, tags]
  where
    file :: [InfoLine]
    file =
      [ InfoField "Filename" (Just (T.pack (takeFileName path)))
      , InfoField "Directory" (case takeDirectory path of "." -> Nothing; d -> Just (T.pack d))
      ]
        <> [InfoField "Part" (Just (part r)) | Just r <- [song.range]]

    path :: FilePath
    path = T.unpack song.file

    part :: SongRange -> T.Text
    part r = formatDuration r.start <> " to " <> maybe "the end" formatDuration r.end

    audio :: [InfoLine]
    audio =
      [InfoField "Length" (formatDuration <$> song.duration)]
        <> [InfoField "Bitrate" (Just (T.pack (show b) <> " kbps")) | Just b <- [bitrate]]
        <> maybe [] audioFormat song.format
        <> [ InfoField
               "Last modified"
               (Just (T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" t)))
           | Just t <- [song.lastModified]
           ]

    -- MPD's format of a song, e.g. @44100:16:2@, of which the bits can be
    -- @f@ for floating point. Another one, e.g. of DSD, shows as it is.
    audioFormat :: T.Text -> [InfoLine]
    audioFormat format = case T.splitOn ":" format of
      [rate, bits, channels]
        | Just r <- readMaybe @Int (T.unpack rate)
        , Just c <- readMaybe @Int (T.unpack channels)
        , Just b <-
            if bits == "f" then Just "32 bit floating point" else (<> " bit") <$> number bits ->
            [ InfoField "Sample rate" (Just (T.pack (show r) <> " Hz"))
            , InfoField "Sample format" (Just b)
            , InfoField "Channels" (Just (channelsText c))
            ]
      _ -> [InfoField "Format" (Just format)]

    number :: T.Text -> Maybe T.Text
    number t = t <$ readMaybe @Int (T.unpack t)

    channelsText :: Int -> T.Text
    channelsText = \case
      1 -> "Mono"
      2 -> "Stereo"
      n -> T.pack (show n) <> " channels"

    -- ReplayGain's comments, as ncmpcpp reads them from the file.
    replayGain :: [InfoLine]
    replayGain =
      [ InfoField label (Just value)
      | (name, label) <-
          [ ("REPLAYGAIN_REFERENCE_LOUDNESS", "Reference loudness")
          , ("REPLAYGAIN_TRACK_GAIN", "Track gain")
          , ("REPLAYGAIN_TRACK_PEAK", "Track peak")
          , ("REPLAYGAIN_ALBUM_GAIN", "Album gain")
          , ("REPLAYGAIN_ALBUM_PEAK", "Album peak")
          ]
      , Just value <- [lookup name [(T.toUpper k, v) | (k, v) <- comments]]
      ]

    -- ncmpcpp's tags, also when the song is without them, then the others
    -- that it has.
    tags :: [InfoLine]
    tags =
      [InfoField label (values t) | (t, label) <- ncmpcppTags]
        <> [ InfoField (tagName t) (values t)
           | t <- M.keys song.tags
           , t `notElem` map fst ncmpcppTags
           ]

    values :: Tag -> Maybe T.Text
    values t = case filter (not . T.null) (M.findWithDefault [] t song.tags) of
      [] -> Nothing
      vs -> Just (T.intercalate separator vs)

    ncmpcppTags :: [(Tag, T.Text)]
    ncmpcppTags =
      [ (Title, "Title")
      , (Artist, "Artist")
      , (AlbumArtist, "Album Artist")
      , (Album, "Album")
      , (Date, "Date")
      , (Track, "Track")
      , (Genre, "Genre")
      , (Composer, "Composer")
      , (Performer, "Performer")
      , (Disc, "Disc")
      , (Comment, "Comment")
      ]

-- | The rows of lines at a width: a label, as wide as the widest, and a
-- value, wrapped below the first row's. A value that the song is without
-- is Nothing.
infoRows :: Int -> [InfoLine] -> [(T.Text, Maybe T.Text)]
infoRows width ls = concatMap row ls
  where
    row :: InfoLine -> [(T.Text, Maybe T.Text)]
    row = \case
      InfoBlank -> [("", Just "")]
      InfoField label Nothing -> [(cell label, Nothing)]
      InfoField label (Just value) ->
        zip (cell label : repeat (cell "")) (map Just (wrapText (width - labelWidth) value))

    -- A label with its colon, padded to the column of the values.
    cell :: T.Text -> T.Text
    cell label =
      let text = if T.null label then "" else label <> ":"
      in text <> T.replicate (labelWidth - textWidth text) " "

    labelWidth :: Int
    labelWidth = maximum (0 : [textWidth label + textWidth ": " | InfoField label _ <- ls])
