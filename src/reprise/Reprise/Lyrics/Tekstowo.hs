-- | Lyrics from tekstowo.pl, a site of lyrics with many that LRCLIB doesn't
-- have. It has no API, so the fetcher reads its pages, and it breaks when
-- the site changes them.
module Reprise.Lyrics.Tekstowo
  ( tekstowo
  , tekstowoLyrics
  , searchResults
  , songLyrics
  ) where

import Control.Monad
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Text.HTML.TagSoup

import Reprise.Lyrics
import Reprise.Lyrics.Http
import Reprise.Mpd.Protocol.Types hiding (Tag)

tekstowo :: Get -> Fetcher
tekstowo = Fetcher "tekstowo.pl" . tekstowoLyrics

-- | The lyrics of a song from tekstowo.pl: of the song that its search
-- finds by the same artist and title, with or without what follows the
-- title in brackets. Another song's lyrics would be stored as the song's,
-- so a result that is only close isn't taken.
tekstowoLyrics :: Get -> Song -> IO FetchResult
tekstowoLyrics get song = byArtistAndTitle song $ \artist title -> do
  let wanted = map (key . ((artist <> " - ") <>)) (L.nub [title, cleanTitle title])
  get "/szukaj" [("search-query", artist <> " " <> cleanTitle title)] >>= \case
    Right (200, body) ->
      case [p | (p, t) <- searchResults (T.decodeUtf8Lenient body), key t `elem` wanted] of
        songPath : _ ->
          get (T.encodeUtf8 songPath) [] >>= \case
            Right (200, page) ->
              pure . maybe FetchedNothing (FetchedLyrics . plainLyrics) $
                songLyrics (T.decodeUtf8Lenient page)
            other -> pure $ failure other
        [] -> pure FetchedNothing
    -- A search that finds nothing redirects to the advanced search.
    Right (status, _) | status `div` 100 == 3 -> pure FetchedNothing
    other -> pure $ failure other
  where
    -- What a song is matched by, without case and extra spaces.
    key :: T.Text -> T.Text
    key = T.unwords . T.words . T.toCaseFold

    failure :: Either T.Text (Int, a) -> FetchResult
    failure = \case
      Left reason -> FetchFailed reason
      Right (status, _) -> FetchFailed $ answeredWith "tekstowo.pl" status

-- | The songs that a page of the search found: the path of each, and its
-- artist and title, e.g. @/kult/arahja@ and @Kult - Arahja@. They are the
-- links of the section of songs, up to the next heading; the page has other
-- songs elsewhere, e.g. the popular ones.
searchResults :: T.Text -> [(T.Text, T.Text)]
searchResults page =
  [ (href, title)
  | TagOpen "a" attributes <- section
  , lookup "class" attributes == Just "title"
  , Just href <- [lookup "href" attributes]
  , Just title <- [lookup "title" attributes]
  ]
  where
    section :: [Tag T.Text]
    section =
      takeWhile (not . isTagOpenName "h2")
        . drop 1
        . dropWhile (\t -> not (isTagText t && T.strip (fromTagText t) == "Znalezione utwory:"))
        $ parseTags page

-- | The lyrics on the page of a song: its first text, as the second is the
-- translation. A @<br />@ ends a line, and other tags aren't text.
songLyrics :: T.Text -> Maybe T.Text
songLyrics page = do
  let text =
        T.strip . T.unlines . map T.stripEnd . T.lines . T.concat . map lyric $
          inside (1 :: Int) (drop 1 (dropWhile (not . lyricsStart) (parseTags page)))
  guard . not $ T.null text
  pure text
  where
    lyricsStart :: Tag T.Text -> Bool
    lyricsStart = \case
      TagOpen "div" attributes -> lookup "class" attributes == Just "inner-text"
      _ -> False

    -- The tags up to the end of the div at a depth of nested divs.
    inside :: Int -> [Tag T.Text] -> [Tag T.Text]
    inside depth = \case
      [] -> []
      t : ts
        | isTagCloseName "div" t -> if depth == 1 then [] else t : inside (depth - 1) ts
        | isTagOpenName "div" t -> t : inside (depth + 1) ts
        | otherwise -> t : inside depth ts

    -- The page's line ends are in its markup, and @<br />@ is the lyrics'.
    lyric :: Tag T.Text -> T.Text
    lyric = \case
      TagText t -> T.filter (`notElem` ['\r', '\n']) t
      TagOpen "br" _ -> "\n"
      _ -> ""
