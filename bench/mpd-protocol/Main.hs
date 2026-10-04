-- | Benchmarks of reading MPD's replies.
module Main (main) where

import Data.ByteString.Char8 qualified as BS8
import Test.Tasty.Bench

import Reprise.Mpd.Protocol.Response

main :: IO ()
main =
  defaultMain
    [ bgroup
        "parse"
        [ env (pure (reply [0 .. queueLength - 1])) $ \ls ->
            bench "playlistinfo of a long queue" $ whnf parseSongCount ls
        , -- Deleting a song shifts every song after it, and MPD sends them all.
          env (pure (reply [queueLength `div` 2 .. queueLength - 1])) $ \ls ->
            bench "plchanges after a delete in the middle" $ whnf parseSongCount ls
        ]
    ]

-- | The number of songs in a reply. Parsing evaluates them fully.
parseSongCount :: [BS8.ByteString] -> Int
parseSongCount ls = case parseReply ls of
  Right [fields] -> either (error . show) length (parseSongs fields)
  other -> error $ "unexpected reply: " <> show (length <$> other)

-- | The lines of a reply with the songs at the positions, as MPD sends
-- them, with their own buffers as lines read from a socket have.
reply :: [Int] -> [BS8.ByteString]
reply positions = map BS8.copy (concatMap songLines positions <> ["OK"])
  where
    songLines :: Int -> [BS8.ByteString]
    songLines p =
      map
        BS8.pack
        [ "file: Artist "
            <> show (p `div` 20)
            <> "/Album "
            <> show (p `div` 10)
            <> "/"
            <> show p
            <> ".flac"
        , "Last-Modified: 2024-01-02T03:04:05Z"
        , "Added: 2024-01-02T03:04:05Z"
        , "Format: 44100:16:2"
        , "Artist: Artist number " <> show (p `div` 20)
        , "Album: An album called " <> show (p `div` 10)
        , "Title: The title of song " <> show p <> " on the album"
        , "Track: " <> show (p `mod` 10 + 1)
        , "Date: 2001"
        , "Time: 200"
        , "duration: 200.000"
        , "Pos: " <> show p
        , "Id: " <> show (p + 1)
        ]

-- | The length of the queue that the work on performance measured.
queueLength :: Int
queueLength = 4254
