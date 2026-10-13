-- | Helpers of the tests that press keys in the queue.
module Reprise.Test.Handler.Helpers
  ( -- * Songs
    songs
  , titled

    -- * Keys
  , key
  , key'
  , keys
  , typed

    -- * Results
  , editRequests
  , cursor
  , position
  , message
  , isError
  ) where

import Data.Text qualified as T

import Reprise.Event
import Reprise.Keys
import Reprise.Mpd.Protocol.Request
import Reprise.Mpd.Protocol.Types
import Reprise.State
import Reprise.Test.Helpers

songs :: Int -> [Song]
songs n = [song i [(Artist, ["A"]), (Title, [T.pack (show i)])] 60 | i <- [0 .. n - 1]]

key :: T.Text -> KeySpec
key = either (error . T.unpack) id . parseKeySpec

key' :: T.Text -> AppEvent
key' = KeyPressed . key

-- | The requests of an edit of the queue, which end in the status.
editRequests :: [Request] -> [[Request]]
editRequests rs = [rs <> [Request "status" []]]

keys :: [T.Text] -> AppState -> IO Result
keys ks = runEvents 0 (map key' ks)

cursor :: Result -> Int
cursor r = (focusedView r.state).cursor

-- | The cursor and the offset of the focused view.
position :: Result -> (Int, Int)
position r = ((focusedView r.state).cursor, (focusedView r.state).offset)

message :: Result -> Maybe T.Text
message r = (.text) <$> r.state.message

isError :: Result -> Maybe Bool
isError r = (.isError) <$> r.state.message

-- | Songs with titles to find.
titled :: [Song]
titled =
  [ song i [(Artist, ["A"]), (Title, [t])] 60
  | (i, t) <- zip [0 ..] ["alpha", "beta", "Gamma", "alpha two", "Pokój"]
  ]

-- | The keys that type the text.
typed :: T.Text -> [T.Text]
typed = map (\c -> if c == ' ' then "space" else T.singleton c) . T.unpack
