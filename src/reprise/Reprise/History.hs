-- | The history of the line prompts, which all of them share.
module Reprise.History
  ( Recall (..)
  , historySize
  , remember
  , recallOlder
  , recallOldest
  , recallNewer

    -- * File
  , historyOfLines
  , readHistoryFile
  , saveToHistoryFile
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Directory
import System.FilePath

import Reprise.File
import Reprise.LineEdit

-- | A line of the history that a prompt shows in place of what the user
-- typed.
data Recall = Recall
  { typed :: LineEdit
  -- ^ What the user typed, which going down past the newest line brings
  -- back.
  , index :: Int
  -- ^ The line in the history, from the newest.
  }
  deriving stock (Eq, Show)

-- | How many lines the history keeps. Every distinct line would make it
-- and its file grow without end. The author chose the number, as enough
-- for the lines a user types again.
historySize :: Int
historySize = 1000

-- | Add a line to a history, newest first. A line that is in it already
-- moves to the front, so each is in it once, and the oldest line goes when
-- the history is full. A blank line isn't added.
remember :: T.Text -> [T.Text] -> [T.Text]
remember line history
  | T.null (T.strip line) = history
  | otherwise = take historySize $ line : filter (/= line) history

-- | The next older line.
recallOlder :: [T.Text] -> LineEdit -> Maybe Recall -> Maybe (LineEdit, Recall)
recallOlder history edit recall =
  recallAt history (typedLine edit recall) (maybe 0 ((+ 1) . (.index)) recall)

-- | The oldest line, as bash's @beginning-of-history@.
recallOldest :: [T.Text] -> LineEdit -> Maybe Recall -> Maybe (LineEdit, Recall)
recallOldest history edit recall =
  recallAt history (typedLine edit recall) (length history - 1)

-- | The next newer line, or what the user typed after the newest one.
recallNewer :: [T.Text] -> Recall -> (LineEdit, Maybe Recall)
recallNewer history recall =
  maybe (recall.typed, Nothing) (fmap Just) $
    recallAt history recall.typed (recall.index - 1)

-- | The line at an index from the newest, if there is one, with the cursor
-- at its end.
recallAt :: [T.Text] -> LineEdit -> Int -> Maybe (LineEdit, Recall)
recallAt history typed i = do
  guard $ i >= 0
  line <- listToMaybe (drop i history)
  pure (LineEdit line T.empty, Recall typed i)

-- | What the user typed, before the line that a prompt recalled, if any.
typedLine :: LineEdit -> Maybe Recall -> LineEdit
typedLine edit = maybe edit (.typed)

----------------------------------------
-- File

-- | Read a history from a file of a line each, the oldest first, as shells
-- keep theirs. A missing file is an empty history.
readHistoryFile :: FilePath -> IO [T.Text]
readHistoryFile path = do
  exists <- doesFileExist path
  if exists
    then historyOfLines . T.lines . T.decodeUtf8Lenient <$> BS.readFile path
    else pure []

-- | The history that 'remember' makes of lines, the oldest first, in one
-- pass from the newest. Each 'remember' scans the history, so the lines of
-- a full history took 3.7 ms that way, and take 115 µs in one pass, by the
-- benchmarks.
historyOfLines :: [T.Text] -> [T.Text]
historyOfLines = take historySize . newest S.empty . reverse
  where
    -- The lines that aren't blank, without those that a newer one repeats.
    newest :: S.Set T.Text -> [T.Text] -> [T.Text]
    newest seen = \case
      [] -> []
      line : rest
        | T.null (T.strip line) || line `S.member` seen -> newest seen rest
        | otherwise -> line : newest (S.insert line seen) rest

-- | Add a line to a history file. Another reprise may have added its own
-- lines since this one read the file, so the line goes into the file as it
-- is now, and both keep theirs.
saveToHistoryFile :: FilePath -> T.Text -> IO ()
saveToHistoryFile path line = do
  history <- readHistoryFile path
  let history' = remember line history
  when (history' /= history) $ do
    createDirectoryIfMissing True (takeDirectory path)
    writeFileAtomically path . T.encodeUtf8 . T.unlines $ reverse history'
