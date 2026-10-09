-- | Whole numbers as people write them in the config, an action or a file:
-- decimal digits only, without a sign or spaces.
module Reprise.Number
  ( decimal
  , decimalIn
  ) where

import Data.Text qualified as T
import Data.Text.Read qualified as T

-- | The number that the digits write, if the text is only digits.
decimal :: T.Text -> Maybe Integer
decimal t = case T.decimal t of
  Right (n, rest) | T.null rest -> Just n
  _ -> Nothing

-- | 'decimal' from a lower bound to an upper one.
decimalIn :: Int -> Int -> T.Text -> Maybe Int
decimalIn low high t = do
  n <- decimal t
  if n >= toInteger low && n <= toInteger high then Just (fromInteger n) else Nothing
