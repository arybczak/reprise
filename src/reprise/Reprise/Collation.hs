-- | The order of text for people, by the rules of a locale.
module Reprise.Collation
  ( Collator
  , userCollator
  , rootCollator
  , CollationKey
  , collationKey
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.ICU qualified as ICU

-- | The rules of a locale, with the locale for 'Show'.
data Collator = Collator ICU.LocaleName ICU.Collator

instance Show Collator where
  showsPrec d (Collator locale _) =
    showParen (d > 10) $ showString "Collator " . showsPrec 11 locale

-- | The rules of the user's locale, as ncmpcpp sorts.
userCollator :: Collator
userCollator = collator ICU.Current

-- | Rules that don't depend on the environment, e.g. for tests. In the C
-- locale, ICU orders letters as their bytes, capitals first.
rootCollator :: Collator
rootCollator = collator ICU.Root

collator :: ICU.LocaleName -> Collator
collator locale = Collator locale (ICU.collator locale)

-- | A key that orders text as a collator does.
newtype CollationKey = CollationKey BS.ByteString
  deriving newtype (Eq, Ord, Show)

-- | The key of a text. A leading "the" can be ignored, so that "The Beatles"
-- sorts with the other names that start with B.
collationKey :: Collator -> Bool -> T.Text -> CollationKey
collationKey (Collator _ c) ignoreThe t =
  CollationKey . ICU.sortKey c $
    if ignoreThe && T.toLower (T.take 4 t) == "the " then T.drop 4 t else t
