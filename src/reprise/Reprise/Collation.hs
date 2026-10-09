-- | The order of text for people, by the rules of a locale.
module Reprise.Collation
  ( Collator
  , userCollator
  , localeCollator
  , rootCollator
  , CollationKey
  , collationKey
  ) where

import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.ICU qualified as ICU
import Data.Text.ICU.Collate qualified as Collate
import System.Environment

-- | The rules of a locale, with the locale for 'Show'.
data Collator = Collator ICU.LocaleName ICU.Collator

instance Show Collator where
  showsPrec d (Collator locale _) =
    showParen (d > 10) $ showString "Collator " . showsPrec 11 locale

-- | The rules of the user's locale, as ncmpcpp sorts.
userCollator :: IO Collator
userCollator = localeCollator <$> traverse lookupEnv ["LC_ALL", "LC_COLLATE", "LANG"]

-- | The rules of the locale that orders text, from the values of @LC_ALL@,
-- @LC_COLLATE@ and @LANG@: the first one set and not empty, as the C
-- library reads them. ICU's default locale reads @LC_MESSAGES@ instead of
-- @LC_COLLATE@. ICU doesn't read a codeset or a modifier, e.g. of
-- @sv_SE.UTF-8@, and the C or POSIX locale, or none, has the root rules.
localeCollator :: [Maybe String] -> Collator
localeCollator values = case [v | Just v <- values, not (null v)] of
  value : _
    | name <- takeWhile (`notElem` ['.', '@']) value
    , name `notElem` ["C", "POSIX"] ->
        collator (ICU.Locale name)
  _ -> rootCollator

-- | Rules that don't depend on the environment, e.g. for tests. In the C
-- locale, ICU orders letters as their bytes, capitals first.
rootCollator :: Collator
rootCollator = collator ICU.Root

-- | The rules of a locale, with numbers by their values, e.g. a track 10
-- after a track 2, as file managers sort them.
collator :: ICU.LocaleName -> Collator
collator locale = Collator locale (ICU.collatorWith locale [Collate.Numeric True])

-- | A key that orders text as a collator does.
newtype CollationKey = CollationKey BS.ByteString
  deriving newtype (Eq, Ord, Show)

-- | The key of a text. A leading "the" can be ignored, so that "The Beatles"
-- sorts with the other names that start with B.
collationKey :: Collator -> Bool -> T.Text -> CollationKey
collationKey (Collator _ c) ignoreThe t =
  CollationKey . ICU.sortKey c $
    if ignoreThe && T.toLower (T.take 4 t) == "the " then T.drop 4 t else t
