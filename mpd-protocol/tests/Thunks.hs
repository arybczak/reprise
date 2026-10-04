-- | A check that a value is fully evaluated.
module Thunks
  ( thunks
  ) where

import GHC.Exts.Heap

-- | The thunks that a value refers to, each with the constructors on the way
-- to it.
thunks :: a -> IO [String]
thunks = go [] . asBox
  where
    go :: [String] -> Box -> IO [String]
    go path b =
      getBoxedClosureData b >>= \case
        ConstrClosure {name, ptrArgs} -> concat <$> mapM (go (name : path)) ptrArgs
        -- An evaluated thunk refers to its value until the next garbage
        -- collection.
        IndClosure {indirectee} -> go path indirectee
        BlackholeClosure {indirectee} -> go path indirectee
        ThunkClosure {} -> found "thunk"
        SelectorClosure {} -> found "selector thunk"
        APClosure {} -> found "application thunk"
        APStackClosure {} -> found "stack thunk"
        _ -> pure []
      where
        found :: String -> IO [String]
        found kind = pure [unwords (reverse (kind : path))]
