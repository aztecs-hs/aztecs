{-# LANGUAGE TypeFamilies #-}

module Aztecs.Asset.Class (Asset (..)) where

import Data.Data

-- | Loadable asset.
class (Typeable a) => Asset a where
  -- | Configuration for loading an asset.
  type AssetConfig a

  -- | Load an asset from a file path with a configuration.
  loadAsset :: FilePath -> AssetConfig a -> IO a
