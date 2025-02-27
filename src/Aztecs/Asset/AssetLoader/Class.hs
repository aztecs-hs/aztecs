{-# LANGUAGE FunctionalDependencies #-}

module Aztecs.Asset.AssetLoader.Class (MonadAssetLoader (..)) where

import Aztecs.Asset.AssetServer (Handle)
import Aztecs.Asset.Class

-- | Monadic interface for loading assets.
class MonadAssetLoader a m | m -> a where
  -- | Load an asset from a file path with a configuration.
  asset :: FilePath -> AssetConfig a -> m (Handle a)
