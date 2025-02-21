{-# LANGUAGE TypeFamilies #-}

module Aztecs.Asset.Class (Asset (..)) where

import Data.Data (Typeable)

class (Typeable a) => Asset a where
  type AssetConfig a

  loadAsset :: FilePath -> AssetConfig a -> IO a
