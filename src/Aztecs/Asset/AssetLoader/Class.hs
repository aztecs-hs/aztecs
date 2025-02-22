{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.Asset.AssetLoader.Class
  ( MonadAssetLoader (..),
  )
where

import Aztecs.Asset.AssetServer (Handle)
import Aztecs.Asset.Class

class MonadAssetLoader a m | m -> a where
  asset :: FilePath -> AssetConfig a -> m (Handle a)
