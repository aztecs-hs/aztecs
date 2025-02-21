{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.Asset.AssetLoader.Class
  ( MonadAssetLoader (..),
  )
where

import Aztecs.Asset.AssetServer (Handle)
import Aztecs.Asset.Class

class MonadAssetLoader a m | m -> a where
  asset :: FilePath -> AssetConfig a -> m (Handle a)
