{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.Asset
  ( AssetId (..),
    AssetServer (..),
    MonadAssetLoader (..),
    Asset (..),
    Handle (..),
    lookupAsset,
    setup,
    loadAssets,
    load,
  )
where

import Aztecs.Asset.AssetLoader
import Aztecs.Asset.AssetServer
import Aztecs.Asset.Class
