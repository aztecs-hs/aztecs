{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.Asset
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
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
