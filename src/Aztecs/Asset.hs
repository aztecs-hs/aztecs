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
