{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Aztecs.Asset.AssetLoader
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Asset.AssetLoader
  ( AssetLoader,
    AssetLoaderT (..),
    MonadAssetLoader (..),
    load,
    loadQuery,
  )
where

import Aztecs.Asset.AssetLoader.Class
import Aztecs.Asset.AssetServer
import Aztecs.Asset.Class
import Aztecs.ECS
import Aztecs.ECS.Query (QueryT (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.Query.Dynamic (DynamicQueryT (DynamicQuery, runDynQuery))
import qualified Aztecs.ECS.System as S
import Control.Concurrent
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Writer
import Data.IORef
import qualified Data.Map.Strict as Map

-- | @since 0.9
type AssetLoader a o = AssetLoaderT a Identity o

-- | Asset loader monad.
--
-- @since 0.9
newtype AssetLoaderT a m o = AssetLoaderT
  { -- | State of the asset loader.
    --
    -- @since 0.9
    unAssetLoader :: StateT (AssetServer a) m o
  }
  deriving newtype (Functor, Applicative, Monad)

-- | @since 0.9
instance (Monad m, Asset a) => MonadAssetLoader a (AssetLoaderT a m) where
  asset path cfg = AssetLoaderT $ do
    server <- get
    let assetId = nextAssetId server
        go = do
          v <- newIORef Nothing
          _ <- forkIO $ do
            a <- loadAsset path cfg
            writeIORef v (Just a)
          return v
    put $
      server
        { loadingAssets = Map.insert assetId (Left go) (loadingAssets server),
          nextAssetId = AssetId (unAssetId assetId + 1)
        }
    return $ Handle assetId

-- | Query to load assets.
--
-- @since 0.9
loadQuery :: (Asset a) => AssetLoader a o -> Query o
loadQuery a =
  -- TODO
  Query $ \cs ->
    let q =
          Q.adjustM
            ( \_ server -> do
                let (o, server') = runState (unAssetLoader a) server
                tell [o]
                return server'
            )
            (pure ())
        (rws, cs', dynQ) = runQuery q cs
     in ( rws,
          cs',
          DynamicQuery $ \arch ->
            let ((_, arch'), os) = runWriter $ runDynQuery dynQ arch in return (os, arch')
        )

-- | System to load assets.
--
-- @since 0.9
load :: (MonadSystem Query s, Asset a) => AssetLoader a o -> s o
load a = S.mapSingle $ loadQuery a
