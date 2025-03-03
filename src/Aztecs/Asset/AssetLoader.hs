{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Arrow
import Control.Concurrent
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.IORef
import qualified Data.Map.Strict as Map

type AssetLoader a o = AssetLoaderT a Identity o

-- | Asset loader monad.
newtype AssetLoaderT a m o = AssetLoaderT {unAssetLoader :: StateT (AssetServer a) m o}
  deriving newtype (Functor, Applicative, Monad)

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
loadQuery :: (Asset a, ArrowQuery m arr) => AssetLoader a o -> arr () o
loadQuery a = proc () -> do
  server <- Q.fetch -< ()
  let (o, server') = runState (unAssetLoader a) server
  Q.set -< server'
  returnA -< o

-- | System to load assets.
load :: forall m q s a o. (ArrowQuery m q, MonadSystem q s, Asset a) => AssetLoader a o -> s o
load a = S.mapSingle @q () $ loadQuery a
