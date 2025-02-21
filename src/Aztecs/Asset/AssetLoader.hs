{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aztecs.Asset.AssetLoader
  ( MonadAssetLoader (..),
    AssetLoader,
    AssetLoaderT (..),
    load,
    loadQuery,
  )
where

import Aztecs.Asset.AssetLoader.Class
import Aztecs.Asset.AssetServer (AssetId (..), AssetServer (..), Handle (..))
import Aztecs.Asset.Class
import Aztecs.ECS
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Arrow (returnA)
import Control.Concurrent (forkIO)
import Control.Monad.Identity (Identity)
import Control.Monad.State.Strict (MonadState (..), StateT, runState)
import Data.IORef (newIORef, writeIORef)
import qualified Data.Map.Strict as Map

type AssetLoader a o = AssetLoaderT a Identity o

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

loadQuery :: (Asset a, ArrowQuery arr) => AssetLoader a o -> arr () o
loadQuery a = proc () -> do
  assetServer <- Q.fetch -< ()
  let (o, assetServer') = runState (unAssetLoader a) assetServer
  Q.set -< assetServer'
  returnA -< o

load :: (ArrowQuery q, ArrowSystem q arr, Asset a) => AssetLoader a o -> arr () o
load a = S.mapSingle $ loadQuery a
