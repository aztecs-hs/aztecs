{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Asset where

import Control.Arrow (returnA)
import Control.Concurrent (forkIO)
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S
import Data.Data (Typeable)
import Data.Foldable (foldrM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype AssetId = AssetId {unAssetId :: Int}
  deriving (Eq, Ord, Show)

data AssetServer a = AssetServer
  { assetServerAssets :: !(Map AssetId a),
    loadingAssets :: !(Map AssetId (IORef (Maybe a))),
    nextAssetId :: !AssetId
  }

instance (Typeable a) => Component (AssetServer a)

empty :: AssetServer a
empty =
  AssetServer
    { assetServerAssets = Map.empty,
      loadingAssets = Map.empty,
      nextAssetId = AssetId 0
    }

class Asset a where
  type AssetConfig a

  loadAsset :: FilePath -> AssetConfig a -> IO a

newtype Handle a = Handle {handleId :: AssetId}
  deriving (Eq, Ord, Show)

load :: (Asset a) => FilePath -> AssetConfig a -> AssetServer a -> IO (Handle a, AssetServer a)
load path cfg server = do
  let assetId = nextAssetId server
  v <- newIORef Nothing
  _ <- forkIO $ do
    a <- loadAsset path cfg
    writeIORef v (Just a)
  return
    ( Handle assetId,
      server
        { loadingAssets = Map.insert assetId v (loadingAssets server),
          nextAssetId = AssetId (unAssetId assetId + 1)
        }
    )

lookupAsset :: Handle a -> AssetServer a -> Maybe a
lookupAsset h server = Map.lookup (handleId h) (assetServerAssets server)

loadAssets :: forall a. (Typeable a) => Schedule IO () ()
loadAssets = proc () -> do
  server <- reader $ S.single (Q.fetch @_ @(AssetServer a)) -< ()
  server' <-
    task
      ( \server ->
          foldrM
            ( \(aId, v) acc -> do
                maybeSurface <- readIORef v
                case maybeSurface of
                  Just surface ->
                    return
                      acc
                        { assetServerAssets = Map.insert aId surface (assetServerAssets acc),
                          loadingAssets = Map.delete aId (loadingAssets acc)
                        }
                  Nothing -> return acc
            )
            server
            (Map.toList $ loadingAssets server)
      )
      -<
        server
  system $ S.mapSingle Q.set -< server'
  returnA -< ()

setup :: forall a. (Typeable a) => System () ()
setup = S.queue . const . A.spawn_ . bundle $ empty @a
