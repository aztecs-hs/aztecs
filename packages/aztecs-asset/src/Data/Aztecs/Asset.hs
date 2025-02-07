{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Asset where

import Control.Arrow ((>>>))
import Control.Concurrent (forkIO)
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.System as S
import Data.Data (Typeable)
import Data.Foldable (foldrM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map

newtype AssetId = AssetId {unAssetId :: Int}
  deriving (Eq, Ord, Show)

data AssetServer a = AssetServer
  { assetServerAssets :: Map AssetId a,
    loadingAssets :: Map AssetId (IORef (Maybe a)),
    nextAssetId :: AssetId
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
  loadAsset :: FilePath -> IO a

newtype Handle a = Handle {handleId :: AssetId}
  deriving (Eq, Ord, Show)

load :: (Asset a) => FilePath -> AssetServer a -> IO (Handle a, AssetServer a)
load path server = do
  let assetId = nextAssetId server
  v <- newIORef Nothing
  _ <- forkIO $ do
    a <- loadAsset path
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

loadAssets :: forall a. (Typeable a) => System () ()
loadAssets =
  S.map_
    ( Q.fetch @_ @(AssetServer a)
        >>> Q.run
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
        >>> Q.set
    )

setup :: forall a. (Typeable a) => System () ()
setup = S.queue . const . A.spawn_ @IO . bundle $ empty @a
