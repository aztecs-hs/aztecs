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

module Aztecs.Asset.AssetServer
  ( AssetId (..),
    AssetServer (..),
    assetServer,
    Handle (..),
    setup,
    loadAssets,
    lookupAsset,
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.Arrow (returnA)
import Control.DeepSeq
import Control.Monad.IO.Class (MonadIO (..))
import Data.Data (Typeable)
import Data.Foldable (foldrM)
import Data.IORef (IORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)

newtype AssetId = AssetId {unAssetId :: Int}
  deriving (Eq, Ord, Show)

data AssetServer a = AssetServer
  { assetServerAssets :: !(Map AssetId a),
    loadingAssets :: !(Map AssetId (Either (IO (IORef (Maybe a))) (IORef (Maybe a)))),
    nextAssetId :: !AssetId
  }
  deriving (Generic)

instance (Typeable a) => Component (AssetServer a)

instance NFData (AssetServer a) where
  rnf = rwhnf

assetServer :: AssetServer a
assetServer =
  AssetServer
    { assetServerAssets = Map.empty,
      loadingAssets = Map.empty,
      nextAssetId = AssetId 0
    }

newtype Handle a = Handle {handleId :: AssetId}
  deriving (Eq, Ord, Show)

instance NFData (Handle a) where
  rnf = rwhnf

lookupAsset :: Handle a -> AssetServer a -> Maybe a
lookupAsset h server = Map.lookup (handleId h) (assetServerAssets server)

loadAssets ::
  forall a qr rs q s b m arr.
  ( Typeable a,
    ArrowQueryReader qr,
    ArrowReaderSystem qr rs,
    ArrowReaderSchedule rs arr,
    ArrowQuery q,
    ArrowSystem q s,
    ArrowSchedule s arr,
    MonadIO m,
    ArrowAccessSchedule b m arr
  ) =>
  arr () ()
loadAssets = proc () -> do
  server <- reader $ S.single (Q.fetch @_ @(AssetServer a)) -< ()
  server' <-
    access
      ( \server ->
          liftIO $
            foldrM
              ( \(aId, v) acc -> do
                  case v of
                    Right r -> do
                      maybeSurface <- readIORef r
                      case maybeSurface of
                        Just surface ->
                          return
                            acc
                              { assetServerAssets = Map.insert aId surface (assetServerAssets acc),
                                loadingAssets = Map.delete aId (loadingAssets acc)
                              }
                        Nothing -> return acc
                    Left f -> do
                      v' <- f
                      return $ acc {loadingAssets = Map.insert aId (Right v') (loadingAssets server)}
              )
              server
              (Map.toList $ loadingAssets server)
      )
      -<
        server
  system $ S.mapSingle Q.set -< server'
  returnA -< ()

setup :: forall a. (Typeable a) => System () ()
setup = S.queue . const . A.spawn_ . bundle $ assetServer @a
