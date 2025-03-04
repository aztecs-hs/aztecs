{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.Asset.AssetServer
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Asset.AssetServer
  ( AssetId (..),
    Handle (..),
    AssetServer (..),
    assetServer,
    lookupAsset,
    setup,
    loadAssets,
    loadAssetServer,
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics

-- | Unique identifier for an asset.
--
-- @since 9.0
newtype AssetId = AssetId
  { -- | Unique integer identifier.
    --
    -- @since 9.0
    unAssetId :: Int
  }
  deriving (Eq, Ord, Show)

-- | Asset server.
--
-- @since 9.0
data AssetServer a = AssetServer
  { -- | Loaded assets.
    --
    -- @since 9.0
    assetServerAssets :: !(Map AssetId a),
    -- | Assets currently being loaded.
    --
    -- @since 9.0
    loadingAssets :: !(Map AssetId (Either (IO (IORef (Maybe a))) (IORef (Maybe a)))),
    -- | Next unique asset identifier.
    --
    -- @since 9.0
    nextAssetId :: !AssetId
  }
  deriving (Generic)

-- | @since 9.0
instance (Typeable a) => Component (AssetServer a)

-- | @since 9.0
instance NFData (AssetServer a) where
  rnf = rwhnf

-- | Empty asset server.
--
-- @since 9.0
assetServer :: AssetServer a
assetServer =
  AssetServer
    { assetServerAssets = Map.empty,
      loadingAssets = Map.empty,
      nextAssetId = AssetId 0
    }

-- | Handle to an asset.
--
-- @since 9.0
newtype Handle a = Handle
  { -- | Asset ID.
    --
    -- @since 9.0
    handleId :: AssetId
  }
  deriving (Eq, Ord, Show)

-- | @since 9.0
instance NFData (Handle a) where
  rnf = rwhnf

-- | Lookup an asset by its handle.
--
-- @since 9.0
lookupAsset :: Handle a -> AssetServer a -> Maybe a
lookupAsset h server = Map.lookup (handleId h) (assetServerAssets server)

-- | Setup the asset server.
--
-- @since 9.0
setup :: forall m b a. (Typeable a, MonadAccess b m) => m ()
setup = A.spawn_ . bundle $ assetServer @a

-- | Load any pending assets.
--
-- @since 9.0
loadAssets :: forall a q s m. (Typeable a, ArrowQuery m q, MonadSystem q s, MonadIO m) => s ()
loadAssets = void . S.map @q () $ Q.adjustM (\_ s -> loadAssetServer @m @a s)

-- | Load any pending assets in an `AssetServer`.
--
-- @since 9.0
loadAssetServer :: (MonadIO m) => AssetServer a -> m (AssetServer a)
loadAssetServer server =
  let go (aId, v) acc = do
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
   in liftIO . foldrM go server . Map.toList $ loadingAssets server
