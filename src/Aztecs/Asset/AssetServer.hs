{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Arrow
import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Data
import Data.Foldable
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import GHC.Generics

-- | Unique identifier for an asset.
newtype AssetId = AssetId {unAssetId :: Int}
  deriving (Eq, Ord, Show)

-- | Asset server.
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

-- | Setup the asset server.
setup :: forall arr m b a. (Typeable a, ArrowQueueSystem m b arr) => arr () ()
setup = S.queue . const . A.spawn_ . bundle $ assetServer @a

-- | Load any pending assets.
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
  server' <- access loadAssetServer -< server
  system $ S.mapSingle Q.set -< server'
  returnA -< ()

-- | Load any pending assets in an `AssetServer`.
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
