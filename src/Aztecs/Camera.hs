{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.Camera
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.Camera
  ( Camera (..),
    CameraTarget (..),
    addCameraTargets,
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query.Reader as Q
import qualified Aztecs.ECS.System as S
import Aztecs.Window
import Control.Arrow
import Control.DeepSeq
import GHC.Generics
import Linear

-- | Camera component.
--
-- @since 0.9
data Camera = Camera
  { -- | Camera viewport size.
    --
    -- @since 0.9
    cameraViewport :: !(V2 Int),
    -- | Camera scale factor.
    --
    -- @since 0.9
    cameraScale :: !(V2 Float)
  }
  deriving (Show, Generic, NFData)

-- | @since 0.9
instance Component Camera

-- | Camera target component.
--
-- @since 0.9
newtype CameraTarget = CameraTarget
  { -- | This camera's target window.
    --
    -- @since 0.9
    cameraTargetWindow :: EntityID
  }
  deriving (Eq, Show, Generic, NFData)

-- | @since 0.9
instance Component CameraTarget

-- | Add `CameraTarget` components to entities with a new `Draw` component.
--
-- @since 0.9
addCameraTargets ::
  ( Applicative qr,
    QueryReaderF qr,
    DynamicQueryReaderF qr,
    MonadReaderSystem qr s,
    MonadAccess b m
  ) =>
  s (m ())
addCameraTargets = do
  windows <- S.all ((,) <$> Q.entity <*> Q.fetch @_ @Window)
  newCameras <- S.filter ((,) <$> Q.entity <*> Q.fetch @_ @Camera) (without @CameraTarget)
  let go = case windows of
        (windowEId, _) : _ -> mapM_ (\(eId, _) -> A.insert eId . bundle $ CameraTarget windowEId) newCameras
        _ -> return ()
  return go
