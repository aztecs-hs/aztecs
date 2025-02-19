module Aztecs.Camera (Camera (..), CameraTarget (..)) where

import Aztecs.ECS
import Linear (V2 (..))

-- | Camera component.
data Camera = Camera
  { -- | Camera viewport size.
    cameraViewport :: !(V2 Int),
    -- | Camera scale factor.
    cameraScale :: !(V2 Float)
  }
  deriving (Show)

instance Component Camera

-- | Camera target component.
newtype CameraTarget = CameraTarget
  { -- | This camera's target window.
    cameraTargetWindow :: EntityID
  }
  deriving (Eq, Show)

instance Component CameraTarget
