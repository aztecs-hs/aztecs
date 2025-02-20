{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.Camera
  ( Camera (..),
    CameraTarget (..),
    addCameraTargets,
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import Aztecs.ECS.Query.Reader (ArrowQueryReader)
import qualified Aztecs.ECS.Query.Reader as Q
import Aztecs.ECS.System (ArrowReaderSystem, ArrowSystem)
import qualified Aztecs.ECS.System as S
import Aztecs.Window (Window)
import Control.Arrow (Arrow (..))
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

-- | Add `CameraTarget` components to entities with a new `Draw` component.
addCameraTargets :: (ArrowQueryReader qr, ArrowReaderSystem qr arr, ArrowSystem q arr) => arr () ()
addCameraTargets = proc () -> do
  windows <- S.all (Q.entity &&& Q.fetch @_ @Window) -< ()
  newCameras <- S.filter (Q.entity &&& Q.fetch @_ @Camera) (without @CameraTarget) -< ()
  S.queue
    ( \(newCameras, windows) -> case windows of
        (windowEId, _) : _ -> mapM_ (\(eId, _) -> A.insert eId $ CameraTarget windowEId) newCameras
        _ -> return ()
    )
    -<
      (newCameras, windows)
