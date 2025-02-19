{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Aztecs
import Aztecs.Asset (load)
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.SDL (Camera (..), Window (..))
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Image (Image (..))
import qualified Aztecs.SDL.Image as IMG
import Aztecs.Transform (Transform (..), transform)
import Control.Arrow (returnA, (>>>))
import SDL (V2 (..))

setup :: Schedule IO () ()
setup = proc () -> do
  texture <-
    system $
      S.mapSingle
        ( proc () -> do
            assetServer <- Q.fetch -< ()
            let (texture, assetServer') = load "assets/example.png" () assetServer
            Q.set -< assetServer'
            returnA -< texture
        )
      -<
        ()
  access
    ( \texture -> do
        A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
        A.spawn_ $
          bundle Camera {cameraViewport = V2 1000 500, cameraScale = 5}
            <> bundle transform
        A.spawn_ $
          bundle Image {imageTexture = texture, imageSize = V2 100 100}
            <> bundle transform {transformPosition = V2 10 10}
    )
    -<
      texture

app :: Schedule IO () ()
app =
  SDL.setup
    >>> system IMG.setup
    >>> setup
    >>> forever_
      ( IMG.load
          >>> SDL.update
          >>> system IMG.draw
          >>> SDL.draw
      )

main :: IO ()
main = runSchedule_ app
