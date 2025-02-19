{-# LANGUAGE Arrows #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.System as S
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Image (Image (..))
import qualified Aztecs.SDL.Image as IMG
import Control.Arrow ((>>>))
import SDL (V2 (..))

setup :: System () ()
setup = proc () -> do
  texture <- load $ asset "assets/example.png" () -< ()
  S.queue
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
    >>> system setup
    >>> forever_
      ( IMG.load
          >>> SDL.update
          >>> system IMG.draw
          >>> SDL.draw
      )

main :: IO ()
main = runSchedule_ app
