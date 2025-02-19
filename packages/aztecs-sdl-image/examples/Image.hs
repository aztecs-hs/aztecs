{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (load)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Camera (..), Window (..))
import qualified Data.Aztecs.SDL as SDL
import Data.Aztecs.SDL.Image (Image (..))
import qualified Data.Aztecs.SDL.Image as IMG
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (V2 (..))

setup :: Schedule IO () ()
setup = proc () -> do
  assetServer <- reader $ S.single Q.fetch -< ()
  (texture, assetServer') <- task $ load "assets/example.png" () -< assetServer
  system $ S.mapSingle Q.set -< assetServer'
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
