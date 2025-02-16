{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow (returnA, (>>>))
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

setup :: System () ()
setup =
  S.mapSingle
    ( proc () -> do
        assetServer <- Q.fetch -< ()
        (texture, assetServer') <- Q.run $ load "assets/example.png" () -< assetServer
        Q.set -< assetServer'
        returnA -< texture
    )
    >>> S.queue
      ( \texture -> do
          A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
          A.spawn_ $ bundle Camera {cameraViewport = V2 1000 500} <> bundle transform
          A.spawn_ $
            bundle Image {imageTexture = texture, imageSize = V2 100 100}
              <> bundle transform {transformPosition = V2 100 100}
      )

app :: Schedule IO () ()
app =
  schedule SDL.setup
    >>> schedule IMG.setup
    >>> schedule setup
    >>> forever
      ( schedule IMG.load
          >>> schedule SDL.update
          >>> schedule IMG.draw
          >>> schedule SDL.draw
      )

main :: IO ()
main = runSchedule_ app
