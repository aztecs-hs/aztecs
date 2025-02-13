{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow (returnA, (>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (load)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Camera (..), Image (..), Window (..))
import qualified Data.Aztecs.SDL as SDL
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (V2 (..))

setup :: System () ()
setup =
  S.mapSingle
    ( proc () -> do
        assetServer <- Q.fetch -< ()
        (texture, assetServer') <- Q.run (load "example.png") -< assetServer
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
          A.spawn_ $
            bundle Image {imageTexture = texture, imageSize = V2 200 200}
              <> bundle transform {transformPosition = V2 500 100}
      )

update :: System () ()
update = S.all (Q.fetch @_ @SDL.Keyboard) >>> S.run (\keyboard -> print keyboard)

main :: IO ()
main = runSystem_ $ SDL.setup >>> setup >>> S.forever (SDL.update >>> update >>> SDL.draw)
