{-# LANGUAGE Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow (returnA, (>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (load)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Image (..), Window (..))
import qualified Data.Aztecs.SDL as SDL
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (V2 (..))

setup :: System IO () ()
setup =
  S.single
    ( proc () -> do
        assetServer <- Q.fetch -< ()
        (texture, assetServer') <- Q.run (load "example.png") -< assetServer
        Q.set -< assetServer'
        returnA -< texture
    )
    >>> S.queueWith
      ( \texture -> do
          A.spawn_ Window {windowTitle = "Aztecs"}
          A.spawn_ $
            Image {imageTexture = texture, imageSize = V2 100 100}
              :& transform {transformPosition = V2 100 100}
          A.spawn_ $
            Image {imageTexture = texture, imageSize = V2 200 200}
              :& transform {transformPosition = V2 500 100}
      )

main :: IO ()
main = runSystem_ $ SDL.setup >>> setup >>> S.forever SDL.update
