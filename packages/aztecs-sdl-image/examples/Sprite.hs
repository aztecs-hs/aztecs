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
import Data.Aztecs.SDL.Image (Sprite (..), spriteAnimationGrid)
import qualified Data.Aztecs.SDL.Image as IMG
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (Point (..), Rectangle (..), V2 (..))

setup :: System () ()
setup =
  S.mapSingle
    ( proc () -> do
        assetServer <- Q.fetch -< ()
        (texture, assetServer') <- Q.run $ load "assets/characters.png" () -< assetServer
        Q.set -< assetServer'
        returnA -< texture
    )
    >>> S.queue
      ( \texture -> do
          A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
          A.spawn_ $
            bundle Camera {cameraViewport = V2 1000 500, cameraScale = 5}
              <> bundle transform
          A.spawn_ $
            bundle
              Sprite
                { spriteTexture = texture,
                  spriteSize = V2 300 300,
                  spriteBounds = Just $ Rectangle (P $ V2 0 32) (V2 32 32)
                }
              <> bundle (spriteAnimationGrid (V2 576 32) (V2 32 32) 3)
              <> bundle transform {transformPosition = V2 10 10}
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
