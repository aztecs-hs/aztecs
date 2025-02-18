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
import Data.Aztecs.SDL.Image (Sprite (..), spriteAnimationGrid)
import qualified Data.Aztecs.SDL.Image as IMG
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (Point (..), Rectangle (..), V2 (..))

setup :: Schedule IO () ()
setup = proc () -> do
  assetServer <- reader $ S.single Q.fetch -< ()
  (texture, assetServer') <- task $ load "assets/characters.png" () -< assetServer
  system $ S.mapSingle Q.set -< assetServer'
  access
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
            <> bundle (spriteAnimationGrid (V2 32 32) (map (\i -> V2 (18 + i) 1) [0 .. 3]))
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
