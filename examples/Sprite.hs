{-# LANGUAGE Arrows #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.System as S
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Image (Sprite (..), spriteAnimationGrid)
import qualified Aztecs.SDL.Image as IMG
import Control.Arrow ((>>>))
import SDL (Point (..), Rectangle (..), V2 (..))

setup :: System () ()
setup = proc () -> do
  texture <- load $ asset "assets/characters.png" () -< ()
  S.queue
    ( \texture -> do
        A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
        A.spawn_ $
          bundle Camera {cameraViewport = V2 1000 500, cameraScale = 5}
            <> bundle transform2d
        A.spawn_ $
          bundle
            Sprite
              { spriteTexture = texture,
                spriteBounds = Just $ Rectangle (P $ V2 0 32) (V2 32 32)
              }
            <> bundle (spriteAnimationGrid (V2 32 32) (map (\i -> V2 (18 + i) 1) [0 .. 3]))
            <> bundle transform2d {transformTranslation = V2 10 10}
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
