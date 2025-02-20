{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aztecs
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.System as S
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Text (Text (..))
import qualified Aztecs.SDL.Text as Text
import Control.Arrow ((>>>))
import SDL (V2 (..))

setup :: System () ()
setup = proc () -> do
  fontHandle <- load $ asset "assets/C&C Red Alert [INET].ttf" 48 -< ()
  S.queue
    ( \fontHandle -> do
        A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
        A.spawn_ $ bundle Camera {cameraViewport = V2 1000 500, cameraScale = 2} <> bundle transform2d
        A.spawn_ $
          bundle
            Text
              { textContent = "Hello, Aztecs!",
                textFont = fontHandle
              }
            <> bundle transform2d
    )
    -<
      fontHandle

app :: Schedule IO () ()
app =
  SDL.setup
    >>> Text.setup
    >>> system setup
    >>> forever_
      ( Text.load
          >>> SDL.update
          >>> Text.draw
          >>> SDL.draw
      )

main :: IO ()
main = runSchedule_ app
