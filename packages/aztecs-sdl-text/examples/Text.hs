{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aztecs
import Aztecs.Asset (asset, load)
import qualified Aztecs.ECS.Access as A
import Aztecs.SDL (Camera (..), Window (..))
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Text (Text (..))
import qualified Aztecs.SDL.Text as Text
import Aztecs.Transform (transform)
import Control.Arrow ((>>>))
import SDL (V2 (..))

setup :: Schedule IO () ()
setup = proc () -> do
  fontHandle <- system . load $ asset "assets/C&C Red Alert [INET].ttf" 48 -< ()
  access
    ( \fontHandle -> do
        A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
        A.spawn_ $ bundle Camera {cameraViewport = V2 1000 500, cameraScale = 2} <> bundle transform
        A.spawn_ $
          bundle
            Text
              { textContent = "Hello, Aztecs!",
                textFont = fontHandle
              }
            <> bundle transform
    )
    -<
      fontHandle

app :: Schedule IO () ()
app =
  SDL.setup
    >>> Text.setup
    >>> setup
    >>> forever_
      ( Text.load
          >>> SDL.update
          >>> Text.draw
          >>> SDL.draw
      )

main :: IO ()
main = runSchedule_ app
