{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow (returnA, (>>>))
import Aztecs
import qualified Aztecs.ECS.Access as A
import Aztecs.ECS.Asset (load)
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.SDL (Camera (..), Window (..))
import qualified Aztecs.ECS.SDL as SDL
import Aztecs.ECS.SDL.Text (Text (..))
import qualified Aztecs.ECS.SDL.Text as Text
import qualified Aztecs.ECS.System as S
import Aztecs.ECS.Transform (transform)
import SDL (V2 (..))

setup :: Schedule IO () ()
setup = proc () -> do
  fontHandle <-
    system $
      S.mapSingle
        ( proc () -> do
            assetServer <- Q.fetch -< ()
            let (font, assetServer') = load "assets/C&C Red Alert [INET].ttf" 48 assetServer
            Q.set -< assetServer'
            returnA -< font
        )
      -<
        ()
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
