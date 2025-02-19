{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aztecs
import Aztecs.Asset (load)
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.SDL (Camera (..), Window (..))
import qualified Aztecs.SDL as SDL
import Aztecs.SDL.Text (Text (..))
import qualified Aztecs.SDL.Text as Text
import Aztecs.Transform (transform)
import Control.Arrow (returnA, (>>>))
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
