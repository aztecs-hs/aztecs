{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (load)
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Camera (..), Window (..))
import qualified Data.Aztecs.SDL as SDL
import Data.Aztecs.SDL.Text (Text (..))
import qualified Data.Aztecs.SDL.Text as Text
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (transform)
import SDL (V2 (..))

setup :: Schedule IO () ()
setup = proc () -> do
  assetServer <- reader $ S.single Q.fetch -< ()
  (texture, assetServer') <- task $ load "assets/C&C Red Alert [INET].ttf" 48 -< assetServer
  system $ S.mapSingle Q.set -< assetServer'
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
      texture

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
