{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow (returnA, (>>>))
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

setup :: System () ()
setup =
  S.mapSingle
    ( proc () -> do
        assetServer <- Q.fetch -< ()
        (texture, assetServer') <- Q.run $ load "assets/C&C Red Alert [INET].ttf" 50 -< assetServer
        Q.set -< assetServer'
        returnA -< texture
    )
    >>> S.queue
      ( \fontHandle -> do
          A.spawn_ $ bundle Window {windowTitle = "Aztecs"}
          A.spawn_ $ bundle Camera {cameraViewport = V2 1000 500} <> bundle transform
          A.spawn_ $
            bundle
              Text
                { textContent = "Hello, Aztecs!",
                  textFont = fontHandle
                }
              <> bundle transform
      )

main :: IO ()
main =
  runSystem_ $
    SDL.setup
      >>> Text.setup
      >>> setup
      >>> S.forever
        ( Text.load
            >>> SDL.update
            >>> Text.draw
            >>> SDL.draw
        )
