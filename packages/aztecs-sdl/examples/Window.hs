{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
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
  S.single (Q.mapAccum (\s -> load "example.png" s))
    >>> S.queueWith
      ( \(texture, _) -> do
          A.spawn_ Window {windowTitle = "Aztecs"}
          A.spawn_ $
            Image {imageTexture = texture, imageSize = V2 100 100}
              :& transform {transformPosition = V2 100 100}
          A.spawn_ $
            Image {imageTexture = texture, imageSize = V2 200 200}
              :& transform {transformPosition = V2 500 100}
      )

main :: IO ()
main = runSystem_ $ SDL.setup >>> setup >>> S.loop SDL.update
