{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (load)
import Data.Aztecs.SDL (Image (..), Window (..), sdlPlugin)
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..), transform)
import SDL (V2 (..))

data Setup

instance System IO Setup where
  task =
    S.mapSingleWith (\assetServer -> load "example.png" assetServer)
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

app :: Scheduler IO
app = sdlPlugin <> schedule @_ @Startup @Setup []

main :: IO ()
main = run app
