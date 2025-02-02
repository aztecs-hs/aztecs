{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.SDL (Image (..), Window (..), WindowTarget (..), load, sdlPlugin)
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (transformPosition), transform)
import SDL hiding (Window, windowTitle)

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

data Setup

instance System IO Setup where
  task =
    S.mapSingleWith
      ( \assetServer -> do
          (assetId, assetServer') <- load "example.png" assetServer
          return (assetId, assetServer')
      )
      >>> S.queueWith
        ( \(assetId, _) -> do
            eId <- A.spawn (Window {windowTitle = "Aztecs"})
            A.spawn_ $
              Image {imageAssetId = assetId, imageSize = V2 100 100}
                :& ( transform {transformPosition = V2 100 100}
                       :& WindowTarget eId
                   )
            A.spawn_ $
              Image {imageAssetId = assetId, imageSize = V2 200 200}
                :& ( transform {transformPosition = V2 500 100}
                       :& WindowTarget eId
                   )
        )

app :: Scheduler IO
app = sdlPlugin <> schedule @_ @Startup @Setup []

main :: IO ()
main = run app
