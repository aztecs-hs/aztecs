{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.SDL (Window (..), sdlPlugin)
import qualified Data.Aztecs.System as S

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

data Setup

instance System IO Setup where
  task = S.queue (A.spawn_ (Window {windowTitle = "Aztecs"}))

app :: Scheduler IO
app = sdlPlugin <> schedule @_ @Startup @Setup []

main :: IO ()
main = run app
