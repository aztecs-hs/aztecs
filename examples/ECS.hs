module Main where

import Aztecs.ECS
import Control.Monad.IO.Class

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

move :: Query Position
move = fetchMap (\(Velocity v) (Position p) -> Position $ p + v) fetch

app :: AccessT IO ()
app = do
  spawn_ $ bundle (Position 0) <> bundle (Velocity 1)
  positions <- system $ query move
  liftIO $ print positions

main :: IO ()
main = runAccessT_ app
