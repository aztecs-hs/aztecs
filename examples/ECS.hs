module Main where

import Aztecs.ECS
import Control.Monad
import Control.Monad.IO.Class
import Data.Function

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int

instance Component Velocity

move :: QueryT m Position
move = fetch & zipFetchMap (\(Velocity v) (Position p) -> Position $ p + v)

run :: SystemT IO ()
run = do
  positions <- query move
  liftIO $ print positions

app :: AccessT IO ()
app = do
  _ <- spawn $ bundle (Position 0) <> bundle (Velocity 1)
  forever $ system run

main :: IO ()
main = runAccessT_ app
