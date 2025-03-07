{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Aztecs.ECS
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Function
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

move :: (Monad m) => QueryT m Position
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
