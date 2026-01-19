{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Aztecs.ECS
import Control.Monad.IO.Class

newtype Position = Position Int deriving (Show)

instance (Monad m) => Component m Position

newtype Velocity = Velocity Int deriving (Show)

instance (Monad m) => Component m Velocity

move :: (Monad m) => Query m Position
move = fetchMapWith go fetch
  where
    go (Velocity v) (Position p) = Position $ p + v

app :: Access IO ()
app = do
  spawn_ $ bundle (Position 0) <> bundle (Velocity 1)
  positions <- system $ query move
  liftIO $ print positions

main :: IO ()
main = runAccess_ app
