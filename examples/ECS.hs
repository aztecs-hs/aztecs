{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs.Component
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W

newtype Position = Position Int
  deriving (Show)

instance Component Position

main :: IO ()
main = do
  let (_, w) = W.spawn (Position 0) (W.empty)
      (_, w') = W.spawn (Position 1) w
      (x, w'') = Q.all (Q.fetch @Position) w'
  print (x, w'')
