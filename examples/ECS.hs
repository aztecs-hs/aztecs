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
  let (e, w) = W.spawn (Position 0) (W.empty)
      (x, w') = Q.lookup e (Q.fetch @Position) w
  print x
