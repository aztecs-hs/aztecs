{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs as W

main :: IO ()
main = do
  w <- W.insert (W.Entity 0) (W.ComponentID 0) (0 :: Int) W.empty
  x <- W.lookup @Int (W.Entity 0) (W.ComponentID 0) w
  print x