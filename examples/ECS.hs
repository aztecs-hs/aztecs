{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs as W

main :: IO ()
main = do
  (e, w) <- W.spawn (W.ComponentID 0) (0 :: Int) W.empty
  x <- W.lookup @Int e (W.ComponentID 0) w
  print x