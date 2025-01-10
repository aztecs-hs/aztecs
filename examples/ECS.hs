{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs as W

main :: IO ()
main = do
  let (e, w) = W.spawn (0 :: Int) W.empty
      x = W.lookup @Int e w
  print x
