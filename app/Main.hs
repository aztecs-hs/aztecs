{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs

data X = X Int deriving (Show)

instance Component X

main :: IO ()
main =
  let (e, w) = spawn (X 1) newWorld
   in print $ get @X e w
