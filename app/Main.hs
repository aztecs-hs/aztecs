module Main where

import Data.Aztecs

data X = X Int

instance Component X

main :: IO ()
main = print (spawn (X 1) newWorld)
