module Main (main) where

import Data.Aztecs

newtype X = X Int deriving (Eq, Show)

instance Component X

newtype Y = Y Int deriving (Eq, Show)

instance Component Y

main :: IO ()
main = return ()
