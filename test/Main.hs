module Main (main) where

import Data.Aztecs
import Data.Aztecs.Entity (ToEntity (toEntity))
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Test.Hspec

newtype X = X Int deriving (Eq, Show)

instance Component X

newtype Y = Y Int deriving (Eq, Show)

instance Component Y

newtype Z = Z Int deriving (Eq, Show)

instance Component Z

main :: IO ()
main = return ()
