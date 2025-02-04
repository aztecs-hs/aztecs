module Main (main) where

import Control.Arrow ((&&&))
import Data.Aztecs
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
main = hspec $ do
  describe "Data.Aztecs.Query.all" $ do
    it "queries multiple components" $ do
      let (_, w) = W.spawn (bundle $ X 0) W.empty
          (_, w') = W.spawn (bundle $ X 1) w
      (xs, _) <- Q.all (Q.fetch) w'
      xs `shouldMatchList` [X 0, X 1]
    it "queries a group of components" $ do
      let (e, w) = W.spawn (bundle $ X 0) W.empty
          w' = W.insert e (Y 1) w
      (xs, _) <- Q.all (Q.fetch &&& Q.fetch) w'
      xs `shouldMatchList` [(X 0, Y 1)]