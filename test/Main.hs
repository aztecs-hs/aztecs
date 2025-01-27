module Main (main) where

import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Test.Hspec
import Data.Aztecs.Entity (ToEntity(toEntity))

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
      let (_, w) = W.spawn (X 0) W.empty
          (_, w') = W.spawn (X 1) w
          xs = Q.allWorld w'
      xs `shouldMatchList` [X 0, X 1]
    it "queries a group of components" $ do
      let (e, w) = W.spawn (X 0) W.empty
          w' = W.insert e (Y 1) w
          xs = Q.allWorld w'
      xs `shouldMatchList` [toEntity (X 0 :& Y 1)]
    it "queries a fragmented group of components" $ do
      let (e, w) = W.spawn (X 0) W.empty
          w' = W.insert e (Y 1) w
          w'' = W.insert e (Z 2) w'
          xs = Q.allWorld w''
      xs `shouldMatchList` [toEntity (Y 1 :& Z 2)]
