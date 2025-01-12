{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Aztecs.Entity (Component, entity, (<&>))
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Test.Hspec

newtype X = X Int deriving (Eq, Show)

instance Component X

newtype Y = Y Int deriving (Eq, Show)

instance Component Y

main :: IO ()
main = hspec $ do
  describe "Data.Aztecs.Query.all" $ do
    it "queries multiple components" $ do
      let (_, w) = W.spawn (entity $ X 0) W.empty
          (_, w') = W.spawn (entity $ X 1) w
          (x, _) = Q.all' (Q.read) w'
      x `shouldMatchList` [X 0, X 1]
    it "queries multiple groups of components" $ do
      let (_, w) = W.spawn (entity (X 0) <&> Y 0) W.empty
          (_, w') = W.spawn (entity (X 1) <&> Y 1) w
          (x, _) = Q.all' ((,) <$> Q.read <*> Q.read) w'
      x `shouldMatchList` [(X 0, Y 0), (X 1, Y 1)]
    it "queries multiple nested components" $ do
      let (_, w) = W.spawn (entity (X 0) <&> Y 0) W.empty
          (_, w') = W.spawn (entity (X 1) <&> Y 1) w
          (x, _) = Q.all' Q.read w'
      x `shouldMatchList` [X 0, X 1]
    it "writes to nested components" $ do
      let (_, w) = W.spawn (entity (X 0) <&> Y 0) W.empty
          (_, w') = W.spawn (entity (X 1) <&> Y 1) w
          (q, _) = Q.all' (Q.write (\(X x) -> X $ x + 1)) w'
      q `shouldMatchList` [X 1, X 2]
