{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Test.Hspec

newtype X = X Int deriving (Eq, Show)

newtype Y = Y Int deriving (Eq, Show)

main :: IO ()
main = hspec $ do
  describe "Data.Aztecs.Query.all" $ do
    it "queries multiple components" $ do
      let (_, w) = W.spawn (X 0) W.empty
          (_, w') = W.spawn (X 1) w
          (x, _) = Q.all (Q.read) w'
      x `shouldMatchList` [X 0, X 1]
    it "queries multiple components" $ do
      let (e, w) = W.spawn (X 0) W.empty
          w' = W.insert e (Y 0) w
          (e', w'') = W.spawn (X 1) w'
          w''' = W.insert e' (Y 1) w''
          (x, _) = Q.all ((,) <$> Q.read <*> Q.read) w'''
      x `shouldMatchList` [(X 0, Y 0), (X 1, Y 1)]
    it "queries multiple nested components" $ do
      let (e, w) = W.spawn (X 0) W.empty
          w' = W.insert e (Y 0) w
          (e', w'') = W.spawn (X 1) w'
          w''' = W.insert e' (Y 1) w''
          (x, _) = Q.all Q.read w'''
      x `shouldMatchList` [X 0, X 1]
