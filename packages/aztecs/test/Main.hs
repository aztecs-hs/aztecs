{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Arrow ((&&&))
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Data.Functor.Identity (Identity (..))
import Test.Hspec
import Test.QuickCheck

newtype X = X Int deriving (Eq, Show, Arbitrary)

instance Component X

newtype Y = Y Int deriving (Eq, Show)

instance Component Y

newtype Z = Z Int deriving (Eq, Show)

instance Component Z

main :: IO ()
main = hspec $ do
  describe "Data.Aztecs.Query.all" $ do
    it "queries multiple components" $ property prop_queryMultipleComponents
    it "queries a group of components" $ do
      let (e, w) = W.spawn (bundle $ X 0) W.empty
          w' = W.insert e (Y 1) w
      (xs, _) <- Q.all (Q.fetch &&& Q.fetch) w'
      xs `shouldMatchList` [(X 0, Y 1)]

prop_queryMultipleComponents :: [X] -> Expectation
prop_queryMultipleComponents xs =
  let w = foldr (\x -> snd . W.spawn (bundle x)) W.empty xs
      (queriedXs, _) = runIdentity $ Q.all Q.fetch w
   in queriedXs `shouldMatchList` xs
