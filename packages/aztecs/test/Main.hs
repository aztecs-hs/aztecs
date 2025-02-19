{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Arrow ((&&&))
import Data.Aztecs
import Data.Aztecs.Hierarchy (Children (..), Parent (..))
import qualified Data.Aztecs.Hierarchy as Hierarchy
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

newtype X = X Int deriving (Eq, Show, Arbitrary)

instance Component X

newtype Y = Y Int deriving (Eq, Show, Arbitrary)

instance Component Y

newtype Z = Z Int deriving (Eq, Show, Arbitrary)

instance Component Z

main :: IO ()
main = hspec $ do
  describe "Data.Aztecs.Query.all" $ do
    it "queries a single component" $ property prop_queryOneComponent
    it "queries two components" $ property prop_queryTwoComponents
    it "queries three components" $ property prop_queryThreeComponents
  describe "Data.Aztecs.Hierarchy.update" $ do
    it "adds Parent components to children" $ property prop_addParents

prop_queryOneComponent :: [X] -> Expectation
prop_queryOneComponent xs =
  let w = foldr (\x -> snd . W.spawn (bundle x)) W.empty xs
      (res, _) = Q.all Q.fetch w
   in res `shouldMatchList` xs

prop_queryTwoComponents :: [(X, Y)] -> Expectation
prop_queryTwoComponents xys =
  let w = foldr (\(x, y) -> snd . W.spawn (bundle x <> bundle y)) W.empty xys
      (res, _) = Q.all (Q.fetch &&& Q.fetch) w
   in res `shouldMatchList` xys

prop_queryThreeComponents :: [(X, Y, Z)] -> Expectation
prop_queryThreeComponents xyzs =
  let w = foldr (\(x, y, z) -> snd . W.spawn (bundle x <> bundle y <> bundle z)) W.empty xyzs
      q = do
        x <- Q.fetch
        y <- Q.fetch
        z <- Q.fetch
        pure (x, y, z)
      (res, _) = Q.all q w
   in res `shouldMatchList` xyzs

prop_addParents :: Expectation
prop_addParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, w'') <- runSchedule (system Hierarchy.update) w' ()
  let (res, _) = Q.all Q.fetch w''
  res `shouldMatchList` [Parent e]
