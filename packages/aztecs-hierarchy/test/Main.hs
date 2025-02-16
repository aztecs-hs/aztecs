{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Data.Aztecs
import Data.Aztecs.Hierarchy (Children (..), Parent (..))
import qualified Data.Aztecs.Hierarchy as Hierarchy
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Data.Functor.Identity (Identity (..))
import qualified Data.Set as Set
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Data.Aztecs.Hierarchy.update" $ do
    it "adds Parent components to children" $ property prop_addParents

prop_addParents :: Expectation
prop_addParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, w'') <- runSchedule (schedule Hierarchy.update) w' ()
  let (res, _) = runIdentity $ Q.all Q.fetch w''
  res `shouldMatchList` [Parent e]
