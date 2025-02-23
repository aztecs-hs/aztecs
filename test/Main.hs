{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Aztecs
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import qualified Aztecs.ECS.World as W
import Aztecs.Hierarchy (Children (..), Parent (..))
import qualified Aztecs.Hierarchy as Hierarchy
import Control.Arrow (returnA, (&&&))
import Control.DeepSeq
import qualified Data.Set as Set
import GHC.Generics
import Test.Hspec
import Test.QuickCheck

newtype X = X Int deriving (Eq, Show, Arbitrary, Generic, NFData)

instance Component X

newtype Y = Y Int deriving (Eq, Show, Arbitrary, Generic, NFData)

instance Component Y

newtype Z = Z Int deriving (Eq, Show, Arbitrary, Generic, NFData)

instance Component Z

main :: IO ()
main = hspec $ do
  describe "Aztecs.ECS.Query.all" $ do
    it "queries entities" $ property prop_queryEntity
    it "queries a single component" $ property prop_queryOneComponent
    it "queries two components" $ property prop_queryTwoComponents
    it "queries three components" $ property prop_queryThreeComponents
  describe "Aztecs.ECS.Hierarchy.update" $ do
    it "adds Parent components to children" $ property prop_addParents
    it "removes Parent components from removed children" $ property prop_removeParents
  describe "Aztecs.ECS.Schedule" $ do
    it "queries entities" $ property prop_scheduleQueryEntity
    it "increments components" prop_quit

prop_queryEntity :: [X] -> Expectation
prop_queryEntity xs = do
  let go x (eAcc, wAcc) = let (e, wAcc') = W.spawn (bundle x) wAcc in (e : eAcc, wAcc')
      (es, w) = foldr go ([], W.empty) xs
      (res, _) = Q.all Q.entity $ W.entities w
  res `shouldMatchList` es

prop_queryOneComponent :: [X] -> Expectation
prop_queryOneComponent xs =
  let w = foldr (\x -> snd . W.spawn (bundle x)) W.empty xs
      (res, _) = Q.all Q.fetch $ W.entities w
   in res `shouldMatchList` xs

prop_queryTwoComponents :: [(X, Y)] -> Expectation
prop_queryTwoComponents xys =
  let w = foldr (\(x, y) -> snd . W.spawn (bundle x <> bundle y)) W.empty xys
      (res, _) = Q.all (Q.fetch &&& Q.fetch) $ W.entities w
   in res `shouldMatchList` xys

prop_queryThreeComponents :: [(X, Y, Z)] -> Expectation
prop_queryThreeComponents xyzs =
  let w = foldr (\(x, y, z) -> snd . W.spawn (bundle x <> bundle y <> bundle z)) W.empty xyzs
      q = do
        x <- Q.fetch
        y <- Q.fetch
        z <- Q.fetch
        pure (x, y, z)
      (res, _) = Q.all q $ W.entities w
   in res `shouldMatchList` xyzs

prop_addParents :: Expectation
prop_addParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, _, w'') <- runSchedule (system Hierarchy.update) w' ()
  let (res, _) = Q.all Q.fetch $ W.entities w''
  res `shouldMatchList` [Parent e]

prop_removeParents :: Expectation
prop_removeParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, _, w'') <- runSchedule (system Hierarchy.update) w' ()
  let w''' = W.insert e (Children Set.empty) w''
  (_, _, w'''') <- runSchedule (system Hierarchy.update) w''' ()
  let (res, _) = Q.all (Q.fetch @_ @Parent) $ W.entities w''''
  res `shouldMatchList` []

prop_scheduleQueryEntity :: [X] -> Expectation
prop_scheduleQueryEntity xs = do
  let go x (eAcc, wAcc) = let (e, wAcc') = W.spawn (bundle x) wAcc in (e : eAcc, wAcc')
      (es, w) = foldr go ([], W.empty) xs
  (res, _, _) <- runSchedule (reader $ S.all Q.entity) w ()
  res `shouldMatchList` es

prop_quit :: Expectation
prop_quit = do
  let (_, w) = W.spawn (bundle $ X 1) W.empty
  (_, _, w') <- runSchedule quit w ()
  (x, _, _) <- runSchedule quit w' ()
  x `shouldBe` True

quit :: Schedule IO () Bool
quit = proc () -> do
  rec lastShouldQuit <- delay False -< shouldQuit
      x <-
        system $
          S.mapSingle
            ( proc () -> do
                X x <- Q.fetch -< ()
                Q.set -< X $ x + 1
                returnA -< x
            )
          -<
            ()
      let shouldQuit = (lastShouldQuit || x > 1)
  returnA -< shouldQuit
