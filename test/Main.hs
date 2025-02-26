{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Aztecs
import Aztecs.ECS.Component (ComponentID (ComponentID))
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import qualified Aztecs.ECS.World as W
import Aztecs.Hierarchy (Children (..), Parent (..))
import qualified Aztecs.Hierarchy as Hierarchy
import Control.Arrow (Arrow (..), returnA, (&&&))
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
    it "queries dynamic components" $ property prop_queryDyn
    it "queries a typed component" $ property prop_queryTypedComponent
    it "queries 2 typed components" $ property prop_queryTwoTypedComponents
    it "queries 3 typed components" $ property prop_queryThreeTypedComponents
  describe "Aztecs.ECS.Hierarchy.update" $ do
    it "adds Parent components to children" $ property prop_addParents
    it "removes Parent components from removed children" $ property prop_removeParents
  describe "Aztecs.ECS.Schedule" $ do
    it "queries entities" $ property prop_scheduleQueryEntity
    it "updates components" prop_scheduleUpdate

-- | Query all components from a list of `ComponentID`s.
queryComponentIds ::
  forall q a.
  (ArrowDynamicQueryReader q, Component a) =>
  [ComponentID] ->
  q () (EntityID, [a])
queryComponentIds =
  let go cId qAcc = proc () -> do
        x' <- Q.fetchDyn @_ @a cId -< ()
        (e, xs) <- qAcc -< ()
        returnA -< (e, x' : xs)
   in foldr go (Q.entity &&& arr (const []))

prop_queryDyn :: [[X]] -> Expectation
prop_queryDyn xs =
  let spawn xs' (acc, wAcc) =
        let spawn' x (bAcc, cAcc, idAcc) =
              ( dynBundle (ComponentID idAcc) x <> bAcc,
                (x, ComponentID idAcc) : cAcc,
                idAcc + 1
              )
            (b, cs, _) = foldr spawn' (mempty, [], 0) xs'
            (e, wAcc') = W.spawn b wAcc
         in ((e, cs) : acc, wAcc')
      (es, w) = foldr spawn ([], W.empty) xs
      go (e, cs) =
        let q = queryComponentIds $ map snd cs
            (res, _) = Q.all () q $ W.entities w
         in res `shouldContain` [(e, map fst cs)]
   in mapM_ go es

prop_queryTypedComponent :: [X] -> Expectation
prop_queryTypedComponent xs =
  let w = foldr (\x -> snd . W.spawn (bundle x)) W.empty xs
      (res, _) = Q.all () Q.fetch $ W.entities w
   in res `shouldMatchList` xs

prop_queryTwoTypedComponents :: [(X, Y)] -> Expectation
prop_queryTwoTypedComponents xys =
  let w = foldr (\(x, y) -> snd . W.spawn (bundle x <> bundle y)) W.empty xys
      (res, _) = Q.all () (Q.fetch &&& Q.fetch) $ W.entities w
   in res `shouldMatchList` xys

prop_queryThreeTypedComponents :: [(X, Y, Z)] -> Expectation
prop_queryThreeTypedComponents xyzs =
  let w = foldr (\(x, y, z) -> snd . W.spawn (bundle x <> bundle y <> bundle z)) W.empty xyzs
      q = do
        x <- Q.fetch
        y <- Q.fetch
        z <- Q.fetch
        pure (x, y, z)
      (res, _) = Q.all () q $ W.entities w
   in res `shouldMatchList` xyzs

prop_addParents :: Expectation
prop_addParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, _, w'') <- runSchedule (system Hierarchy.update) w' ()
  let (res, _) = Q.all () Q.fetch $ W.entities w''
  res `shouldMatchList` [Parent e]

prop_removeParents :: Expectation
prop_removeParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, _, w'') <- runSchedule (system Hierarchy.update) w' ()
  let w''' = W.insert e (Children Set.empty) w''
  (_, _, w'''') <- runSchedule (system Hierarchy.update) w''' ()
  let (res, _) = Q.all () (Q.fetch @_ @Parent) $ W.entities w''''
  res `shouldMatchList` []

prop_scheduleQueryEntity :: [X] -> Expectation
prop_scheduleQueryEntity xs = do
  let go x (eAcc, wAcc) = let (e, wAcc') = W.spawn (bundle x) wAcc in (e : eAcc, wAcc')
      (es, w) = foldr go ([], W.empty) xs
  (res, _, _) <- runSchedule (reader $ S.all Q.entity) w ()
  res `shouldMatchList` es

prop_scheduleUpdate :: Expectation
prop_scheduleUpdate = do
  let (_, w) = W.spawn (bundle $ X 1) W.empty
  (_, _, w') <- runSchedule update w ()
  (x, _, _) <- runSchedule update w' ()
  x `shouldBe` True

update :: Schedule IO () Bool
update = proc () -> do
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
