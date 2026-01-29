{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Aztecs
import Aztecs.ECS.Component
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.World as W
import Control.Applicative
import Data.Functor.Identity
import Data.Word
import GHC.Generics
import Test.Hspec
import Test.QuickCheck

newtype X = X Int deriving (Eq, Show, Arbitrary, Generic)

instance (Monad m) => Component m X

newtype Y = Y Int deriving (Eq, Show, Arbitrary, Generic)

instance (Monad m) => Component m Y

newtype Z = Z Int deriving (Eq, Show, Arbitrary, Generic)

instance (Monad m) => Component m Z

main :: IO ()
main = hspec $ do
  describe "Aztecs.ECS.Query.single" $ do
    it "queries a single entity" prop_querySingle
  describe "Aztecs.ECS.Query.mapSingle" $ do
    it "maps a single entity" $ property prop_queryMapSingle
  describe "Aztecs.ECS.Query.all" $ do
    it "queries an empty world" prop_queryEmpty
    it "queries dynamic components" $ property prop_queryDyn
    it "queries a typed component" $ property prop_queryTypedComponent
    it "queries 2 typed components" $ property prop_queryTwoTypedComponents
    it "queries 3 typed components" $ property prop_queryThreeTypedComponents

{-TODO
describe "Aztecs.ECS.System.mapSingle" $ do
  it "maps a single entity" $ property prop_systemMapSingle
describe "Aztecs.ECS.Hierarchy.update" $ do
  it "adds Parent components to children" $ property prop_addParents
  it "removes Parent components from removed children" $ property prop_removeParents
-}

prop_queryEmpty :: Expectation
prop_queryEmpty =
  let res =
        fst
          . runIdentity
          . Q.readQuery (Q.query @_ @_ @X)
          $ W.entities W.empty
   in res `shouldMatchList` []

-- | Query all components from a list of `ComponentID`s.
queryComponentIds ::
  forall a m.
  (Component m a) =>
  [ComponentID] ->
  (forall f. (Applicative f) => Q.Query f m (f (EntityID, [a])))
queryComponentIds cIds =
  let go cId qAcc = do
        x' <- Q.queryDyn @_ @_ @a cId
        acc <- qAcc
        return $ liftA2 (\x (e, xs) -> (e, x : xs)) x' acc
   in foldr
        go
        ( do
            e <- Q.entity
            return $ fmap (\eid -> (eid, [])) e
        )
        cIds

prop_queryDyn :: [[X]] -> Expectation
prop_queryDyn xs =
  let spawner :: [X] -> ([(EntityID, [(X, ComponentID)])], World Identity) -> ([(EntityID, [(X, ComponentID)])], World Identity)
      spawner xs' (acc, wAcc) =
        let spawner' x (bAcc, cAcc, idAcc) =
              ( dynBundle @Identity (ComponentID idAcc) x <> bAcc,
                (x, ComponentID idAcc) : cAcc,
                idAcc + 1
              )
            (b, cs, _) = foldr spawner' (mempty, [], 0) xs'
            (e, wAcc', _) = W.spawn b wAcc
         in ((e, cs) : acc, wAcc')
      (es, w) = foldr spawner ([], W.empty) xs
      go (e, cs) = do
        let (res, _) = runIdentity . Q.readQuery (queryComponentIds @X @Identity $ map snd cs) $ W.entities w
        return $ res `shouldContain` [(e, map fst cs)]
   in mapM_ go es

prop_queryTypedComponent :: [X] -> Expectation
prop_queryTypedComponent xs = do
  let w = foldr (\x -> (\(_, w', _) -> w') . W.spawn (bundle x)) W.empty xs
      (res, _) = runIdentity . Q.readQuery (Q.query @_ @_ @X) $ W.entities w
  res `shouldMatchList` xs

prop_queryTwoTypedComponents :: [(X, Y)] -> Expectation
prop_queryTwoTypedComponents xys = do
  let w = foldr (\(x, y) -> (\(_, w', _) -> w') . W.spawn (bundle x <> bundle y)) W.empty xys
      (res, _) =
        runIdentity
          $ Q.readQuery
            ( do
                x <- Q.query @_ @_ @X
                y <- Q.query @_ @_ @Y
                return $ liftA2 (,) x y
            )
          $ W.entities w
  res `shouldMatchList` xys

prop_queryThreeTypedComponents :: [(X, Y, Z)] -> Expectation
prop_queryThreeTypedComponents xyzs = do
  let w = foldr (\(x, y, z) -> (\(_, w', _) -> w') . W.spawn (bundle x <> bundle y <> bundle z)) W.empty xyzs
      (res, _) =
        runIdentity
          $ Q.readQuery
            ( do
                x <- Q.query @_ @_ @X
                y <- Q.query @_ @_ @Y
                z <- Q.query @_ @_ @Z
                return $ liftA3 (,,) x y z
            )
          $ W.entities w
  res `shouldMatchList` xyzs

prop_querySingle :: Expectation
prop_querySingle =
  let (_, w, _) = W.spawn (bundle $ X 1) W.empty
      (res, _) = runIdentity $ Q.readQuerySingle (Q.query @_ @_ @X) $ W.entities w
   in res `shouldBe` X 1

prop_queryMapSingle :: Word8 -> Expectation
prop_queryMapSingle n =
  let (_, w, _) = W.spawn (bundle $ X 0) W.empty
      w' = foldr (\_ es -> (\(_, es', _) -> es') . runIdentity $ Q.runQuerySingle (Q.queryMap @_ @_ @X $ fmap (\(X x) -> X $ x + 1)) es) (W.entities w) [1 .. n]
      (res, _) = runIdentity $ Q.readQuerySingle (Q.query @_ @_ @X) w'
   in res `shouldBe` X (fromIntegral n)

{-TODO
prop_systemMapSingle :: Word8 -> Expectation
prop_systemMapSingle n =
  let (_, w) = W.spawn (bundle $ X 0) W.empty
      q = Q.adjust (\_ (X x) -> X $ x + 1)
      s =  S.mapSingle q
      go _ wAcc = let (_, _, wAcc') = runIdentity $ runSchedule s wAcc () in wAcc'
      w' = foldr go w [1 .. n]
      (res, _) = Q.single () Q.query (W.entities w')
   in res `shouldBe` X (fromIntegral n)

prop_addParents :: Expectation
prop_addParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, _, w'') <- runSchedule (system Hierarchy.update) w' ()
  let (res, _) = Q.all () Q.query $ W.entities w''
  res `shouldMatchList` [Parent e]

prop_removeParents :: Expectation
prop_removeParents = do
  let (_, w) = W.spawnEmpty W.empty
      (e, w') = W.spawn (bundle . Children $ Set.singleton e) w
  (_, _, w'') <- runSchedule (system Hierarchy.update) w' ()
  let w''' = W.insert e (Children Set.empty) w''
  (_, _, w'''') <- runSchedule (system Hierarchy.update) w''' ()
  let (res, _) = Q.all () (Q.query @_ @Parent) $ W.entities w''''
  res `shouldMatchList` []
-}

{-
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
                X x <- Q.query -< ()
                Q.set -< X $ x + 1
                returnA -< x
            )
          -<
            ()
      let shouldQuit = (lastShouldQuit || x > 1)
  returnA -< shouldQuit
-}
