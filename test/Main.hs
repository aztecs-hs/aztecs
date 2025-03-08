{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Aztecs.ECS
import Aztecs.ECS.Component (ComponentID (ComponentID))
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.Query.Dynamic as Q
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Data.Functor.Identity (Identity (runIdentity))
import Data.Word
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
  describe "Aztecs.ECS.Query.querySingle" $ do
    it "queries a single entity" prop_querySingle
  describe "Aztecs.ECS.Query.queryMapSingle" $ do
    it "maps a single entity" $ property prop_queryMapSingle
  describe "Aztecs.ECS.Query.query" $ do
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
  let res = fst . runIdentity . Q.query (fetch @_ @X) $ W.entities W.empty in res `shouldMatchList` []

-- | Query all components from a list of `ComponentID`s.
queryComponentIds ::
  forall f a.
  (Monad f, Component a) =>
  [ComponentID] ->
  QueryT f (EntityID, [a])
queryComponentIds =
  let go cId qAcc = do
        x' <- Q.fromDyn $ Q.fetchDyn @a cId
        (e, xs) <- qAcc
        return (e, x' : xs)
   in foldr go ((,) <$> Q.entity <*> pure [])

prop_queryDyn :: [[X]] -> Expectation
prop_queryDyn xs =
  let s xs' (acc, wAcc) =
        let s' x (bAcc, cAcc, idAcc) =
              ( fromDynBundle (dynBundle (ComponentID idAcc) x) <> bAcc,
                (x, ComponentID idAcc) : cAcc,
                idAcc + 1
              )
            (b, cs, _) = foldr s' (mempty, [], 0) xs'
            (e, wAcc') = W.spawn b wAcc
         in ((e, cs) : acc, wAcc')
      (es, w) = foldr s ([], W.empty) xs
      go (e, cs) = do
        let q = queryComponentIds $ map snd cs
            (res, _) = runIdentity . Q.query q $ W.entities w
        return $ res `shouldContain` [(e, map fst cs)]
   in mapM_ go es

prop_queryTypedComponent :: [X] -> Expectation
prop_queryTypedComponent xs = do
  let w = foldr (\x -> snd . W.spawn (bundle x)) W.empty xs
      (res, _) = runIdentity . Q.query Q.fetch $ W.entities w
  res `shouldMatchList` xs

prop_queryTwoTypedComponents :: [(X, Y)] -> Expectation
prop_queryTwoTypedComponents xys = do
  let w = foldr (\(x, y) -> snd . W.spawn (bundle x <> bundle y)) W.empty xys
      (res, _) = runIdentity . Q.query ((,) <$> Q.fetch <*> Q.fetch) $ W.entities w
  res `shouldMatchList` xys

prop_queryThreeTypedComponents :: [(X, Y, Z)] -> Expectation
prop_queryThreeTypedComponents xyzs = do
  let w = foldr (\(x, y, z) -> snd . W.spawn (bundle x <> bundle y <> bundle z)) W.empty xyzs
      q = do
        x <- Q.fetch
        y <- Q.fetch
        z <- Q.fetch
        pure (x, y, z)
      (res, _) = runIdentity . Q.query q $ W.entities w
  res `shouldMatchList` xyzs

prop_querySingle :: Expectation
prop_querySingle =
  let (_, w) = W.spawn (bundle $ X 1) W.empty
      (res, _) = runIdentity . Q.readSingle Q.fetch $ W.entities w
   in res `shouldBe` X 1

prop_queryMapSingle :: Word8 -> Expectation
prop_queryMapSingle n =
  let (_, w) = W.spawn (bundle $ X 0) W.empty
      q = Q.zipFetchMap (\_ (X x) -> X $ x + 1) (pure ())
      w' = foldr (\_ es -> snd . runIdentity $ Q.querySingle q es) (W.entities w) [1 .. n]
      (res, _) = runIdentity $ Q.readSingle Q.fetch w'
   in res `shouldBe` X (fromIntegral n)
