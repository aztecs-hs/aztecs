{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Aztecs.ECS
import qualified Aztecs.ECS as ECS
import Control.DeepSeq (NFData)
import Control.Monad.Primitive
import Criterion.Main
import qualified Data.SparseSet as S
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

newtype Velocity = Velocity Int deriving (Show)

s ::
  forall m.
  ( MonadEntities m,
    MonadQuery (ComponentRef (PrimState m) Position) m,
    MonadQuery (ComponentRef (PrimState m) Velocity) m,
    PrimMonad m
  ) =>
  m ()
s = do
  x <-
    runQuery $
      (,,)
        <$> entities
        <*> query @(ComponentRef (PrimState m) Position)
        <*> query @(ComponentRef (PrimState m) Velocity)
  mapM_ go x
  where
    go (_, pRef, vRef) = do
      Velocity v <- readComponentRef vRef
      Position p <- readComponentRef pRef
      writeComponentRef pRef (Position $ p + v)

app ::
  ( MonadEntities m,
    MonadAccess Position m,
    MonadAccess Velocity m,
    MonadQuery Position m,
    MonadQuery Velocity m
  ) =>
  m ()
app = do
  e <- spawn
  ECS.insert e $ Position 0
  ECS.insert e $ Velocity 1

main :: IO ()
main = do
  (((_, ps), vs), es) <- runEntitiesT (runAccessT (runAccessT app S.empty) S.empty) emptyEntityCounter
  ps' <- S.thaw ps
  vs' <- S.thaw vs

  let run = do
        _ <- runEntitiesT (runSystemT (runSystemT s ps') vs') es
        S.freeze ps'

  defaultMain [bench "iter" $ nfIO run]
