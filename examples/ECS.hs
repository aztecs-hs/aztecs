{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS as ECS
import Control.Monad.IO.Class
import qualified Data.SparseSet as S
import Data.SparseSet.Mutable (PrimMonad (..))

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

s ::
  forall m.
  ( MonadEntities m,
    MonadQuery (ComponentRef (PrimState m) Position) m,
    MonadQuery (ComponentRef (PrimState m) Velocity) m,
    MonadIO m,
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
    go (e, pRef, vRef) = do
      Velocity v <- readComponentRef vRef
      Position p <- readComponentRef pRef
      writeComponentRef pRef (Position $ p + v)

      p' <- readComponentRef pRef
      liftIO $ print (e, p')

app ::
  ( MonadEntities m,
    MonadAccess Position m,
    MonadAccess Velocity m,
    MonadQuery Position m,
    MonadQuery Velocity m,
    MonadIO m
  ) =>
  m ()
app = do
  e <- spawn
  ECS.insert e $ Position 0
  ECS.insert e $ Velocity 1
  x <- runQuery $ (,,) <$> entities <*> query @Position <*> query @Velocity
  liftIO $ print x

main :: IO ()
main = do
  (((_, ps), vs), es) <- runEntitiesT (runAccessT (runAccessT app S.empty) S.empty) emptyEntityCounter
  vs' <- S.thaw vs
  ps' <- S.thaw ps
  _ <- runEntitiesT (runSystemT (runSystemT s vs') ps') es
  return ()
