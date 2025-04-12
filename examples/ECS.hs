{-# LANGUAGE FlexibleContexts #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS as ECS
import Control.Monad.IO.Class
import qualified Data.SparseSet as S
import Data.SparseSet.Mutable (PrimMonad (..))

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

setup ::
  ( MonadEntities m,
    MonadAccess Position m,
    MonadAccess Velocity m,
    MonadIO m
  ) =>
  m ()
setup = do
  e <- spawn
  ECS.insert e $ Position 0
  ECS.insert e $ Velocity 1

move ::
  ( MonadEntities m,
    MonadQuery (ComponentRef (PrimState m) Position) m,
    MonadQuery (ComponentRef (PrimState m) Velocity) m,
    MonadIO m,
    PrimMonad m
  ) =>
  m ()
move = do
  q <-
    runQuery $
      (,,)
        <$> entities
        <*> query
        <*> query
  mapM_ go q
  where
    go (e, pRef, vRef) = do
      Velocity v <- readComponentRef vRef
      Position p <- readComponentRef pRef
      writeComponentRef pRef (Position $ p + v)

      p' <- readComponentRef pRef
      liftIO $ print (e, p')

main :: IO ()
main = do
  (((_, ps), vs), es) <- runEntitiesT (runAccessT (runAccessT setup S.empty) S.empty) emptyEntityCounter
  vs' <- S.thaw vs
  ps' <- S.thaw ps
  _ <- runEntitiesT (runSystemT (runSystemT move vs') ps') es
  return ()
