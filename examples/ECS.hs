{-# LANGUAGE FlexibleContexts #-}

module Main where

import Aztecs.ECS
import Aztecs.ECS.Access
import qualified Aztecs.ECS.Access as A
import Aztecs.ECS.Entities
import Aztecs.ECS.Query
import Aztecs.ECS.System
import Control.Monad.IO.Class
import qualified Data.SparseSet as S
import Data.SparseSet.Mutable (PrimMonad (..))

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

setup ::
  ( MonadEntities m,
    MonadAccess Position m,
    MonadAccess Velocity m
  ) =>
  m ()
setup = do
  e <- spawn
  A.insert e $ Position 0
  A.insert e $ Velocity 1

move ::
  ( MonadEntities m,
    MonadSystem (ComponentRef (PrimState m) Position) m,
    MonadSystem (ComponentRef (PrimState m) Velocity) m,
    MonadIO m,
    PrimMonad m
  ) =>
  m ()
move = do
  q <- runQuery $ (,,) <$> entities <*> query <*> query
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
