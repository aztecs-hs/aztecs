{-# LANGUAGE FlexibleContexts #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS as ECS
import qualified Data.SparseSet.Strict as S

newtype Position = Position Int

app :: (MonadEntities m, MonadAccess Position m) => m ()
app = do
  e <- spawn
  ECS.insert e $ Position 1

main :: IO ()
main = do
  _ <- runEntitiesT (runAccessT app S.empty) emptyEntityCounter
  return ()
