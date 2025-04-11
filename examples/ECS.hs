{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS as ECS
import Control.Monad.IO.Class
import qualified Data.SparseSet.Strict as S

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

app ::
  ( MonadEntities m,
    MonadAccess Position m,
    MonadAccess Velocity m,
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
  _ <- runEntitiesT (runAccessT (runAccessT app S.empty) S.empty) emptyEntityCounter
  return ()
