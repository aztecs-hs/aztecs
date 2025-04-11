{-# LANGUAGE FlexibleContexts #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS as ECS
import qualified Data.SparseSet.Strict as S

newtype Position = Position Int

app :: (MonadAccess Position m) => m ()
app = ECS.insert (mkEntity 0 0) (Position 1)

main :: IO ()
main = do
  _ <- runAccessT app S.empty
  return ()
