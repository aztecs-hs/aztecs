{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Aztecs.ECS
import qualified Aztecs.ECS as ECS
import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Criterion.Main
import qualified Data.SparseSet.Strict as S
import GHC.Generics

newtype Position = Position Int deriving (Generic, NFData)

newtype Velocity = Velocity Int

move ::
  ( MonadQuery (ComponentRef (PrimState m) Position) m,
    MonadQuery (ComponentRef (PrimState m) Velocity) m,
    PrimMonad m
  ) =>
  m ()
move = do
  q <- runQuery $ (,) <$> query <*> query
  mapM_ go q
  where
    go (pRef, vRef) = do
      Velocity v <- readComponentRef vRef
      modifyComponentRef pRef $ \(Position p) -> Position (p + v)

setup ::
  ( MonadEntities m,
    MonadAccess Position m,
    MonadAccess Velocity m
  ) =>
  m ()
setup = replicateM_ 10000 $ do
  e <- spawn
  ECS.insert e $ Position 0
  ECS.insert e $ Velocity 1

main :: IO ()
main = do
  (((_, p), v), _) <- runEntitiesT (runAccessT (runAccessT setup S.empty) S.empty) emptyEntityCounter
  let run ps vs = runST $ do
        !ps' <- S.unsafeThaw ps
        !vs' <- S.unsafeThaw vs
        _ <- runSystemT (runSystemT move ps') vs'
        S.unsafeFreeze ps'
  defaultMain [bench "iter" $ whnf (run p) v]
