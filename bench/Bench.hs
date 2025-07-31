{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Aztecs.ECS
import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Criterion.Main
import qualified Data.SparseSet.Strict as S
import GHC.Generics
import Aztecs.ECS.System
import Aztecs.ECS.Access
import Aztecs.ECS.Entities
import Aztecs.ECS.Query
import qualified Aztecs.ECS.Access as A

newtype Position = Position Int deriving (Generic, NFData)

newtype Velocity = Velocity Int

move ::
  ( MonadSystem (W (PrimState m) Position) m,
    MonadSystem (W (PrimState m) Velocity) m,
    PrimMonad m
  ) =>
  m ()
move = do
  q <- runQuery $ (,) <$> query <*> query
  mapM_ go q
  where
    go (pRef, vRef) = do
      Velocity v <- readW vRef
      modifyW pRef $ \(Position p) -> Position (p + v)

setup ::
  ( MonadEntities m,
    MonadAccess Position m,
    MonadAccess Velocity m
  ) =>
  m ()
setup = replicateM_ 10000 $ do
  e <- spawn
  A.insert e $ Position 0
  A.insert e $ Velocity 1

main :: IO ()
main = do
  (((_, p), v), _) <- runEntitiesT (runAccessT (runAccessT setup S.empty) S.empty) emptyEntities
  let run ps vs = runST $ do
        !ps' <- S.unsafeThaw ps
        !vs' <- S.unsafeThaw vs
        _ <- runSystemT (runSystemT move ps') vs'
        S.unsafeFreeze ps'
  defaultMain [bench "iter" $ whnf (run p) v]
