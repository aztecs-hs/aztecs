{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aztecs.Scheduler where

import Data.Aztecs.System (System (..), Task)
import Data.Data (Proxy (..), TypeRep, Typeable, typeOf)
import Data.Map (Map)
import qualified Data.Map as Map

data Node m = Node
  { nodeTask :: Task m () (),
    nodeBefore :: [TypeRep],
    nodeAfter :: [TypeRep]
  }

instance Show (Node m) where
  show n = "Node " ++ "{ nodeBefore = " ++ show (nodeBefore n) ++ ", nodeAfter = " ++ show (nodeAfter n) ++ " }"

data Constraint = Constraint {constraintKind :: ConstraintKind, constraintType :: TypeRep}
  deriving (Eq, Show)

data ConstraintKind = Before | After deriving (Eq, Show)

before :: forall m a. (System m a) => Constraint
before = Constraint Before (typeOf (Proxy @a))

after :: forall m a. (System m a) => Constraint
after = Constraint After (typeOf (Proxy @a))

data Startup

data Update

newtype Stage m = Stage {unStage :: Map TypeRep (Node m)}
  deriving (Show, Semigroup, Monoid)

newtype Scheduler m = Scheduler {unScheduler :: Map TypeRep (Stage m)}
  deriving (Show, Monoid)

instance Semigroup (Scheduler m) where
  s1 <> s2 = Scheduler (Map.unionWith (<>) (unScheduler s1) (unScheduler s2))

schedule :: forall m l a. (Typeable l, System m a) => [Constraint] -> Scheduler m
schedule cs =
  let (bs, as) =
        foldr
          ( \c (bAcc, aAcc) -> case constraintKind c of
              Before -> (constraintType c : bAcc, aAcc)
              After -> (bAcc, constraintType c : aAcc)
          )
          ([], [])
          cs
   in Scheduler
        ( Map.singleton
            (typeOf (Proxy @l))
            ( Stage
                ( Map.singleton
                    (typeOf (Proxy @a))
                    (Node {nodeTask = task @m @a, nodeBefore = bs, nodeAfter = as})
                )
            )
        )
