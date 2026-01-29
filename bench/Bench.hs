{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

import Aztecs.ECS
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.World
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Criterion.Main
import Data.Functor.Identity
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance (Monad m) => Component m Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance (Monad m) => Component m Velocity

move :: (Applicative f, Monad m) => Q.Query f m (f Position)
move = do
  vs <- Q.query
  Q.queryMap $ \ps -> (\(Velocity v) (Position p) -> Position $ p + v) <$> vs <*> ps

run :: (forall f. (Applicative f) => Q.Query f Identity (f Position)) -> World Identity -> [Position]
run q = (\(a, _, _) -> a) . runIdentity . Q.runQuery q . entities

runSys :: (forall f. (Applicative f) => Q.Query f Identity (f Position)) -> World Identity -> [Position]
runSys q = fst . runIdentity . runAccess (system $ runQuery q)

main :: IO ()
main = do
  let go wAcc = (\(_, w', _) -> w') $ W.spawn (bundle (Position 0) <> bundle (Velocity 1)) wAcc
      !w = foldr (const go) W.empty [0 :: Int .. 10000]
  defaultMain
    [ bench "iter" $ nf (run move) w,
      bench "iterSystem" $ nf (runSys move) w
    ]
