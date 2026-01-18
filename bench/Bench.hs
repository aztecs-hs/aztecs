{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Aztecs.ECS
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.World
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Criterion.Main
import Data.Function
import Data.Functor.Identity
import Data.Vector (Vector)
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance (Monad m) => Component m Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance (Monad m) => Component m Velocity

move :: Query Position
move = Q.fetch & Q.fetchMap (\(Velocity v) (Position p) -> Position $ p + v)

run :: Query Position -> World Identity -> Vector Position
run q = fst . Q.query q . entities

runSystem :: Query Position -> World Identity -> Vector Position
runSystem q = fst . runAccess (system $ query q)

main :: IO ()
main = do
  let go wAcc = snd . runIdentity $ W.spawn (bundle (Position 0) <> bundle (Velocity 1)) wAcc
      !w = foldr (const go) W.empty [0 :: Int .. 10000]
  defaultMain
    [ bench "iter" $ nf (run move) w,
      bench "iterSystem" $ nf (runSystem move) w
    ]
