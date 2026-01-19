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
import Data.Functor.Identity (Identity (runIdentity))
import Data.Vector (Vector)
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance (Monad m) => Component m Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance (Monad m) => Component m Velocity

move :: (Monad m) => Query m Position
move = fetchMapWith (\(Velocity v) (Position p) -> (Position $ p + v)) fetch

run :: Query Identity Position -> World Identity -> Vector Position
run q = (\(a, _, _) -> a) . runIdentity . Q.query q . entities

runSystem :: Query Identity Position -> World Identity -> Vector Position
runSystem q = fst . runIdentity . runAccess (system $ query q)

main :: IO ()
main = do
  let go wAcc = (\(_, w', _) -> w') $ W.spawn (bundle (Position 0) <> bundle (Velocity 1)) wAcc
      !w = foldr (const go) W.empty [0 :: Int .. 10000]
  defaultMain
    [ bench "iter" $ nf (run move) w,
      bench "iterSystem" $ nf (runSystem move) w
    ]
