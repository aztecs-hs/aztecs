{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Aztecs.ECS
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.World
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Criterion.Main
import Data.Function
import Data.Functor.Identity
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

q :: Query Position
q = fetch & zipFetchMap (\(Velocity v) (Position p) -> Position $ p + v)

run :: World -> [Position]
run = fst . runIdentity . Q.query q . entities

main :: IO ()
main = do
  let go wAcc = snd $ W.spawn (bundle (Position 0) <> bundle (Velocity 1)) wAcc
      !w = foldr (const go) W.empty [0 :: Int .. 10000]
  defaultMain [bench "iter" $ nf run w]
