{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.ECS.World
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Criterion.Main
import Data.Function
import Data.Vector (Vector)
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

query :: Query Position
query = Q.fetch & Q.adjust (\(Velocity v) (Position p) -> Position $ p + v)

run :: Query Position -> World -> Vector Position
run q = fst . Q.map q . entities

runSystem :: Query Position -> World -> Vector Position
runSystem q = fst . A.runAccess (A.system $ S.map q)

main :: IO ()
main = do
  let go wAcc = snd $ W.spawn (bundle (Position 0) <> bundle (Velocity 1)) wAcc
      !w = foldr (const go) W.empty [0 :: Int .. 10000]
  defaultMain
    [ bench "iter" $ nf (run query) w,
      bench "iterSystem" $ nf (runSystem query) w
    ]
