{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Aztecs.ECS
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.World
import qualified Aztecs.ECS.World as W
import Control.Arrow
import Control.DeepSeq
import Criterion.Main
import Data.Functor.Identity
import GHC.Generics

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

query :: Query () ()
query = Q.fetch >>> Q.adjust_ (\(Velocity v) (Position p) -> Position $ p + v)

queryDo :: Query () ()
queryDo = proc () -> do
  Velocity v <- Q.fetch -< ()
  Q.adjust_ (\v (Position p) -> Position $ p + v) -< v

run :: Query () () -> World -> World
run q w = let !(_, es) = runIdentity $ Q.map () q $ entities w in w {entities = es}

main :: IO ()
main = do
  let go wAcc = snd $ W.spawn (bundle (Position 0) <> bundle (Velocity 1)) wAcc
      !w = foldr (const go) W.empty [0 :: Int .. 10000]
  defaultMain
    [ bench "iter" $ nf (run query) w,
      bench "iter do-notation" $ nf (run queryDo) w
    ]
