{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Aztecs.ECS
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.ECS.World (World (..))
import qualified Aztecs.ECS.World as W
import qualified Aztecs.ECS.World.Entities as E
import Control.DeepSeq
import Criterion.Main
import GHC.Generics (Generic)

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

query :: Query () Position
query = proc () -> do
  Velocity v <- Q.fetch -< ()
  Position p <- Q.fetch -< ()
  Q.set -< Position $ p + v

run :: QueryState () Position -> World -> World
run q w = let !(_, es) = Q.queryStateMap () q $ entities w in w {entities = es}

runSystem :: World -> IO World
runSystem w = do
  (_, _, w') <- runSchedule (system $ S.map query) w ()
  return w'

main :: IO ()
main = do
  let go wAcc = snd $ W.spawn (bundle (Position 0) <> bundle (Velocity 1)) wAcc
      !w = foldr (const go) W.empty [0 :: Int .. 10000]
      !(q, _) = Q.runQuery query (E.components $ W.entities w)
  defaultMain [bench "iter" $ nf (run q) w, bench "iter system" . nfIO $ runSystem w]
