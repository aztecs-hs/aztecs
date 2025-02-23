{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Aztecs.ECS
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Control.Monad (void)
import Criterion.Main
import GHC.Generics (Generic)

newtype Position = Position Int deriving (Show, Generic, NFData)

instance Component Position

newtype Velocity = Velocity Int deriving (Show, Generic, NFData)

instance Component Velocity

run :: World -> IO World
run w = do
  let s =
        void
          ( S.map
              ( proc () -> do
                  Velocity v <- Q.fetch -< ()
                  Position p <- Q.fetch -< ()
                  Q.set -< Position $ p + v
              )
          )
  !(_, _, w') <- runSchedule (system s) w ()
  return w'

main :: IO ()
main = do
  let !w =
        foldr
          ( \_ wAcc ->
              let (e, wAcc') = W.spawn (bundle $ Position 0) wAcc
               in W.insert e (Velocity 1) wAcc'
          )
          W.empty
          [0 :: Int .. 100000]
  defaultMain [bench "iter" . nfIO $ run w]
