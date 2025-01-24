{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.World (World)
import qualified Data.Aztecs.World as W

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

run :: World -> World
run w =
  let !(_, w') =
        Q.map
          (Q.fetch @Position Q.<&> Q.fetch @Velocity)
          (\(Position x, Velocity v) -> (Position $ x + 1, Velocity v))
          w
   in w'

main :: IO ()
main = do
  let w =
        foldr
          ( \_ wAcc ->
              let (e, wAcc') = W.spawn (Position 0) wAcc
               in W.insert e (Velocity 1) wAcc'
          )
          W.empty
          [0 :: Int .. 10000]
  defaultMain [bench "iter" $ whnf run w]
