{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W

newtype Position = Position Int deriving (Show)

newtype Velocity = Velocity Int deriving (Show)

run :: W.World -> ([(Position, Velocity)], W.World)
run w =
  let !x = Q.all ((,) <$> Q.read @Position <*> Q.read @Velocity) w
   in x

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
