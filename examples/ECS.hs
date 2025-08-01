{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS.World as W

newtype Position = Position Int

newtype Velocity = Velocity Int

move :: (PrimMonad m) => Query m (R Position, W m Velocity) -> m ()
move q = do
  results <- runQuery q
  mapM_ go results
  where
    go (R (Position x), vRef) = modifyW vRef (\(Velocity v) -> Velocity (v + x))

main :: IO ()
main = do
  w <- W.empty @_ @'[Position, Velocity]
  (e, w') <- W.spawn (Position 0) w
  w'' <- W.insert e (Velocity 1) w'
  move (W.query w'')
