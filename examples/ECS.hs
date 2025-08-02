{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS.World as W

newtype Position = Position Int
  deriving (Show, Eq)

newtype Velocity = Velocity Int
  deriving (Show, Eq)

data MoveSystem = MoveSystem

instance System IO MoveSystem where
  type SystemInputs MoveSystem = Query IO (W IO Position, R Velocity)
  runSystem MoveSystem q = do
    results <- runQuery q
    mapM_ go results
    where
      go (posRef, R (Velocity v)) = do
        modifyW posRef $ \(Position p) -> Position (p + v)

        p <- readW posRef
        putStrLn $ "Moved to: " ++ show p

main :: IO ()
main = do
  w <- W.empty @_ @'[Position, Velocity]
  (_, w') <- W.spawn (bundle (Position 0) <> bundle (Velocity 1)) w
  runSystemWithWorld MoveSystem w'
