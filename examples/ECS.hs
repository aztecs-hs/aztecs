{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS.World as W
import Control.Monad.IO.Class

newtype Position = Position Int
  deriving (Show, Eq)

newtype Velocity = Velocity Int
  deriving (Show, Eq)

data MoveSystem = MoveSystem

instance (PrimMonad m, MonadIO m) => System m MoveSystem where
  type SystemInputs m MoveSystem = Query m (W m Position, R Velocity)
  runSystem MoveSystem q = do
    results <- runQuery q
    mapM_ go results
    where
      go (posRef, R (Velocity v)) = do
        modifyW posRef $ \(Position p) -> Position (p + v)

        p <- readW posRef
        liftIO $ putStrLn $ "Moved to: " ++ show p

main :: IO ()
main = do
  world <- W.empty @_ @'[Position, Velocity]
  runAztecsT_ go world
  where
    go = do
      _ <- spawn (bundle (Position 0) <> bundle (Velocity 1))
      system MoveSystem
