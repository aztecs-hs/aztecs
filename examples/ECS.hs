{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Aztecs
import qualified Aztecs.World as W
import Control.Monad.IO.Class

newtype Position = Position Int
  deriving (Show, Eq)

instance Component m Position where
  type ComponentStorage m Position = SparseStorage m

newtype Velocity = Velocity Int
  deriving (Show, Eq)

instance Component m Velocity where
  type ComponentStorage m Velocity = SparseStorage m

data MoveSystem = MoveSystem

instance (PrimMonad m, MonadIO m) => System m MoveSystem where
  type SystemIn m MoveSystem = Query (W m Position, R Velocity)

  runSystem _ = mapM_ go
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
