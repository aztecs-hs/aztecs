{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.System (System (..))
import qualified Aztecs.ECS.World as W
import Control.Monad.IO.Class
import Data.Kind

newtype Position = Position Int
  deriving (Show, Eq)

newtype Velocity = Velocity Int
  deriving (Show, Eq)

data MoveSystem (m :: Type -> Type) = MoveSystem

instance
  ( ECS m,
    PrimMonad (Task m),
    MonadIO (Task m),
    t ~ (Task m)
  ) =>
  System t (MoveSystem m)
  where
  type SystemInputs (MoveSystem m) = Query (Task m) (W (Task m) Position, R Velocity)
  runSystem MoveSystem q = do
    results <- Q.runQuery q
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
      runSystemWithWorld @(AztecsT '[Position, Velocity] IO) (MoveSystem @(AztecsT '[Position, Velocity] IO))
