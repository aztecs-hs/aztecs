{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Aztecs
import qualified Aztecs.World as W
import Control.Monad.IO.Class

newtype Position = Position Int
  deriving (Show, Eq)

instance (MonadIO m, Show (Entity m)) => Component m Position where
  type ComponentStorage m Position = SparseStorage m

  componentHooks _ =
    Hooks
      { onInsert = \entity ->
          liftIO . putStrLn $ "Position component inserted for " ++ show entity,
        onRemove = \entity ->
          liftIO . putStrLn $ "Position component removed for " ++ show entity
      }

newtype Velocity = Velocity Int
  deriving (Show, Eq)

instance (MonadIO m, Show (Entity m)) => Component m Velocity where
  type ComponentStorage m Velocity = SparseStorage m

  componentHooks _ =
    Hooks
      { onInsert = \entity ->
          liftIO $ putStrLn $ "Velocity component inserted for " ++ show entity,
        onRemove = \entity ->
          liftIO $ putStrLn $ "Velocity component removed for " ++ show entity
      }

data MoveSystem = MoveSystem

instance (PrimMonad m, MonadIO m) => System m MoveSystem where
  type SystemIn m MoveSystem = Query m (W m Position, R Velocity)

  runSystem _ q = mapQueryM_ go q
    where
      go (posRef, R (Velocity v)) = do
        modifyW posRef $ \(Position p) -> Position (p + v)
        p <- readW posRef
        liftIO $ putStrLn $ "Moved to: " ++ show p

main :: IO ()
main = do
  world <- W.empty @_ @'[Position, Velocity]
  (entity1, world') <- runAztecsT go world
  runAztecsT_ (remove entity1) world'
  where
    go = do
      e <- spawn (bundle (Position 0) <> bundle (Velocity 5))
      system MoveSystem
      return e
