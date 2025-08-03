{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Aztecs
import qualified Aztecs.World as W
import Control.Monad.IO.Class
import Control.Monad.Identity

newtype Position = Position Int
  deriving (Show, Eq)

newtype Velocity = Velocity Int
  deriving (Show, Eq)

newtype Health = Health Int
  deriving (Show, Eq)

newtype Damage = Damage Int
  deriving (Show, Eq)

data MoveSystem = MoveSystem
  deriving (Show)

data PhysicsSystem = PhysicsSystem
  deriving (Show)

data CombatSystem = CombatSystem
  deriving (Show)

data RenderSystem = RenderSystem
  deriving (Show)

instance (PrimMonad m, MonadIO m) => System m MoveSystem where
  type SystemIn m MoveSystem = Query (W m Position, R Velocity)
  runSystem MoveSystem q = do
    liftIO $ putStrLn "Running MoveSystem..."
    mapM_ go q
    where
      go (posRef, R (Velocity v)) = do
        modifyW posRef $ \(Position p) -> Position (p + v)
        p <- readW posRef
        liftIO $ putStrLn $ "  Moved to position: " ++ show p

instance (PrimMonad m, MonadIO m) => System m PhysicsSystem where
  type SystemIn m PhysicsSystem = Query (R Position, W m Velocity)
  runSystem PhysicsSystem q = do
    liftIO $ putStrLn "Running PhysicsSystem..."
    mapM_ go q
    where
      go (R (Position p), velRef) = do
        modifyW velRef $ \(Velocity v) -> Velocity (max 0 (v - 1))
        v <- readW velRef
        liftIO $ putStrLn $ "  Applied physics at position " ++ show p ++ ", new velocity: " ++ show v

instance (PrimMonad m, MonadIO m) => System m CombatSystem where
  type SystemIn m CombatSystem = Query (W m Health, R Damage)

  runSystem CombatSystem q = do
    liftIO $ putStrLn "Running CombatSystem..."
    mapM_ go q
    where
      go (healthRef, R (Damage d)) = do
        modifyW healthRef $ \(Health h) -> Health (max 0 (h - d))
        h <- readW healthRef
        liftIO $ putStrLn $ "  Applied damage, remaining health: " ++ show h

instance (PrimMonad m, MonadIO m) => System m RenderSystem where
  type SystemIn m RenderSystem = Query (R Position, R Health)

  runSystem RenderSystem q = do
    liftIO $ putStrLn "Running RenderSystem..."
    mapM_ go q
    where
      go (R (Position p), R (Health h)) = do
        liftIO $ putStrLn $ "  Rendering entity at position " ++ show p ++ " with health " ++ show h

app ::
  HSet
    '[ Run '[Before PhysicsSystem] MoveSystem,
       Run '[] PhysicsSystem,
       Run '[Before RenderSystem] CombatSystem,
       Run '[] RenderSystem
     ]
app =
  hcons (Run MoveSystem)
    . hcons (Run PhysicsSystem)
    . hcons (Run CombatSystem)
    . hcons (Run RenderSystem)
    $ hempty

appSmall ::
  HSetT
    Identity
    '[ Run '[After PhysicsSystem] MoveSystem,
       Run '[] PhysicsSystem,
       Run '[] RenderSystem
     ]
appSmall =
  hcons (Run MoveSystem)
    . hcons (Run PhysicsSystem)
    . hcons (Run RenderSystem)
    $ hempty

runSchedulerExample :: IO ()
runSchedulerExample = do
  world <- W.empty @_ @'[Position, Velocity, Health, Damage]
  runAztecsT_ go world
  where
    go :: AztecsT '[Position, Velocity, Health, Damage] IO ()
    go = do
      _ <- spawn (W.bundle (Position 0) <> W.bundle (Velocity 5) <> W.bundle (Health 100))
      _ <- spawn (W.bundle (Position 10) <> W.bundle (Velocity 3) <> W.bundle (Health 75) <> W.bundle (Damage 10))
      _ <- spawn (W.bundle (Position (-5)) <> W.bundle (Velocity 2) <> W.bundle (Health 50))
      runSchedule app
      return ()

runSchedulerExampleSmall :: IO ()
runSchedulerExampleSmall = do
  world <- W.empty @_ @'[Position, Velocity, Health, Damage]
  runAztecsT_ go world
  where
    go :: AztecsT '[Position, Velocity, Health, Damage] IO ()
    go = do
      _ <- spawn (W.bundle (Position 0) <> W.bundle (Velocity 5) <> W.bundle (Health 100))
      _ <- spawn (W.bundle (Position 10) <> W.bundle (Velocity 3) <> W.bundle (Health 75) <> W.bundle (Damage 10))
      _ <- spawn (W.bundle (Position (-5)) <> W.bundle (Velocity 2) <> W.bundle (Health 50))
      runSchedule appSmall
      return ()

main :: IO ()
main = do
  runSchedulerExample
  runSchedulerExampleSmall
