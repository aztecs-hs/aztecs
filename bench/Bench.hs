{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Aztecs.ECS
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Criterion.Main
import GHC.Generics (Generic)

newtype Position = Position Int deriving (Generic, NFData, Show)

newtype Velocity = Velocity Int deriving (Generic, NFData, Show)

data MoveSystem = MoveSystem

instance (PrimMonad m, MonadIO m) => System m MoveSystem where
  type SystemInputs m MoveSystem = Query (W m Position, R Velocity)
  runSystem _ = mapM_ go
    where
      go (posRef, R (Velocity v)) =
        modifyW posRef $ \(Position p) -> Position (p + v)

setup :: IO (W.World IO '[Position, Velocity])
setup = do
  w <- W.empty @_ @'[Position, Velocity]
  foldM setupEntity w [0 :: Int .. 10000]
  where
    setupEntity w _ = do
      (_, w') <- W.spawn (bundle (Position 0) <> bundle (Velocity 1)) w
      return w'

main :: IO ()
main = do
  !w <- setup
  defaultMain [bench "iter" . whnfIO $ runAztecsT_ (system MoveSystem) w]