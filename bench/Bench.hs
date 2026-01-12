{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Aztecs
import qualified Aztecs.World as W
import Control.DeepSeq
import Control.Monad.IO.Class
import Criterion.Main
import GHC.Generics (Generic)

newtype Position = Position Int deriving (Generic, NFData, Show)

instance (Monad m) => Component m Position where
  type ComponentStorage m Position = SparseStorage m

newtype Velocity = Velocity Int deriving (Generic, NFData, Show)

instance (Monad m) => Component m Velocity where
  type ComponentStorage m Velocity = SparseStorage m

data MoveSystem = MoveSystem

instance (PrimMonad m, MonadIO m) => System m MoveSystem where
  type SystemIn m s MoveSystem = (W s m Position, R s Velocity)
  runSystem _ q = mapQueryM_ go q
    where
      go (posRef, R (Velocity v)) =
        modifyW posRef $ \(Position p) -> Position (p + v)

setup :: IO (W.World IO '[Position, Velocity])
setup = do
  w <- W.empty @_ @'[Position, Velocity]
  snd <$> runAztecsT (mapM setupEntity [0 :: Int .. 10000]) w
  where
    setupEntity _ = spawn (bundle (Position 0) <> bundle (Velocity 1))

main :: IO ()
main = do
  !w <- setup
  defaultMain [bench "iter" . whnfIO $ runAztecsT_ (system MoveSystem) w]
