{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Aztecs.ECS
import qualified Aztecs.ECS as ECS
import qualified Aztecs.ECS.World as W
import Control.DeepSeq
import Control.Monad
import Criterion.Main
import GHC.Generics (Generic)

newtype Position = Position Int deriving (Generic, NFData, Show)

newtype Velocity = Velocity Int deriving (Generic, NFData, Show)

move :: Query IO (W IO Position, R Velocity) -> IO ()
move q = do
  results <- runQuery q
  mapM_ go results
  where
    go (posRef, ECS.R (Velocity v)) = do
      Position oldPos <- readW posRef
      writeW posRef (Position (oldPos + v))
    {-# INLINE go #-}
{-# INLINE move #-}

data MoveSystem = MoveSystem

instance System IO MoveSystem where
  type SystemInputs MoveSystem = Query IO (W IO Position, ECS.R Velocity)
  runSystem MoveSystem q = move q

setup :: IO (W.World IO '[Position, Velocity])
setup = do
  w <- W.empty @_ @'[Position, Velocity]
  foldM setupEntity w [0 :: Int .. 10000]
  where
    setupEntity w _ = do
      (e, w') <- W.spawn (Position 0) w
      W.insert e (Velocity 1) w'

main :: IO ()
main = do
  !w <- setup
  defaultMain [bench "iter" $ whnfIO (runSystemWithWorld MoveSystem w)]
