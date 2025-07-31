{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Aztecs.ECS
import Aztecs.ECS.Entities
import Aztecs.ECS.Query
import qualified Aztecs.ECS.World as W

-- fail:
-- q :: (PrimMonad m) => World (PrimState m) '[Int, Bool] -> Query m (Q '[W (PrimState m) Int, W (PrimState m) Int])
-- q = query

main :: IO ()
main = do
  w <- W.empty @_ @'[Int, Bool]
  (e, w') <- W.spawn (42 :: Int) w
  print e
  let q = W.query @_ @_ @_ @(Q '[Entity, R Int]) w'
  x <- runQuery q
  print x
