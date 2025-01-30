{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import qualified Data.Aztecs.System as S
import qualified Data.Aztecs.World as W

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

data Setup

instance System IO Setup where
  task = S.queue (A.spawn_ (Position 0 :& Velocity 1))

data Movement

instance System IO Movement where
  task = S.map (\(Position x :& Velocity v) -> Position (x + v)) >>> S.run print

main :: IO ()
main = do
  w <- S.runSystem @_ @Setup W.empty
  _ <- S.runSystem @_ @Movement w
  return ()
