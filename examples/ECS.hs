{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import Data.Aztecs.Entity
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

main :: IO ()
main = do
  let (e, w) = W.spawn (Position 0) W.empty
      w' = W.insert e (Velocity 1) w
      (q, _) = Q.mapWith Q.fetch Q.fetch (\(Velocity v :& Position x) -> Position (x + v)) w'
  pPrint q
