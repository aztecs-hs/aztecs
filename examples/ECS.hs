{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs.World as W
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

main :: IO ()
main = do
  let (_, w) = W.spawn (Position 0) W.empty
      q = Q.map (Q.fetch @Position) (\(Position x) -> Position $ x + 1) w
  pPrint q
