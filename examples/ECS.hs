{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
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
      w' = W.insert e (Velocity 0) w
      (q, _) =
        Q.map
          (Q.fetch @Position Q.<&> Q.fetch @Velocity)
          (\(Position x, Velocity v) -> (Position $ x + 1, Velocity v))
          w'
  pPrint q
