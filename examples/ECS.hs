{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs as W
import Data.Aztecs.Archetype
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

main :: IO ()
main = do
  let (e, w) = W.spawn (Position 0) W.empty
  pPrint w
