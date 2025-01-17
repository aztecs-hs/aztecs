{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import Text.Pretty.Simple

newtype Position = Position Int deriving (Show)

instance Component Position

newtype Velocity = Velocity Int deriving (Show)

instance Component Velocity

main :: IO ()
main = do
  let (e, w) = spawn (Position 0) empty
      w' = insert e (Velocity 1) w
  (x, _) <- runQuery (mapFetch (\(Position p) -> Position $ p + 1)) w'
  pPrint x
