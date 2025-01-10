{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs as W
import qualified Data.Aztecs.Query as Q
import Text.Pretty.Simple

newtype X = X Int deriving (Show)

newtype Y = Y Int deriving (Show)

main :: IO ()
main = do
  let (_, w) = W.spawn (X 0) W.empty
      (_, w') = W.spawn (X 1) w
      (x, w'') = Q.all (Q.read @X) w'
  pPrint (w'', x)
