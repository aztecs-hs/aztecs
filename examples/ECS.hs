{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs as W
import qualified Data.Aztecs.Query as Q
import Text.Pretty.Simple

newtype X = X Int deriving (Show)

main :: IO ()
main = do
  let (e, w) = W.spawn (X 0) W.empty
      (x, w') = Q.lookup e (Q.read @X) w
  pPrint x
