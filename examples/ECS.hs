{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs as W
import qualified Data.Aztecs.Query as Q
import Text.Pretty.Simple

newtype X = X Int deriving (Show)

newtype Y = Y Int deriving (Show)

main :: IO ()
main = do
  let (e, w) = W.spawn (X 0) W.empty
      w' = W.insert e (Y 0) w
      (e', w'') = W.spawn (X 1) w'
      w''' = W.insert e' (Y 1) w''
      (x, w'''') = Q.all (Q.read @X) w'''
  pPrint (w'''', x)
