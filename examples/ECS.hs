{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs as W
import Text.Pretty.Simple

newtype X = X Int deriving (Show)

main :: IO ()
main = do
  let (e, w) = W.spawn (0 :: Int) W.empty
      w' = W.insert e (X 1) w
      x = W.lookup @X e w'
  pPrint w'
  pPrint x
