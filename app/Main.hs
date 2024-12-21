{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import qualified Data.Aztecs as ECS

data X = X Int deriving (Show)

instance Component X

data Y = Y Int deriving (Show)

instance Component Y

data Q = Q X Y deriving (Show)

main :: IO ()
main =
  let (e, w) = spawn (X 1) newWorld
      w' = insert e (Y 2) w
   in print $ runQuery (Q <$> ECS.read <*> ECS.read) w'
