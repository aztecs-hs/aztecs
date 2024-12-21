{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Aztecs
import qualified Data.Aztecs as ECS

data X = X Int deriving (Show)

instance Component X

data Y = Y Int deriving (Show)

instance Component Y

data Q = Q (Write X) Y deriving (Show)

main :: IO ()
main =
  let (e, w) = spawn (X 1) newWorld
      w' = insert e (Y 2) w
   in case query e (Q <$> ECS.write <*> ECS.read) w' of
        Just (Q x y) ->
          let w'' = updateQuery (queryAll (ECS.write @X) w') (\(X x') -> X $ x' + 1) w'
           in case query e (ECS.read @X) w'' of
                Just (X x') -> print (x', y)
                Nothing -> return ()
        Nothing -> return ()
