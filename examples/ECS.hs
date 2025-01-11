{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Text.Pretty.Simple

newtype X = X Int deriving (Show)

newtype Y = Y Int deriving (Show)

main :: IO ()
main = do
  let (e, w) = W.spawn (X 0) W.empty
      w' = W.insert e (Y 0) w
      (e', w'') = W.spawn (X 1) w'
      w''' = W.insert e' (Y 1) w''
      (q, _) =
        Q.all
          ( Q.writeWith
              Q.read
              ( \(X x) (Y y) ->
                  let y' = x + y
                   in (Y y', (x, y'))
              )
          )
          w'''
  pPrint (w''', q)
