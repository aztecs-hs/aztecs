{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.Task as T

data X = X Int deriving (Show)

instance Component X

data Y = Y Int deriving (Show)

instance Component Y

data A = A (Query (Write X))

instance System IO A where
  access = A <$> query Q.write
  run (A q) = do
    liftIO $ print "A"

    e <- T.spawn (X 0)
    T.insert e (Y 1)

    xs <- T.all q
    liftIO $ print xs

    T.alter xs (\(X x) -> X $ x + 1)

data XY = XY X Y deriving (Show)

data B = B (Query XY)

instance System IO B where
  access = B <$> query (XY <$> Q.read <*> Q.read)
  run (B q) = do
    liftIO $ print "B"

    xys <- T.all q
    liftIO $ print xys

app :: Schedule IO
app = schedule @_ @A [] <> schedule @_ @B [after @A]

main :: IO ()
main = runSchedule app
