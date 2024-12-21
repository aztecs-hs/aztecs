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

data XY = XY (Write X) Y deriving (Show)

data S = S (Query XY)

instance (MonadIO m) => System m S where
  access = S <$> query (XY <$> Q.write <*> Q.read)
  run (S q) = do
    e <- T.spawn (X 0)
    T.insert e (Y 1)

    xys <- T.all q
    liftIO $ print xys

    T.alter (fmap (\(XY x _) -> x) xys) (\(X x) -> X $ x + 1)

    xys' <- T.all q
    liftIO $ print xys'

    return ()

main :: IO ()
main = do
  _ <- runSystem @_ @S newWorld
  return ()
