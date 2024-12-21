{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Query as Q

data X = X Int deriving (Show)

instance Component X

data Y = Y Int deriving (Show)

instance Component Y

data XY = XY (Write X) Y deriving (Show)

data S = S (Query XY)

instance (MonadIO m) => System m S where
  access = S <$> query (XY <$> Q.write <*> Q.read)
  run (S q) = do
    e <- spawn (X 0)
    insert e (Y 1)

    q' <- queryAll q
    liftIO $ print q'

    adjustQuery (fmap (\(XY x _) -> x) q') (\(X x) -> X $ x + 1)

    q'' <- queryAll q
    liftIO $ print q''

    return ()

main :: IO ()
main = do
  _ <- runSystem @_ @S newWorld
  return ()
