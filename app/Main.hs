{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Command as C
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.Task as T

-- Components

data X = X Int deriving (Show)

instance Component X

data Y = Y Int deriving (Show)

instance Component Y

data A = A (Query X)

-- Systems

instance System IO A where
  access = A <$> query Q.read
  run (A q) = do
    liftIO $ print "A"

    T.command $ do
      e <- C.spawn (X 0)
      C.insert e (Y 1)

    xs <- T.all q
    liftIO $ print xs

data XY = XY (Write X) Y deriving (Show)

data B = B (Query XY)

instance System IO B where
  access = B <$> query (XY <$> Q.write <*> Q.read)
  run (B q) = do
    liftIO $ print "B"

    -- Query all entities with an X and Y component
    xys <- T.all q
    liftIO $ print xys

    -- Increment all X components
    T.alter (fmap (\(XY x _) -> x) xys) (\(X x) -> X $ x + 1)

app :: Scheduler IO
app = schedule @Startup @_ @A [] <> schedule @Update @_ @B []

main :: IO ()
main = runScheduler app
