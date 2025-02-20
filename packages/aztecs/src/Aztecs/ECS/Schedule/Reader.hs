{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.Schedule.Reader
  ( ReaderSchedule (..),
  )
where

import Aztecs.ECS.Access (AccessT (..), runAccessT)
import Aztecs.ECS.Schedule.Reader.Class
import Aztecs.ECS.System.Dynamic.Reader (DynamicReaderSystem (..))
import Aztecs.ECS.System.Reader (ReaderSystem (..))
import Aztecs.ECS.World.Components (Components)
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad ((>=>))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (..))

newtype ReaderSchedule m i o
  = ReaderSchedule {runReaderSchedule :: Components -> (i -> AccessT m o, Components)}
  deriving (Functor)

instance (Monad m) => Category (ReaderSchedule m) where
  id = ReaderSchedule $ \cs -> (return, cs)
  ReaderSchedule f . ReaderSchedule g = ReaderSchedule $ \cs ->
    let (g', cs') = g cs
        (f', cs'') = f cs'
     in (g' >=> f', cs'')

instance (Monad m) => Arrow (ReaderSchedule m) where
  arr f = ReaderSchedule $ \cs -> (return Prelude.. f, cs)
  first (ReaderSchedule f) = ReaderSchedule $ \cs ->
    let (g, cs') = f cs in (\(b, d) -> (,) <$> g b <*> return d, cs')

instance (Monad m) => ArrowReaderSchedule ReaderSystem (ReaderSchedule m) where
  reader t = ReaderSchedule $ \cs ->
    let (dynT, _, cs') = runReaderSystem t cs
        go i = AccessT $ do
          w <- get
          let (o, a) = runReaderSystemDyn dynT w i
              ((), w') = runIdentity $ runAccessT a w
          put w'
          return o
     in (go, cs')
