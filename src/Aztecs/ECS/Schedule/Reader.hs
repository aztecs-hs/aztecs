{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.Schedule.Reader
  ( ReaderScheduleT (..),
  )
where

import Aztecs.ECS.Access (AccessT (..), runAccessT)
import Aztecs.ECS.Schedule.Dynamic.Reader (DynamicReaderScheduleT (..))
import Aztecs.ECS.Schedule.Reader.Class
import Aztecs.ECS.System.Dynamic.Reader (DynamicReaderSystem (..))
import Aztecs.ECS.System.Reader (ReaderSystem (..))
import Aztecs.ECS.World (World (..))
import Aztecs.ECS.World.Components (Components)
import Control.Arrow
import Control.Category
import Control.Monad.Fix
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (..))
import Prelude hiding (id, (.))

type ReaderSchedule m = ReaderScheduleT (AccessT m)

newtype ReaderScheduleT m i o
  = ReaderSchedule {runReaderSchedule :: Components -> (DynamicReaderScheduleT m i o, Components)}
  deriving (Functor)

instance (Monad m) => Category (ReaderScheduleT m) where
  id = ReaderSchedule $ \cs -> (id, cs)
  ReaderSchedule f . ReaderSchedule g = ReaderSchedule $ \cs ->
    let (g', cs') = g cs
        (f', cs'') = f cs'
     in (f' . g', cs'')

instance (Monad m) => Arrow (ReaderScheduleT m) where
  arr f = ReaderSchedule $ \cs -> (arr f, cs)
  first (ReaderSchedule f) = ReaderSchedule $ \cs -> let (f', cs') = f cs in (first f', cs')

instance (Monad m) => ArrowChoice (ReaderScheduleT m) where
  left (ReaderSchedule f) = ReaderSchedule $ \cs -> let (f', cs') = f cs in (left f', cs')

instance (MonadFix m) => ArrowLoop (ReaderScheduleT m) where
  loop (ReaderSchedule f) = ReaderSchedule $ \cs -> let (f', cs') = f cs in (loop f', cs')

instance (Monad m) => ArrowReaderSchedule ReaderSystem (ReaderSchedule m) where
  reader s = ReaderSchedule $ \cs ->
    let (dynS, _, cs') = runReaderSystem s cs
        go dynSAcc i = AccessT $ do
          w <- get
          let (o, a, dynSAcc') = runReaderSystemDyn dynSAcc (entities w) i
              ((), w') = runIdentity $ runAccessT a w
          put w'
          return (o, DynamicReaderSchedule $ go dynSAcc')
     in (DynamicReaderSchedule $ go dynS, cs')
