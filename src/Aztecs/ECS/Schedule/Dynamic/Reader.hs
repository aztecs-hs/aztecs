{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Schedule.Dynamic.Reader
  ( DynamicReaderSchedule,
    DynamicReaderScheduleT (..),
  )
where

import Aztecs.ECS.Access
import Control.Arrow
import Control.Category
import Control.Monad.Fix
import Prelude hiding (all, any, id, lookup, reads, (.))

type DynamicReaderSchedule m = DynamicReaderScheduleT (AccessT m)

newtype DynamicReaderScheduleT m i o = DynamicReaderSchedule {runScheduleDyn :: i -> m (o, DynamicReaderScheduleT m i o)}
  deriving (Functor)

instance (Monad m) => Category (DynamicReaderScheduleT m) where
  id = DynamicReaderSchedule $ \i -> pure (i, id)
  DynamicReaderSchedule f . DynamicReaderSchedule g = DynamicReaderSchedule $ \i -> do
    (b, g') <- g i
    (c, f') <- f b
    return (c, f' . g')

instance (Monad m) => Arrow (DynamicReaderScheduleT m) where
  arr f = DynamicReaderSchedule $ \i -> pure (f i, arr f)
  first (DynamicReaderSchedule f) = DynamicReaderSchedule $ \(b, d) -> do
    (c, f') <- f b
    return ((c, d), first f')

instance (MonadFix m) => ArrowLoop (DynamicReaderScheduleT m) where
  loop (DynamicReaderSchedule f) = DynamicReaderSchedule $ \b -> do
    rec ((c, d), f') <- f (b, d)
    return (c, loop f')
