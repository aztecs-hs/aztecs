{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}

module Aztecs.ECS.Schedule.Dynamic
  ( DynamicSchedule,
    DynamicScheduleT (..),
    fromDynReaderSchedule,
  )
where

import Aztecs.ECS.Access
import Aztecs.ECS.Schedule.Dynamic.Reader (DynamicReaderScheduleT (..))
import Control.Arrow
import Control.Category
import Control.Monad.Fix
import Prelude hiding (id, (.))

type DynamicSchedule m = DynamicScheduleT (AccessT m)

newtype DynamicScheduleT m i o = DynamicSchedule {runScheduleDyn :: i -> m (o, DynamicScheduleT m i o)}
  deriving (Functor)

instance (Monad m) => Category (DynamicScheduleT m) where
  id = DynamicSchedule $ \i -> pure (i, id)
  DynamicSchedule f . DynamicSchedule g = DynamicSchedule $ \i -> do
    (b, g') <- g i
    (c, f') <- f b
    return (c, f' . g')

instance (Monad m) => Arrow (DynamicScheduleT m) where
  arr f = DynamicSchedule $ \i -> pure (f i, arr f)
  first (DynamicSchedule f) = DynamicSchedule $ \(b, d) -> do
    (c, f') <- f b
    return ((c, d), first f')

instance (Monad m) => ArrowChoice (DynamicScheduleT m) where
  left (DynamicSchedule f) = DynamicSchedule $ \i -> case i of
    Left b -> do
      (c, f') <- f b
      return (Left c, left f')
    Right d -> return (Right d, left (DynamicSchedule f))

instance (MonadFix m) => ArrowLoop (DynamicScheduleT m) where
  loop (DynamicSchedule f) = DynamicSchedule $ \b -> do
    rec ((c, d), f') <- f (b, d)
    return (c, loop f')

fromDynReaderSchedule :: (Monad m) => DynamicReaderScheduleT m i o -> DynamicScheduleT m i o
fromDynReaderSchedule (DynamicReaderSchedule f) = DynamicSchedule $ \i -> do
  (o, f') <- f i
  return (o, fromDynReaderSchedule f')
