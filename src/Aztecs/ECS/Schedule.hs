{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}

module Aztecs.ECS.Schedule
  ( -- * Schedules
    Schedule,
    ScheduleT (..),
    ArrowReaderSchedule (..),
    ArrowSchedule (..),
    ArrowAccessSchedule (..),
    delay,
    forever,
    forever_,
    runSchedule,
    runSchedule_,
  )
where

import Aztecs.ECS.Access (AccessT (..), runAccessT)
import Aztecs.ECS.Schedule.Access.Class (ArrowAccessSchedule (..))
import Aztecs.ECS.Schedule.Class (ArrowSchedule (..))
import Aztecs.ECS.Schedule.Reader.Class (ArrowReaderSchedule (..))
import Aztecs.ECS.System (System (..))
import Aztecs.ECS.System.Dynamic (DynamicSystem (..))
import Aztecs.ECS.System.Dynamic.Reader (DynamicReaderSystem (..))
import Aztecs.ECS.System.Reader (ReaderSystem (..))
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import qualified Aztecs.ECS.World as W
import Aztecs.ECS.World.Bundle (Bundle)
import Aztecs.ECS.World.Components (Components)
import Control.Arrow (Arrow (..), ArrowLoop (..))
import Control.Category (Category (..))
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad.Fix
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor (void)
import Prelude hiding (id, (.))

type Schedule m = ScheduleT (AccessT m)

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

instance (MonadFix m) => ArrowLoop (DynamicScheduleT m) where
  loop (DynamicSchedule f) = DynamicSchedule $ \b -> do
    rec ((c, d), f') <- f (b, d)
    return (c, loop f')

accessDyn :: (Monad m) => (i -> m o) -> DynamicScheduleT m i o
accessDyn f = DynamicSchedule $ \i -> do
  a <- f i
  return (a, accessDyn f)

delayDyn :: (Applicative m) => a -> DynamicScheduleT m a a
delayDyn d = DynamicSchedule $ \this -> pure (d, delayDyn this)

-- | System schedule.
newtype ScheduleT m i o = Schedule {runSchedule' :: Components -> (DynamicScheduleT m i o, Components)}
  deriving (Functor)

instance (Monad m) => Category (ScheduleT m) where
  id = Schedule $ \cs -> (id, cs)
  Schedule f . Schedule g = Schedule $ \cs ->
    let (g', cs') = g cs
        (f', cs'') = f cs'
     in (f' . g', cs'')

instance (Monad m) => Arrow (ScheduleT m) where
  arr f = Schedule $ \cs -> (arr f, cs)
  first (Schedule f) = Schedule $ \cs -> let (f', cs') = f cs in (first f', cs')

instance (MonadFix m) => ArrowLoop (ScheduleT m) where
  loop (Schedule f) = Schedule $ \cs ->
    let (f', cs') = f cs
     in (loop f', cs')

instance (Monad m) => ArrowAccessSchedule Bundle (AccessT m) (Schedule m) where
  access f = Schedule $ \cs -> (accessDyn f, cs)

instance (Monad m) => ArrowReaderSchedule ReaderSystem (Schedule m) where
  reader t = Schedule $ \cs ->
    let (dynT, _, cs') = runReaderSystem t cs
        go i = AccessT $ do
          w <- get
          let (o, a) = runReaderSystemDyn dynT w i
              ((), w') = runIdentity $ runAccessT a w
          put w'
          return (o, DynamicSchedule go)
     in (DynamicSchedule go, cs')

instance (Monad m) => ArrowSchedule System (Schedule m) where
  system t = Schedule $ \cs ->
    let (dynT, _, cs') = runSystem t cs
        go i = AccessT $ do
          w <- get
          let (o, v, a) = runSystemDyn dynT w i
              ((), w') = runIdentity $ runAccessT a $ V.unview v w
          put w'
          return (o, DynamicSchedule go)
     in (DynamicSchedule go, cs')

delay :: (Monad m) => a -> Schedule m a a
delay d = Schedule $ \cs -> (delayDyn d, cs)

runSchedule :: (Monad m) => Schedule m i o -> World -> i -> m (o, DynamicSchedule m i o, World)
runSchedule s w i = do
  let (f, cs) = runSchedule' s (components w)
  ((o, f'), w') <- runAccessT (runScheduleDyn f i) w {components = cs}
  return (o, f', w')

runSchedule_ :: (Monad m) => Schedule m () () -> m ()
runSchedule_ s = void (runSchedule s W.empty ())

forever :: Schedule IO i o -> (o -> IO ()) -> Schedule IO i ()
forever s f = Schedule $ \cs ->
  let (g, cs') = runSchedule' s cs
      go i = AccessT $ do
        w <- get
        let go' gAcc wAcc = do
              ((o, g'), wAcc') <- lift $ runAccessT (runScheduleDyn gAcc i) wAcc
              lift $ evaluate $ rnf wAcc'
              lift $ f o
              go' g' wAcc'
        go' g w
   in (DynamicSchedule go, cs')

forever_ :: Schedule IO i o -> Schedule IO i ()
forever_ s = forever s (const $ pure ())
