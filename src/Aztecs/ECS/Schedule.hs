{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.Schedule
  ( -- * Schedules
    Schedule,
    ScheduleT (..),
    ArrowReaderSchedule (..),
    ArrowSchedule (..),
    ArrowAccessSchedule (..),
    fromReaderSchedule,
    delay,
    forever,
    forever_,
    runSchedule,
    runSchedule_,
  )
where

import Aztecs.ECS.Access (AccessT (..), runAccessT)
import Aztecs.ECS.Schedule.Access.Class
import Aztecs.ECS.Schedule.Class
import Aztecs.ECS.Schedule.Dynamic
import Aztecs.ECS.Schedule.Reader (ReaderScheduleT (..))
import Aztecs.ECS.Schedule.Reader.Class
import Aztecs.ECS.System
import Aztecs.ECS.System.Dynamic
import Aztecs.ECS.System.Reader (ReaderSystemT (..))
import qualified Aztecs.ECS.View as V
import Aztecs.ECS.World (World (..))
import qualified Aztecs.ECS.World as W
import Aztecs.ECS.World.Bundle (Bundle)
import Aztecs.ECS.World.Components (Components)
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Control.DeepSeq
import Control.Exception
import Control.Monad.Fix
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor
import Prelude hiding (id, (.))

type Schedule m = ScheduleT (AccessT m)

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
  loop (Schedule f) = Schedule $ \cs -> let (f', cs') = f cs in (loop f', cs')

instance (Monad m) => ArrowAccessSchedule Bundle (AccessT m) (Schedule m) where
  access f = Schedule $ \cs -> (accessDyn f, cs)

instance (Monad m) => ArrowReaderSchedule (ReaderSystemT m) (Schedule m) where
  reader = fromReaderSchedule . reader

instance (Monad m) => ArrowSchedule (SystemT m) (Schedule m) where
  system s = Schedule $ \cs ->
    let (dynS, _, cs') = runSystem s cs
        go dynSAcc i = AccessT $ do
          w <- get
          let (o, v, a, dynSAcc') = runSystemDyn dynSAcc (W.entities w) i
          ((), w') <- lift $ runAccessT a w {W.entities = V.unview v (W.entities w)}
          put w'
          return (o, DynamicSchedule $ go dynSAcc')
     in (DynamicSchedule $ go dynS, cs')

fromReaderSchedule :: (Monad m) => ReaderScheduleT m i o -> ScheduleT m i o
fromReaderSchedule s = Schedule $ \cs ->
  let (dynS, cs') = runReaderSchedule s cs in (fromDynReaderSchedule dynS, cs')

delay :: (Monad m) => a -> Schedule m a a
delay d = Schedule $ \cs -> (delayDyn d, cs)

runSchedule :: (Monad m) => Schedule m i o -> World -> i -> m (o, DynamicSchedule m i o, World)
runSchedule s w i = do
  let (f, cs) = runSchedule' s (components $ W.entities w)
  ((o, f'), w') <- runAccessT (runScheduleDyn f i) w {W.entities = (W.entities w) {components = cs}}
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
