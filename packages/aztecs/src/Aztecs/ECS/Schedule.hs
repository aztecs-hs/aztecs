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
import Aztecs.ECS.World.Components (Components)
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad ((>=>))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor (void)

type Schedule m = ScheduleT (AccessT m)

newtype ScheduleT m i o = Schedule {runSchedule' :: Components -> (i -> m o, Components)}
  deriving (Functor)

instance (Monad m) => Category (Schedule m) where
  id = Schedule $ \cs -> (return, cs)
  Schedule f . Schedule g = Schedule $ \cs ->
    let (g', cs') = g cs
        (f', cs'') = f cs'
     in (g' >=> f', cs'')

instance (Monad m) => Arrow (Schedule m) where
  arr f = Schedule $ \cs -> (return Prelude.. f, cs)
  first (Schedule f) = Schedule $ \cs ->
    let (g, cs') = f cs in (\(b, d) -> (,) <$> g b <*> return d, cs')

instance (Monad m) => ArrowAccessSchedule (AccessT m) (Schedule m) where
  access f = Schedule $ \cs -> (f, cs)

instance (Monad m) => ArrowReaderSchedule ReaderSystem (Schedule m) where
  reader t = Schedule $ \cs ->
    let (dynT, _, cs') = runReaderSystem t cs
        go i = AccessT $ do
          w <- get
          let (o, a) = runReaderSystemDyn dynT w i
              ((), w') = runIdentity $ runAccessT a w
          put w'
          return o
     in (go, cs')

instance (Monad m) => ArrowSchedule System (Schedule m) where
  system t = Schedule $ \cs ->
    let (dynT, _, cs') = runSystem t cs
        go i = AccessT $ do
          w <- get
          let (o, v, a) = runSystemDyn dynT w i
              ((), w') = runIdentity $ runAccessT a $ V.unview v w
          put w'
          return o
     in (go, cs')

runSchedule :: (Monad m) => Schedule m i o -> World -> i -> m (o, World)
runSchedule s w i = do
  let (f, cs) = runSchedule' s (components w)
  (o, w') <- runAccessT (f i) w {components = cs}
  return (o, w')

runSchedule_ :: (Monad m) => Schedule m () () -> m ()
runSchedule_ s = void (runSchedule s W.empty ())

forever :: Schedule IO i o -> (o -> IO ()) -> Schedule IO i ()
forever s f = Schedule $ \cs ->
  let (g, cs') = runSchedule' s cs
      go i = AccessT $ do
        w <- get
        let loop wAcc = do
              (o, wAcc') <- lift $ runAccessT (g i) wAcc
              lift $ evaluate $ rnf wAcc'
              lift $ f o
              loop wAcc'
        loop w
   in (go, cs')

forever_ :: Schedule IO i o -> Schedule IO i ()
forever_ s = forever s (const $ pure ())
