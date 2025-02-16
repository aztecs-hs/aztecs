{-# LANGUAGE FlexibleInstances #-}

module Data.Aztecs.Schedule
  ( -- * Schedules
    Schedule (..),
    schedule,
    forever,
    runSchedule,
    runSchedule_,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad ((>=>))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Aztecs.Access (Access (..), runAccess)
import Data.Aztecs.System (DynamicSystemT (..), SystemT (..))
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Components (Components)

newtype Schedule m i o = Schedule {runSchedule' :: Components -> (i -> Access m o, Components)}

instance (Monad m) => Category (Schedule m) where
  id = Schedule $ \cs -> (return, cs)
  Schedule f . Schedule g = Schedule $ \cs ->
    let (g', cs') = g cs
        (f', cs'') = f cs'
     in (g' >=> f', cs'')

instance Arrow (Schedule IO) where
  arr f = Schedule $ \cs -> (return Prelude.. f, cs)
  first (Schedule f) = Schedule $ \cs ->
    let (g, cs') = f cs
     in (\(b, d) -> (,) <$> g b <*> return d, cs')

runSchedule :: (Monad m) => Schedule m i o -> World -> i -> m (o, World)
runSchedule s w i = do
  let (f, cs) = runSchedule' s (components w)
  (o, w') <- runAccess (f i) w {components = cs}
  return (o, w')

runSchedule_ :: (Monad m) => Schedule m () () -> m ()
runSchedule_ s = const () <$> runSchedule s (W.empty) ()

schedule :: (Monad m) => SystemT m i o -> Schedule m i o
schedule t = Schedule $ \cs ->
  let (dynT, _, cs') = runSystemT t cs
      go i = Access $ do
        w <- get
        let f = runSystemTDyn dynT w
        (o, v, access) <- lift $ f i
        ((), w') <- lift Prelude.. runAccess access $ V.unview v w
        put w'
        return o
   in (go, cs')

forever :: (Monad m) => Schedule m i () -> Schedule m i ()
forever s = Schedule $ \cs ->
  let (f, cs') = runSchedule' s cs
      go i = Access $ do
        w <- get
        let loop wAcc = do
              ((), wAcc') <- lift $ runAccess (f i) wAcc
              loop wAcc'
        loop w
   in (go, cs')
