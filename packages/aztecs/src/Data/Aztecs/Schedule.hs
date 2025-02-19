{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aztecs.Schedule
  ( -- * Schedules
    Schedule (..),
    reader,
    system,
    forever,
    forever_,
    access,
    task,
    runSchedule,
    runSchedule_,
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Monad ((>=>))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Aztecs.Access (AccessT (..), runAccessT)
import Data.Aztecs.System (System (..))
import Data.Aztecs.System.Dynamic (DynamicSystemT (..))
import Data.Aztecs.System.Dynamic.Reader (DynamicReaderSystem (..))
import Data.Aztecs.System.Reader (ReaderSystem (..))
import qualified Data.Aztecs.View as V
import Data.Aztecs.World (World (..))
import qualified Data.Aztecs.World as W
import Data.Aztecs.World.Components (Components)
import Data.Functor (void)

newtype Schedule m i o = Schedule {runSchedule' :: Components -> (i -> AccessT m o, Components)}
  deriving (Functor)

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
  (o, w') <- runAccessT (f i) w {components = cs}
  return (o, w')

runSchedule_ :: (Monad m) => Schedule m () () -> m ()
runSchedule_ s = void (runSchedule s (W.empty) ())

reader :: (Monad m) => ReaderSystem i o -> Schedule m i o
reader t = Schedule $ \cs ->
  let (dynT, _, cs') = runReaderSystem t cs
      go i = AccessT $ do
        w <- get
        return $ runReaderSystemDyn dynT w i
   in (go, cs')

system :: (Monad m) => System i o -> Schedule m i o
system t = Schedule $ \cs ->
  let (dynT, _, cs') = runSystem t cs
      go i = AccessT $ do
        w <- get
        let f = runSystemTDyn dynT w
        let (o, v, a) = f i
        let ((), w') = runIdentity $ runAccessT a $ V.unview v w
        put w'
        return o
   in (go, cs')

access :: (Monad m) => (i -> AccessT m o) -> Schedule m i o
access f = Schedule $ \cs -> (f, cs)

task :: (Monad m) => (i -> m o) -> Schedule m i o
task f = Schedule $ \cs -> (AccessT Prelude.. lift Prelude.. f, cs)

forever :: (Monad m) => Schedule m i o -> (o -> m ()) -> Schedule m i ()
forever s f = Schedule $ \cs ->
  let (g, cs') = runSchedule' s cs
      go i = AccessT $ do
        w <- get
        let loop wAcc = do
              (o, wAcc') <- lift $ runAccessT (g i) wAcc
              lift $ f o
              loop wAcc'
        loop w
   in (go, cs')

forever_ :: (Monad m) => Schedule m i o -> Schedule m i ()
forever_ s = forever s (const $ pure ())
