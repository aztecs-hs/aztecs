{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Dynamic (DynamicSystem (..), raceDyn) where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Control.Parallel (par)
import Aztecs.ECS.Access (Access)
import Aztecs.ECS.System.Dynamic.Class (ArrowDynamicSystem (..))
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (..))
import Aztecs.ECS.View (View)
import Aztecs.ECS.World (World (..))

newtype DynamicSystem i o = DynamicSystem
  { -- | Run a dynamic system,
    -- producing some output, an updated `View` into the `World`, and any queued `Access`.
    runSystemDyn :: World -> (i -> (o, View, Access ()))
  }
  deriving (Functor)

instance Category DynamicSystem where
  id = DynamicSystem $ \_ i -> (i, mempty, pure ())
  DynamicSystem f . DynamicSystem g = DynamicSystem $ \w i ->
    let (b, gView, gAccess) = g w i
        (a, fView, fAccess) = f w b
     in (a, gView <> fView, gAccess >> fAccess)

instance Arrow DynamicSystem where
  arr f = DynamicSystem $ \_ i -> (f i, mempty, pure ())
  first (DynamicSystem f) = DynamicSystem $ \w (i, x) -> let (a, v, access) = f w i in ((a, x), v, access)

instance ArrowDynamicReaderSystem DynamicSystem where
  runArrowReaderSystemDyn f = DynamicSystem $ \w i -> let o = f w i in (o, mempty, pure ())

instance ArrowDynamicSystem DynamicSystem where
  runArrowSystemDyn = DynamicSystem

raceDyn :: DynamicSystem i a -> DynamicSystem i b -> DynamicSystem i (a, b)
raceDyn (DynamicSystem f) (DynamicSystem g) = DynamicSystem $ \w -> \i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, v, fAccess) = fa
      (b, v', gAccess) = gbPar
   in ((a, b), v <> v', fAccess >> gAccess)
