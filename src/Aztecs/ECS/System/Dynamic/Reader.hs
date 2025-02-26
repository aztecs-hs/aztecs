{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Dynamic.Reader
  ( DynamicReaderSystem,
    DynamicReaderSystemT (..),
    ArrowDynamicReaderSystem (..),
    ArrowQueueSystem (..),
    raceDyn,
  )
where

import Aztecs.ECS.Access
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryReader (..))
import Aztecs.ECS.System.Dynamic.Reader.Class
import Aztecs.ECS.System.Queue (ArrowQueueSystem (..))
import qualified Aztecs.ECS.View as V
import qualified Aztecs.ECS.World.Archetype as A
import Aztecs.ECS.World.Bundle
import Aztecs.ECS.World.Entities (Entities (..))
import Control.Arrow
import Control.Category
import Control.Monad.Identity
import Control.Parallel (par)
import qualified Data.Map as Map
import Prelude hiding (id, (.))

type DynamicReaderSystem = DynamicReaderSystemT Identity

newtype DynamicReaderSystemT m i o = DynamicReaderSystem
  { -- | Run a dynamic system producing some output
    runReaderSystemDyn :: Entities -> i -> (o, AccessT m (), DynamicReaderSystemT m i o)
  }
  deriving (Functor)

instance (Monad m) => Category (DynamicReaderSystemT m) where
  id = DynamicReaderSystem $ \_ i -> (i, pure (), id)
  DynamicReaderSystem f . DynamicReaderSystem g = DynamicReaderSystem $ \w i ->
    let (b, gAccess, g') = g w i
        (c, fAccess, f') = f w b
     in (c, gAccess >> fAccess, f' . g')

instance (Monad m) => Arrow (DynamicReaderSystemT m) where
  arr f = DynamicReaderSystem $ \_ i -> (f i, pure (), arr f)
  first (DynamicReaderSystem f) = DynamicReaderSystem $ \w (i, x) ->
    let (a, access, f') = f w i in ((a, x), access, first f')

instance (Monad m) => ArrowChoice (DynamicReaderSystemT m) where
  left (DynamicReaderSystem f) = DynamicReaderSystem $ \w i -> case i of
    Left b -> let (c, access, f') = f w b in (Left c, access, left f')
    Right d -> (Right d, pure (), left (DynamicReaderSystem f))

instance (Monad m) => ArrowLoop (DynamicReaderSystemT m) where
  loop (DynamicReaderSystem f) = DynamicReaderSystem $ \w b ->
    let ((c, d), access, f') = f w (b, d) in (c, access, loop f')

instance (Monad m) => ArrowDynamicReaderSystem DynamicQueryReader (DynamicReaderSystemT m) where
  allDyn cIds q = DynamicReaderSystem $ \w i ->
    let !v = V.view cIds $ archetypes w
     in if V.null v
          then (runDynQueryReader q (repeat i) (Map.keys $ entities w) A.empty, pure (), allDyn cIds q)
          else (V.readAllDyn i q v, pure (), allDyn cIds q)
  filterDyn cIds q f = DynamicReaderSystem $ \w i ->
    let !v = V.filterView cIds f $ archetypes w
     in (V.readAllDyn i q v, pure (), filterDyn cIds q f)

instance (Monad m) => ArrowQueueSystem Bundle (AccessT m) (DynamicReaderSystemT m) where
  queue f = DynamicReaderSystem $ \_ i -> let !a = f i in ((), a, queue f)

raceDyn :: (Monad m) => DynamicReaderSystemT m i a -> DynamicReaderSystemT m i b -> DynamicReaderSystemT m i (a, b)
raceDyn (DynamicReaderSystem f) (DynamicReaderSystem g) = DynamicReaderSystem $ \w i ->
  let fa = f w i
      gb = g w i
      gbPar = fa `par` gb
      (a, fAccess, f') = fa
      (b, gAccess, g') = gbPar
   in ((a, b), fAccess >> gAccess, raceDyn f' g')
