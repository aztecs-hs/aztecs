{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System
  ( -- * Systems
    System (..),
    ArrowReaderSystem (..),
    ArrowSystem (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Aztecs.ECS.Query (ReadsWrites (..))
import qualified Aztecs.ECS.Query as Q
import Aztecs.ECS.System.Class (ArrowSystem (..), filterMap, map, mapSingle, map_, queue)
import Aztecs.ECS.System.Dynamic (DynamicSystem (..), raceDyn)
import Aztecs.ECS.System.Dynamic.Reader.Class (ArrowDynamicReaderSystem (runArrowReaderSystemDyn))
import Aztecs.ECS.System.Reader.Class (ArrowReaderSystem (..), all, filter, single)
import Aztecs.ECS.World.Components (Components)
import Prelude hiding (all, filter, map, (.))
import qualified Prelude hiding (filter, map)

-- | System to process entities.
newtype System i o = System
  { -- | Run a system, producing a `DynamicSystem` that can be repeatedly run.
    runSystem :: Components -> (DynamicSystem i o, ReadsWrites, Components)
  }
  deriving (Functor)

instance Category System where
  id = System $ \cs -> (DynamicSystem $ \_ i -> (i, mempty, pure ()), mempty, cs)
  System f . System g = System $ \cs ->
    let (f', rwsF, cs') = f cs
        (g', rwsG, cs'') = g cs'
     in (f' . g', rwsF <> rwsG, cs'')

instance Arrow System where
  arr f = System $ \cs -> (DynamicSystem $ \_ i -> (f i, mempty, pure ()), mempty, cs)
  first (System f) = System $ \cs ->
    let (f', rwsF, cs') = f cs in (first f', rwsF, cs')
  f &&& g = System $ \cs ->
    let (dynF, rwsA, cs') = runSystem f cs
        (dynG, rwsB, cs'') = runSystem g cs'
     in ( if Q.disjoint rwsA rwsB then dynF &&& dynG else raceDyn dynF dynG,
          rwsA <> rwsB,
          cs''
        )

instance ArrowReaderSystem System where
  runArrowReaderSystem f = System $ \cs ->
    let (g, rs, cs') = f cs in (runArrowReaderSystemDyn g, ReadsWrites rs mempty, cs')

instance ArrowSystem System where
  runArrowSystem f = System $ \cs -> let (g, rs, cs') = f cs in (DynamicSystem g, rs, cs')
