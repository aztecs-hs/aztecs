{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Aztecs.Transform
  ( -- * Transform
    Transform (..),

    -- ** 2D
    Transform2D,
    transform2d,

    -- * Size
    Size (..),

    -- ** 2D
    Size2D,
    size2D,

    -- * Systems
    update,
    update2d,
    propagate,
    propagate2d,
  )
where

import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import Aztecs.Hierarchy (Hierarchy, hierarchies, mapWithAccum, toList)
import Control.Arrow (Arrow (..), (>>>))
import Control.DeepSeq
import GHC.Generics
import Linear

-- | Transform component.
data Transform v r = Transform
  { transformTranslation :: !v,
    transformRotation :: !r,
    transformScale :: !v
  }
  deriving (Eq, Show, Generic, NFData)

instance (Num v, Num r) => Semigroup (Transform v r) where
  Transform t1 r1 s1 <> Transform t2 r2 s2 = Transform (t1 + t2) (r1 + r2) (s1 + s2)

instance (Num v, Num r) => Monoid (Transform v r) where
  mempty = Transform 0 0 0

-- | 2D transform component.
type Transform2D = Transform (V2 Int) Int

-- | Empty transform.
transform2d :: Transform2D
transform2d = Transform (V2 0 0) 0 (V2 1 1)

instance Component (Transform (V2 Int) Int)

-- | Size component.
newtype Size v = Size {unSize :: v}
  deriving (Generic, NFData)

type Size2D = Size (V2 Int)

size2D :: Size (V2 Integer)
size2D = Size (V2 0 0)

instance Component (Size (V2 Int))

propagateHierarchy :: (Component a, Monoid a) => Hierarchy a -> Hierarchy a
propagateHierarchy = mapWithAccum (\_ t acc -> let t' = t <> acc in (t', t')) mempty

-- | Propagate and update all hierarchies of transform components.
update ::
  forall q arr b m a.
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    ArrowReaderSystem q arr,
    Component a,
    Monoid a,
    MonadAccess b m
  ) =>
  arr () (m ())
update = propagate @_ @_ @a >>> arr (mapM_ $ mapM_ (uncurry A.insert) . toList)

-- | Propagate and update all hierarchies of transform components.
update2d ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    ArrowReaderSystem q arr,
    MonadAccess b m
  ) =>
  arr () (m ())
update2d = propagate @_ @_ @Transform2D >>> arr (mapM_ $ mapM_ (uncurry A.insert) . toList)

propagate ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    ArrowReaderSystem q arr,
    Component a,
    Monoid a
  ) =>
  arr () [Hierarchy a]
propagate = hierarchies Q.fetch >>> arr (map propagateHierarchy)

propagate2d ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    ArrowReaderSystem q arr
  ) =>
  arr () [Hierarchy Transform2D]
propagate2d = propagate
