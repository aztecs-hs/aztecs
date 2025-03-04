{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Aztecs.Transform
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
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
import Control.DeepSeq
import GHC.Generics
import Linear

-- | Transform component.
--
-- @since 9.0
data Transform v r = Transform
  { -- | Translation.
    --
    -- @since 9.0
    transformTranslation :: !v,
    -- | Rotation.
    --
    -- @since 9.0
    transformRotation :: !r,
    -- | Scale.
    --
    -- @since 9.0
    transformScale :: !v
  }
  deriving (Eq, Show, Generic, NFData)

-- | @since 9.0
instance (Num v, Num r) => Semigroup (Transform v r) where
  Transform t1 r1 s1 <> Transform t2 r2 s2 = Transform (t1 + t2) (r1 + r2) (s1 + s2)

-- | @since 9.0
instance (Num v, Num r) => Monoid (Transform v r) where
  mempty = Transform 0 0 0

-- | 2D transform component.
--
-- @since 9.0
type Transform2D = Transform (V2 Int) Int

-- | Empty transform.
--
-- @since 9.0
transform2d :: Transform2D
transform2d = Transform (V2 0 0) 0 (V2 1 1)

-- | @since 9.0
instance Component (Transform (V2 Int) Int)

-- | Size component.
--
-- @since 9.0
newtype Size v = Size {unSize :: v}
  deriving (Generic, NFData)

-- | @since 9.0
type Size2D = Size (V2 Int)

-- | Empty size.
--
-- @since 9.0
size2D :: Size (V2 Integer)
size2D = Size (V2 0 0)

-- | @since 9.0
instance Component (Size (V2 Int))

-- | Propagate a hierarchy of transform components.
--
-- @since 9.0
propagateHierarchy :: (Component a, Monoid a) => Hierarchy a -> Hierarchy a
propagateHierarchy = mapWithAccum (\_ t acc -> let t' = t <> acc in (t', t')) mempty

-- | Propagate and update all hierarchies of transform components.
--
-- @since 9.0
update ::
  forall q s b m a.
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    MonadReaderSystem q s,
    Component a,
    Monoid a,
    MonadAccess b m
  ) =>
  s (m ())
update = mapM_ (\(e, a) -> A.insert e $ bundle a) . concatMap toList <$> propagate @_ @_ @a

-- | Propagate and update all hierarchies of transform components.
--
-- @since 9.0
update2d ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    MonadReaderSystem q s,
    MonadAccess b m
  ) =>
  s (m ())
update2d = update @_ @_ @_ @_ @Transform2D

-- | Propagate all hierarchies of transform components.
--
-- @since 9.0
propagate ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    MonadReaderSystem q s,
    Component a,
    Monoid a
  ) =>
  s [Hierarchy a]
propagate = do
  hs <- hierarchies () Q.fetch
  return $ map propagateHierarchy hs

-- | Propagate all hierarchies of `Transform2D` components.
--
-- @since 9.0
propagate2d ::
  ( ArrowQueryReader q,
    ArrowDynamicQueryReader q,
    MonadReaderSystem q s
  ) =>
  s [Hierarchy Transform2D]
propagate2d = propagate
