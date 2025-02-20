{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

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
  )
where

import Aztecs.ECS
import Control.DeepSeq
import GHC.Generics (Generic)
import Linear (V2 (..))

-- | Transform component.
data Transform v r = Transform
  { transformTranslation :: !v,
    transformRotation :: !r,
    transformScale :: !v
  }
  deriving (Eq, Show, Generic, NFData)

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
