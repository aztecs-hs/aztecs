{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Aztecs.Transform where

import Aztecs.ECS
import Control.DeepSeq
import GHC.Generics (Generic)
import Linear (V2 (..))

data Transform = Transform
  { transformPosition :: !(V2 Int),
    transformRotation :: !Float,
    transformScale :: !(V2 Int)
  }
  deriving (Eq, Show, Generic, NFData)

transform :: Transform
transform = Transform (V2 0 0) 0 (V2 1 1)

instance Component Transform

newtype Size = Size {unSize :: V2 Int}
  deriving (Generic, NFData)

instance Component Size
