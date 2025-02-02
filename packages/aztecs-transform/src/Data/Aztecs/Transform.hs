module Data.Aztecs.Transform where

import Data.Aztecs
import Linear (V2 (..))

data Transform = Transform
  { transformPosition :: V2 Float,
    transformRotation :: Float,
    transformScale :: V2 Float
  } deriving (Eq, Show)

transform :: Transform
transform = Transform (V2 0 0) 0 (V2 1 1)

instance Component Transform
