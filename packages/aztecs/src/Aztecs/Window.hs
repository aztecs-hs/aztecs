module Aztecs.Window (Window (..)) where

import Aztecs.ECS

-- | Window component.
data Window = Window
  { -- | Window title.
    windowTitle :: !String
  }
  deriving (Show)

instance Component Window
