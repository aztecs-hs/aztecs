{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Aztecs.Window (Window (..)) where

import Aztecs.ECS
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Window component.
data Window = Window
  { -- | Window title.
    windowTitle :: !String
  }
  deriving (Show, Generic, NFData)

instance Component Window
