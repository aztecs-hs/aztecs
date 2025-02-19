{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aztecs.Time (Time (..)) where

import Aztecs.ECS
import Data.Word (Word32)

newtype Time = Time {elapsedMS :: Word32}
  deriving (Eq, Ord, Num, Show)

instance Component Time
