{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aztecs.Time (Time (..)) where

import Aztecs.ECS
import Control.DeepSeq
import Data.Word (Word32)
import GHC.Generics

newtype Time = Time {elapsedMS :: Word32}
  deriving (Eq, Ord, Num, Show, Generic)

instance Component Time

instance NFData Time where
  rnf a = rwhnf a
