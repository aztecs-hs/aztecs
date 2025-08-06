module Aztecs.Entity where

import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word

newtype Entity = Entity {unEntity :: Word64}
  deriving (Eq, Ord)

instance Show Entity where
  show e = "Entity {index = " ++ show (entityIndex e) ++ ", generation = " ++ show (entityGeneration e) ++ "}"

mkEntity :: Word32 -> Word32 -> Entity
mkEntity index generation = Entity $ (fromIntegral generation `shiftL` 32) .|. fromIntegral index
{-# INLINE mkEntity #-}

entityIndex :: Entity -> Word32
entityIndex (Entity e) = fromIntegral (e .&. 0xFFFFFFFF)
{-# INLINE entityIndex #-}

entityGeneration :: Entity -> Word32
entityGeneration (Entity e) = fromIntegral ((e `shiftR` 32) .&. 0xFFFFFFFF)
{-# INLINE entityGeneration #-}
