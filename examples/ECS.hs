{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Aztecs.ECS
import qualified Aztecs.ECS.World as W
import Data.SparseSet.Strict.Mutable (PrimMonad (..))
import GHC.Generics (Generic)

-- fail:
-- q :: (PrimMonad m) => World (PrimState m) '[Int, Bool] -> Query m (Q '[W (PrimState m) Int, W (PrimState m) Int])
-- q = query

data EIntBool m = EIntBool
  { entityId :: Entity,
    intValue :: W (PrimState m) Int,
    boolValue :: W (PrimState m) Bool
  }
  deriving (Generic, Queryable m)

instance Show (EIntBool s) where
  show (EIntBool eid _ _) = "EIntBool{entityId=" ++ show eid ++ ",...}"

data EntityBool = EntityBool
  { entityIdBool :: Entity,
    boolValueQuery :: R Bool
  }
  deriving (Generic)

instance Show EntityBool where
  show (EntityBool eid (R b)) = "EntityBool{entityId=" ++ show eid ++ ",bool=" ++ show b ++ "}"

instance (PrimMonad m) => Queryable m EntityBool

-- Simple data type for querying just Entity and R Bool together
data EntityRBool = EntityRBool Entity (R Bool)
  deriving (Generic)

instance Show EntityRBool where
  show (EntityRBool eid (R b)) = "EntityRBool(" ++ show eid ++ "," ++ show b ++ ")"

instance (PrimMonad m) => Queryable m EntityRBool

main :: IO ()
main = do
  w <- W.empty @_ @'[Int, Bool]
  (e, w') <- W.spawn (42 :: Int) w
  w_with_bool <- W.insert e True w'
  print e
  let q = W.query @_ @(EIntBool IO) w_with_bool
  x <- runQuery q
  putStrLn $ "Found " ++ show (length x) ++ " entities with EIntBool components"

  w'' <- W.removeComponent @_ @_ @Int e w_with_bool
  let q' = W.query @_ @(Entity, R Bool) w''
  x' <- runQuery q'
  print ("After removing Int component:", x')

  w''' <- W.remove e w''
  let q'' = W.query @_ @EntityRBool w'''
  x'' <- runQuery q''
  print ("After removing entity:", x'')
