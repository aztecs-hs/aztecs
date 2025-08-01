{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.System where

import Aztecs.ECS.HSet (Subset)
import Aztecs.ECS.Query (Query)
import Aztecs.ECS.Queryable (AccessToComponents, Contains, If, Queryable, QueryableAccess, ValidAccess, WriteComponents, type (++))
import Aztecs.ECS.World (World, query)
import Control.Monad.Primitive (PrimMonad)
import Data.Kind (Constraint, Type)

type family ValidSystemInputAccess (accesses :: [Type]) :: Constraint where
  ValidSystemInputAccess accesses =
    ( ValidAccess accesses,
      NoOverlappingSystemWrites accesses
    )

type family NoOverlappingSystemWrites (accesses :: [Type]) :: Constraint where
  NoOverlappingSystemWrites accesses =
    (HasDuplicateWrites (WriteComponents accesses) ~ 'False)

type family HasDuplicateWrites (components :: [Type]) :: Bool where
  HasDuplicateWrites '[] = 'False
  HasDuplicateWrites (c ': rest) =
    If (Contains c rest) 'True (HasDuplicateWrites rest)

class (PrimMonad m) => SystemInput m input where
  type SystemInputAccess input :: [Type]
  systemInput ::
    ( Subset (AccessToComponents (SystemInputAccess input)) cs,
      ValidSystemInputAccess (SystemInputAccess input)
    ) =>
    World m cs ->
    input

instance (PrimMonad m, Queryable m a) => SystemInput m (Query m a) where
  type SystemInputAccess (Query m a) = QueryableAccess a
  systemInput = query

instance (PrimMonad m) => SystemInput m () where
  type SystemInputAccess () = '[]
  systemInput _ = ()

class (PrimMonad m) => System m sys where
  type SystemConstraints sys (cs :: [Type]) :: Constraint
  runSystem :: (SystemConstraints sys cs) => sys -> World m cs -> m ()

instance (PrimMonad m) => System m (m ()) where
  type SystemConstraints (m ()) cs = ()
  runSystem action _ = action

instance (PrimMonad m, SystemInput m input) => System m (input -> m ()) where
  type
    SystemConstraints (input -> m ()) cs =
      ( Subset (AccessToComponents (SystemInputAccess input)) cs,
        ValidSystemInputAccess (SystemInputAccess input)
      )
  runSystem sys world = sys (systemInput world)

instance (PrimMonad m, SystemInput m input1, SystemInput m input2) => System m (input1 -> input2 -> m ()) where
  type
    SystemConstraints (input1 -> input2 -> m ()) cs =
      ( Subset (AccessToComponents (SystemInputAccess input1)) cs,
        Subset (AccessToComponents (SystemInputAccess input2)) cs,
        ValidSystemInputAccess (SystemInputAccess input1),
        ValidSystemInputAccess (SystemInputAccess input2),
        NoOverlappingSystemWrites (SystemInputAccess input1 ++ SystemInputAccess input2)
      )
  runSystem sys world = sys (systemInput world) (systemInput world)

instance (PrimMonad m, SystemInput m input1, SystemInput m input2, SystemInput m input3) => System m (input1 -> input2 -> input3 -> m ()) where
  type
    SystemConstraints (input1 -> input2 -> input3 -> m ()) cs =
      ( Subset (AccessToComponents (SystemInputAccess input1)) cs,
        Subset (AccessToComponents (SystemInputAccess input2)) cs,
        Subset (AccessToComponents (SystemInputAccess input3)) cs,
        ValidSystemInputAccess (SystemInputAccess input1),
        ValidSystemInputAccess (SystemInputAccess input2),
        ValidSystemInputAccess (SystemInputAccess input3),
        NoOverlappingSystemWrites (SystemInputAccess input1 ++ SystemInputAccess input2 ++ SystemInputAccess input3)
      )
  runSystem sys world = sys (systemInput world) (systemInput world) (systemInput world)
