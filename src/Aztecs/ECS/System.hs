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

import Aztecs.ECS.HSet
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.World
import Control.Monad.Primitive
import Data.Kind

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
  type SystemAccess input :: [Type]
  systemInput ::
    ( Subset (AccessToComponents (SystemAccess input)) cs,
      ValidSystemInputAccess (SystemAccess input)
    ) =>
    World m cs ->
    input

instance (PrimMonad m, Queryable m a) => SystemInput m (Query m a) where
  type SystemAccess (Query m a) = QueryableAccess a
  systemInput = query
  {-# INLINE systemInput #-}

instance (PrimMonad m) => SystemInput m () where
  type SystemAccess () = '[]
  systemInput _ = ()

class (PrimMonad m) => System m sys where
  type SystemInputs sys
  runSystem :: sys -> SystemInputs sys -> m ()

runSystemWithWorld ::
  ( System m sys,
    SystemInput m (SystemInputs sys),
    Subset (AccessToComponents (SystemAccess (SystemInputs sys))) cs,
    ValidSystemInputAccess (SystemAccess (SystemInputs sys))
  ) =>
  sys ->
  World m cs ->
  m ()
runSystemWithWorld sys world = runSystem sys (systemInput world)
