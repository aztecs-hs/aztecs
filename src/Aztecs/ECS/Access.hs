{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Access where

import Aztecs.ECS.HSet
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.World
import Control.Monad.Primitive
import Data.Kind
import GHC.Generics

type family ValidAccessInput (accesses :: [Type]) :: Constraint where
  ValidAccessInput accesses =
    ( ValidAccess accesses,
      NoOverlappingWrites accesses
    )

type family NoOverlappingWrites (accesses :: [Type]) :: Constraint where
  NoOverlappingWrites accesses =
    (HasDuplicateWrites (WriteComponents accesses) ~ 'False)

type family HasDuplicateWrites (components :: [Type]) :: Bool where
  HasDuplicateWrites '[] = 'False
  HasDuplicateWrites (c ': rest) = Or (Contains c rest) (HasDuplicateWrites rest)

class (PrimMonad m) => Access cs m a where
  type AccessType a :: [Type]
  access ::
    ( Subset (AccessToComponents (AccessType a)) cs,
      ValidAccessInput (AccessType a)
    ) =>
    World m cs ->
    a
  default access ::
    ( Generic a,
      GenericAccess cs m (Rep a),
      Subset (AccessToComponents (GenericAccessType (Rep a))) cs,
      ValidAccessInput (GenericAccessType (Rep a)),
      AccessType a ~ GenericAccessType (Rep a)
    ) =>
    World m cs ->
    a
  access = deriveAccess

class GenericAccess cs m f where
  type GenericAccessType f :: [Type]
  genericAccess ::
    ( Subset (AccessToComponents (GenericAccessType f)) cs,
      ValidAccessInput (GenericAccessType f)
    ) =>
    World m cs ->
    f p

instance GenericAccess cs m U1 where
  type GenericAccessType U1 = '[]
  genericAccess _ = U1

instance (Access cs m c) => GenericAccess cs m (K1 i c) where
  type GenericAccessType (K1 i c) = AccessType c
  genericAccess world = K1 (access world)

instance (GenericAccess cs m f) => GenericAccess cs m (M1 i c f) where
  type GenericAccessType (M1 i c f) = GenericAccessType f
  genericAccess world = M1 (genericAccess world)

instance
  ( GenericAccess cs m f,
    GenericAccess cs m g,
    Subset (AccessToComponents (GenericAccessType f)) cs,
    ValidAccessInput (GenericAccessType f),
    Subset (AccessToComponents (GenericAccessType g)) cs,
    ValidAccessInput (GenericAccessType g)
  ) =>
  GenericAccess cs m (f :*: g)
  where
  type GenericAccessType (f :*: g) = Append (GenericAccessType f) (GenericAccessType g)
  genericAccess world = genericAccess world :*: genericAccess world

type family Append (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

instance (PrimMonad m, Queryable m a) => Access cs m (Query m a) where
  type AccessType (Query m a) = QueryableAccess a
  access = query
  {-# INLINE access #-}

instance (PrimMonad m) => Access cs m () where
  type AccessType () = '[]
  access _ = ()

deriveAccess ::
  forall a m cs.
  ( Generic a,
    GenericAccess cs m (Rep a),
    Subset (AccessToComponents (GenericAccessType (Rep a))) cs,
    ValidAccessInput (GenericAccessType (Rep a))
  ) =>
  World m cs ->
  a
deriveAccess world = to (genericAccess world)

type family DeriveAccessType (rep :: Type -> Type) :: [Type] where
  DeriveAccessType rep = GenericAccessType rep

instance
  ( Access cs m a,
    Access cs m b,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    Subset (AccessToComponents (AccessType a)) cs,
    Subset (AccessToComponents (AccessType b)) cs
  ) =>
  Access cs m (a, b)
  where
  type AccessType (a, b) = Append (AccessType a) (AccessType b)

instance
  ( Access cs m a,
    Access cs m b,
    Access cs m c,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (Append (AccessType b) (AccessType c)),
    Subset (AccessToComponents (AccessType a)) cs,
    Subset (AccessToComponents (AccessType b)) cs,
    Subset (AccessToComponents (AccessType c)) cs,
    Subset (AccessToComponents (Append (AccessType b) (AccessType c))) cs,
    Subset (AccessToComponents (Append (AccessType a) (Append (AccessType b) (AccessType c)))) cs
  ) =>
  Access cs m (a, b, c)
  where
  type AccessType (a, b, c) = Append (AccessType a) (Append (AccessType b) (AccessType c))
