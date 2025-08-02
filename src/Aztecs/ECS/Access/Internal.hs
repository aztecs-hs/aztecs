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

module Aztecs.ECS.Access.Internal where

import Aztecs.ECS.HSet
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.World
import Control.Monad.Identity (IdentityT)
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
  {-# INLINE access #-}

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
  {-# INLINE genericAccess #-}

instance (Access cs m c) => GenericAccess cs m (K1 i c) where
  type GenericAccessType (K1 i c) = AccessType c
  genericAccess world = K1 (access world)
  {-# INLINE genericAccess #-}

instance (GenericAccess cs m f) => GenericAccess cs m (M1 i c f) where
  type GenericAccessType (M1 i c f) = GenericAccessType f
  genericAccess world = M1 (genericAccess world)
  {-# INLINE genericAccess #-}

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
  type GenericAccessType (f :*: g) = GenericAccessType f ++ GenericAccessType g
  genericAccess world = genericAccess world :*: genericAccess world
  {-# INLINE genericAccess #-}

instance (PrimMonad m, Queryable cs m a) => Access cs m (Query m a) where
  type AccessType (Query m a) = QueryableAccess a
  access = query
  {-# INLINE access #-}

instance (PrimMonad m) => Access cs m () where
  type AccessType () = '[]
  access _ = ()
  {-# INLINE access #-}

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
{-# INLINE deriveAccess #-}

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
  type AccessType (a, b) = AccessType a ++ AccessType b

instance
  ( Access cs m a,
    Access cs m b,
    Access cs m c,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType b ++ AccessType c),
    Subset (AccessToComponents (AccessType a)) cs,
    Subset (AccessToComponents (AccessType b)) cs,
    Subset (AccessToComponents (AccessType c)) cs,
    Subset (AccessToComponents (AccessType b ++ AccessType c)) cs,
    Subset (AccessToComponents (AccessType a ++ (AccessType b ++ AccessType c))) cs
  ) =>
  Access cs m (a, b, c)
  where
  type AccessType (a, b, c) = AccessType a ++ (AccessType b ++ AccessType c)

instance
  ( Access cs m a,
    Access cs m b,
    Access cs m c,
    Access cs m d,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType a ++ AccessType b),
    ValidAccessInput (AccessType c ++ AccessType d),
    Subset (AccessToComponents (AccessType a)) cs,
    Subset (AccessToComponents (AccessType b)) cs,
    Subset (AccessToComponents (AccessType c)) cs,
    Subset (AccessToComponents (AccessType d)) cs,
    Subset (AccessToComponents (AccessType a ++ AccessType b)) cs,
    Subset (AccessToComponents (AccessType c ++ AccessType d)) cs
  ) =>
  Access cs m (a, b, c, d)
  where
  type
    AccessType (a, b, c, d) =
      ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d))

instance
  ( Access cs m a,
    Access cs m b,
    Access cs m c,
    Access cs m d,
    Access cs m e,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType e),
    ValidAccessInput (AccessType a ++ AccessType b),
    ValidAccessInput ((AccessType c ++ (AccessType d ++ AccessType e))),
    ValidAccessInput (AccessType d ++ AccessType e),
    Subset (AccessToComponents (AccessType a)) cs,
    Subset (AccessToComponents (AccessType b)) cs,
    Subset (AccessToComponents (AccessType c)) cs,
    Subset (AccessToComponents (AccessType d)) cs,
    Subset (AccessToComponents (AccessType e)) cs,
    Subset (AccessToComponents (AccessType a ++ AccessType b)) cs,
    Subset
      (AccessToComponents (AccessType d ++ AccessType e))
      cs,
    Subset
      ( AccessToComponents
          (AccessType c ++ (AccessType d ++ AccessType e))
      )
      cs,
    Subset
      ( AccessToComponents
          ( ( (AccessType a ++ AccessType b)
                ++ (AccessType c ++ (AccessType d ++ AccessType e))
            )
          )
      )
      cs
  ) =>
  Access cs m (a, b, c, d, e)
  where
  type
    AccessType (a, b, c, d, e) =
      ( (AccessType a ++ AccessType b)
          ++ (AccessType c ++ (AccessType d ++ AccessType e))
      )

instance
  ( Access cs m a,
    Access cs m b,
    Access cs m c,
    Access cs m d,
    Access cs m e,
    Access cs m f,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType e),
    ValidAccessInput (AccessType f),
    ValidAccessInput (AccessType e ++ AccessType f),
    ValidAccessInput (AccessType d ++ (AccessType e ++ AccessType f)),
    ValidAccessInput (AccessType a ++ (AccessType b ++ AccessType c)),
    ValidAccessInput (AccessType b ++ AccessType c),
    Subset (AccessToComponents (AccessType a)) cs,
    Subset (AccessToComponents (AccessType b)) cs,
    Subset (AccessToComponents (AccessType c)) cs,
    Subset (AccessToComponents (AccessType d)) cs,
    Subset (AccessToComponents (AccessType e)) cs,
    Subset (AccessToComponents (AccessType f)) cs,
    Subset (AccessToComponents (AccessType b ++ AccessType c)) cs,
    Subset (AccessToComponents (AccessType e ++ AccessType f)) cs,
    Subset
      ( AccessToComponents
          (AccessType a ++ (AccessType b ++ AccessType c))
      )
      cs,
    Subset
      ( AccessToComponents
          (AccessType d ++ (AccessType e ++ AccessType f))
      )
      cs
  ) =>
  Access cs m (a, b, c, d, e, f)
  where
  type
    AccessType (a, b, c, d, e, f) =
      ( (AccessType a ++ (AccessType b ++ AccessType c))
          ++ (AccessType d ++ (AccessType e ++ AccessType f))
      )

instance
  ( Access cs m a,
    Access cs m b,
    Access cs m c,
    Access cs m d,
    Access cs m e,
    Access cs m f,
    Access cs m g,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType e),
    ValidAccessInput (AccessType f),
    ValidAccessInput (AccessType g),
    ValidAccessInput (AccessType b ++ AccessType c),
    ValidAccessInput (AccessType d ++ AccessType e),
    ValidAccessInput (AccessType f ++ AccessType g),
    ValidAccessInput (AccessType a ++ (AccessType b ++ AccessType c)),
    ValidAccessInput ((AccessType d ++ AccessType e) ++ (AccessType f ++ AccessType g)),
    Subset (AccessToComponents (AccessType a)) cs,
    Subset (AccessToComponents (AccessType b)) cs,
    Subset (AccessToComponents (AccessType c)) cs,
    Subset (AccessToComponents (AccessType d)) cs,
    Subset (AccessToComponents (AccessType e)) cs,
    Subset (AccessToComponents (AccessType f)) cs,
    Subset (AccessToComponents (AccessType g)) cs,
    Subset (AccessToComponents (AccessType b ++ AccessType c)) cs,
    Subset (AccessToComponents (AccessType d ++ AccessType e)) cs,
    Subset (AccessToComponents (AccessType f ++ AccessType g)) cs,
    Subset
      ( AccessToComponents
          (AccessType a ++ (AccessType b ++ AccessType c))
      )
      cs,
    Subset
      ( AccessToComponents
          ((AccessType d ++ AccessType e) ++ (AccessType f ++ AccessType g))
      )
      cs
  ) =>
  Access cs m (a, b, c, d, e, f, g)
  where
  type
    AccessType (a, b, c, d, e, f, g) =
      ( (AccessType a ++ (AccessType b ++ AccessType c))
          ++ ((AccessType d ++ AccessType e) ++ (AccessType f ++ AccessType g))
      )

instance
  ( Access cs m a,
    Access cs m b,
    Access cs m c,
    Access cs m d,
    Access cs m e,
    Access cs m f,
    Access cs m g,
    Access cs m h,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType e),
    ValidAccessInput (AccessType f),
    ValidAccessInput (AccessType g),
    ValidAccessInput (AccessType h),
    ValidAccessInput (AccessType a ++ AccessType b),
    ValidAccessInput (AccessType c ++ AccessType d),
    ValidAccessInput (AccessType e ++ AccessType f),
    ValidAccessInput (AccessType g ++ AccessType h),
    ValidAccessInput ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d)),
    ValidAccessInput ((AccessType e ++ AccessType f) ++ (AccessType g ++ AccessType h)),
    Subset (AccessToComponents (AccessType a)) cs,
    Subset (AccessToComponents (AccessType b)) cs,
    Subset (AccessToComponents (AccessType c)) cs,
    Subset (AccessToComponents (AccessType d)) cs,
    Subset (AccessToComponents (AccessType e)) cs,
    Subset (AccessToComponents (AccessType f)) cs,
    Subset (AccessToComponents (AccessType g)) cs,
    Subset (AccessToComponents (AccessType h)) cs,
    Subset (AccessToComponents (AccessType a ++ AccessType b)) cs,
    Subset (AccessToComponents (AccessType c ++ AccessType d)) cs,
    Subset (AccessToComponents (AccessType e ++ AccessType f)) cs,
    Subset (AccessToComponents (AccessType g ++ AccessType h)) cs,
    Subset
      ( AccessToComponents
          ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d))
      )
      cs,
    Subset
      ( AccessToComponents
          ((AccessType e ++ AccessType f) ++ (AccessType g ++ AccessType h))
      )
      cs
  ) =>
  Access cs m (a, b, c, d, e, f, g, h)
  where
  type
    AccessType (a, b, c, d, e, f, g, h) =
      ( ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d))
          ++ ((AccessType e ++ AccessType f) ++ (AccessType g ++ AccessType h))
      )
