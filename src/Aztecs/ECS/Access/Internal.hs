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

import Aztecs.ECS.Class
import Aztecs.ECS.HSet
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.World
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

class (Functor m) => Access m a where
  type AccessType a :: [Type]
  access :: (ValidAccessInput (AccessType a)) => m a
  default access ::
    ( Generic a,
      GenericAccess m (Rep a),
      ValidAccessInput (GenericAccessType (Rep a)),
      AccessType a ~ GenericAccessType (Rep a)
    ) =>
    m a
  access = deriveAccess
  {-# INLINE access #-}

class GenericAccess m f where
  type GenericAccessType f :: [Type]
  genericAccess :: (ValidAccessInput (GenericAccessType f)) => m (f p)

instance (Applicative m) => GenericAccess m U1 where
  type GenericAccessType U1 = '[]
  genericAccess = pure U1
  {-# INLINE genericAccess #-}

instance (Access m c) => GenericAccess m (K1 i c) where
  type GenericAccessType (K1 i c) = AccessType c
  genericAccess = K1 <$> access
  {-# INLINE genericAccess #-}

instance (Functor m, GenericAccess m f) => GenericAccess m (M1 i c f) where
  type GenericAccessType (M1 i c f) = GenericAccessType f
  genericAccess = M1 <$> genericAccess
  {-# INLINE genericAccess #-}

instance
  ( Applicative m,
    GenericAccess m f,
    GenericAccess m g,
    ValidAccessInput (GenericAccessType f),
    ValidAccessInput (GenericAccessType g)
  ) =>
  GenericAccess m (f :*: g)
  where
  type GenericAccessType (f :*: g) = GenericAccessType f ++ GenericAccessType g
  genericAccess = (:*:) <$> genericAccess <*> genericAccess
  {-# INLINE genericAccess #-}

instance (ECS m, Applicative m, Queryable m a) => Access m (Query a) where
  type AccessType (Query a) = QueryableAccess a
  access = queryable
  {-# INLINE access #-}

instance (Applicative m) => Access m () where
  type AccessType () = '[]
  access = pure ()
  {-# INLINE access #-}

deriveAccess ::
  forall a m cs.
  ( Functor m,
    Generic a,
    GenericAccess m (Rep a),
    ValidAccessInput (GenericAccessType (Rep a))
  ) =>
  m a
deriveAccess = to <$> genericAccess
{-# INLINE deriveAccess #-}

type family DeriveAccessType (rep :: Type -> Type) :: [Type] where
  DeriveAccessType rep = GenericAccessType rep

instance
  ( Applicative m,
    Access m a,
    Access m b,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b)
  ) =>
  Access m (a, b)
  where
  type AccessType (a, b) = AccessType a ++ AccessType b

instance
  ( Applicative m,
    Access m a,
    Access m b,
    Access m c,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType b ++ AccessType c)
  ) =>
  Access m (a, b, c)
  where
  type AccessType (a, b, c) = AccessType a ++ (AccessType b ++ AccessType c)

instance
  ( Applicative m,
    Access m a,
    Access m b,
    Access m c,
    Access m d,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType a ++ AccessType b),
    ValidAccessInput (AccessType c ++ AccessType d)
  ) =>
  Access m (a, b, c, d)
  where
  type
    AccessType (a, b, c, d) =
      ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d))

instance
  ( Applicative m,
    Access m a,
    Access m b,
    Access m c,
    Access m d,
    Access m e,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType e),
    ValidAccessInput (AccessType a ++ AccessType b),
    ValidAccessInput ((AccessType c ++ (AccessType d ++ AccessType e))),
    ValidAccessInput (AccessType d ++ AccessType e)
  ) =>
  Access m (a, b, c, d, e)
  where
  type
    AccessType (a, b, c, d, e) =
      ( (AccessType a ++ AccessType b)
          ++ (AccessType c ++ (AccessType d ++ AccessType e))
      )

instance
  ( Applicative m,
    Access m a,
    Access m b,
    Access m c,
    Access m d,
    Access m e,
    Access m f,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType e),
    ValidAccessInput (AccessType f),
    ValidAccessInput (AccessType e ++ AccessType f),
    ValidAccessInput (AccessType d ++ (AccessType e ++ AccessType f)),
    ValidAccessInput (AccessType a ++ (AccessType b ++ AccessType c)),
    ValidAccessInput (AccessType b ++ AccessType c)
  ) =>
  Access m (a, b, c, d, e, f)
  where
  type
    AccessType (a, b, c, d, e, f) =
      ( (AccessType a ++ (AccessType b ++ AccessType c))
          ++ (AccessType d ++ (AccessType e ++ AccessType f))
      )

instance
  ( Applicative m,
    Access m a,
    Access m b,
    Access m c,
    Access m d,
    Access m e,
    Access m f,
    Access m g,
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
    ValidAccessInput ((AccessType d ++ AccessType e) ++ (AccessType f ++ AccessType g))
  ) =>
  Access m (a, b, c, d, e, f, g)
  where
  type
    AccessType (a, b, c, d, e, f, g) =
      ( (AccessType a ++ (AccessType b ++ AccessType c))
          ++ ((AccessType d ++ AccessType e) ++ (AccessType f ++ AccessType g))
      )

instance
  ( Applicative m,
    Access m a,
    Access m b,
    Access m c,
    Access m d,
    Access m e,
    Access m f,
    Access m g,
    Access m h,
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
    ValidAccessInput ((AccessType e ++ AccessType f) ++ (AccessType g ++ AccessType h))
  ) =>
  Access m (a, b, c, d, e, f, g, h)
  where
  type
    AccessType (a, b, c, d, e, f, g, h) =
      ( ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d))
          ++ ((AccessType e ++ AccessType f) ++ (AccessType g ++ AccessType h))
      )
