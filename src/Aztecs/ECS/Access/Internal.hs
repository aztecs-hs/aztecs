{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Access.Internal where

import Aztecs.ECS.Query.Internal
import Data.Kind
import GHC.Generics

type family ValidAccessInput (accesses :: [Type]) :: Constraint where
  ValidAccessInput accesses =
    (ValidAccess accesses, HasDuplicateWrites (WriteComponents accesses) ~ 'False)

type family HasDuplicateWrites (components :: [Type]) :: Bool where
  HasDuplicateWrites '[] = 'False
  HasDuplicateWrites (c ': rest) = Or (Contains c rest) (HasDuplicateWrites rest)

-- | Access class for types that can be accessed within a scoped context.
class (Functor m) => Access m s a where
  type AccessType a :: [Type]
  access :: m a
  default access ::
    forall.
    ( Generic a,
      GenericAccess m s (Rep a),
      ValidAccessInput (GenericAccessType (Rep a)),
      AccessType a ~ GenericAccessType (Rep a)
    ) =>
    m a
  access = deriveAccess @a @m @s
  {-# INLINE access #-}

class GenericAccess m s f where
  type GenericAccessType f :: [Type]
  genericAccess :: (ValidAccessInput (GenericAccessType f)) => m (f p)

instance (Applicative m) => GenericAccess m s U1 where
  type GenericAccessType U1 = '[]
  genericAccess = pure U1
  {-# INLINE genericAccess #-}

instance forall m s c i. (Access m s c) => GenericAccess m s (K1 i c) where
  type GenericAccessType (K1 i c) = AccessType c
  genericAccess = K1 <$> access @m @s @c
  {-# INLINE genericAccess #-}

instance forall m s f i c. (Functor m, GenericAccess m s f) => GenericAccess m s (M1 i c f) where
  type GenericAccessType (M1 i c f) = GenericAccessType f
  genericAccess = M1 <$> genericAccess @m @s @f
  {-# INLINE genericAccess #-}

instance
  forall m s f g.
  ( Applicative m,
    GenericAccess m s f,
    GenericAccess m s g,
    ValidAccessInput (GenericAccessType f),
    ValidAccessInput (GenericAccessType g)
  ) =>
  GenericAccess m s (f :*: g)
  where
  type GenericAccessType (f :*: g) = GenericAccessType f ++ GenericAccessType g
  genericAccess = (:*:) <$> genericAccess @m @s @f <*> genericAccess @m @s @g
  {-# INLINE genericAccess #-}

instance (Applicative m) => Access m s () where
  type AccessType () = '[]
  access = pure ()
  {-# INLINE access #-}

deriveAccess ::
  forall a m s.
  ( Functor m,
    Generic a,
    GenericAccess m s (Rep a),
    ValidAccessInput (GenericAccessType (Rep a))
  ) =>
  m a
deriveAccess = to <$> genericAccess @m @s @(Rep a)
{-# INLINE deriveAccess #-}

type family DeriveAccessType (rep :: Type -> Type) :: [Type] where
  DeriveAccessType rep = GenericAccessType rep

instance
  ( Applicative m,
    Access m s a,
    Access m s b,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType a ++ AccessType b)
  ) =>
  Access m s (a, b)
  where
  type AccessType (a, b) = AccessType a ++ AccessType b

instance
  ( Applicative m,
    Access m s a,
    Access m s b,
    Access m s c,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType b ++ AccessType c),
    ValidAccessInput (AccessType a ++ (AccessType b ++ AccessType c))
  ) =>
  Access m s (a, b, c)
  where
  type AccessType (a, b, c) = AccessType a ++ (AccessType b ++ AccessType c)

instance
  ( Applicative m,
    Access m s a,
    Access m s b,
    Access m s c,
    Access m s d,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType a ++ AccessType b),
    ValidAccessInput (AccessType c ++ AccessType d),
    ValidAccessInput
      ( (AccessType a ++ AccessType b)
          ++ (AccessType c ++ AccessType d)
      )
  ) =>
  Access m s (a, b, c, d)
  where
  type
    AccessType (a, b, c, d) =
      ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d))

instance
  ( Applicative m,
    Access m s a,
    Access m s b,
    Access m s c,
    Access m s d,
    Access m s e,
    ValidAccessInput (AccessType a),
    ValidAccessInput (AccessType b),
    ValidAccessInput (AccessType c),
    ValidAccessInput (AccessType d),
    ValidAccessInput (AccessType e),
    ValidAccessInput (AccessType a ++ AccessType b),
    ValidAccessInput (AccessType c ++ (AccessType d ++ AccessType e)),
    ValidAccessInput (AccessType d ++ AccessType e),
    ValidAccessInput
      ( (AccessType a ++ AccessType b)
          ++ (AccessType c ++ (AccessType d ++ AccessType e))
      )
  ) =>
  Access m s (a, b, c, d, e)
  where
  type
    AccessType (a, b, c, d, e) =
      ( (AccessType a ++ AccessType b)
          ++ (AccessType c ++ (AccessType d ++ AccessType e))
      )

instance
  ( Applicative m,
    Access m s a,
    Access m s b,
    Access m s c,
    Access m s d,
    Access m s e,
    Access m s f,
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
    ValidAccessInput
      ( (AccessType a ++ (AccessType b ++ AccessType c))
          ++ (AccessType d ++ (AccessType e ++ AccessType f))
      )
  ) =>
  Access m s (a, b, c, d, e, f)
  where
  type
    AccessType (a, b, c, d, e, f) =
      ( (AccessType a ++ (AccessType b ++ AccessType c))
          ++ (AccessType d ++ (AccessType e ++ AccessType f))
      )

instance
  ( Applicative m,
    Access m s a,
    Access m s b,
    Access m s c,
    Access m s d,
    Access m s e,
    Access m s f,
    Access m s g,
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
    ValidAccessInput
      ( (AccessType a ++ (AccessType b ++ AccessType c))
          ++ ((AccessType d ++ AccessType e) ++ (AccessType f ++ AccessType g))
      )
  ) =>
  Access m s (a, b, c, d, e, f, g)
  where
  type
    AccessType (a, b, c, d, e, f, g) =
      ( (AccessType a ++ (AccessType b ++ AccessType c))
          ++ ((AccessType d ++ AccessType e) ++ (AccessType f ++ AccessType g))
      )

instance
  ( Applicative m,
    Access m s a,
    Access m s b,
    Access m s c,
    Access m s d,
    Access m s e,
    Access m s f,
    Access m s g,
    Access m s h,
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
    ValidAccessInput
      ( ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d))
          ++ ((AccessType e ++ AccessType f) ++ (AccessType g ++ AccessType h))
      )
  ) =>
  Access m s (a, b, c, d, e, f, g, h)
  where
  type
    AccessType (a, b, c, d, e, f, g, h) =
      ( ((AccessType a ++ AccessType b) ++ (AccessType c ++ AccessType d))
          ++ ((AccessType e ++ AccessType f) ++ (AccessType g ++ AccessType h))
      )
