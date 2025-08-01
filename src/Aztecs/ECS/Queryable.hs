{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Queryable where

import Aztecs.ECS.Entities
import Aztecs.ECS.HSet
import Aztecs.ECS.Query
import Data.Kind
import Data.Maybe
import Data.SparseSet.Strict.Mutable (MSparseSet, PrimMonad (..))
import Data.Word
import GHC.Generics
import Prelude hiding (Read)

type family (++) (xs :: [Type]) (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family ReadComponents (accesses :: [Type]) :: [Type] where
  ReadComponents '[] = '[]
  ReadComponents (Read a ': rest) = a ': ReadComponents rest
  ReadComponents (Write a ': rest) = ReadComponents rest

type family WriteComponents (accesses :: [Type]) :: [Type] where
  WriteComponents '[] = '[]
  WriteComponents (Read a ': rest) = WriteComponents rest
  WriteComponents (Write a ': rest) = a ': WriteComponents rest

type family Contains (a :: Type) (list :: [Type]) :: Bool where
  Contains a '[] = 'False
  Contains a (a ': rest) = 'True
  Contains a (b ': rest) = Contains a rest

type family HasOverlap (list1 :: [Type]) (list2 :: [Type]) :: Bool where
  HasOverlap '[] list2 = 'False
  HasOverlap (a ': rest) list2 =
    If (Contains a list2) 'True (HasOverlap rest list2)

type family If (condition :: Bool) (then_ :: k) (else_ :: k) :: k where
  If 'True then_ else_ = then_
  If 'False then_ else_ = else_

type family HasDuplicates (list :: [Type]) :: Bool where
  HasDuplicates '[] = 'False
  HasDuplicates (a ': rest) = If (Contains a rest) 'True (HasDuplicates rest)

type family ValidateAccess (accesses :: [Type]) :: Bool where
  ValidateAccess accesses =
    And
      (Not (HasOverlap (WriteComponents accesses) (ReadComponents accesses)))
      (Not (HasDuplicates (WriteComponents accesses)))

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And 'True 'False = 'False
  And 'False 'True = 'False
  And 'False 'False = 'False

type family Not (b :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type ValidAccess accesses = (ValidateAccess accesses ~ 'True)

data Read (a :: Type)

data Write (a :: Type)

type family AccessComponent (access :: Type) :: Type where
  AccessComponent (Read a) = a
  AccessComponent (Write a) = a

type family AccessToComponents (accesses :: [Type]) :: [Type] where
  AccessToComponents '[] = '[]
  AccessToComponents (access ': rest) = AccessComponent access ': AccessToComponents rest

type Components s = HSet (ComponentStorage s)

type ComponentStorage s = MSparseSet s Word32

-- Generic derivation support for Queryable
type family GenericQueryableAccess (f :: Type -> Type) :: [Type] where
  GenericQueryableAccess (M1 _ _ f) = GenericQueryableAccess f
  GenericQueryableAccess (f :*: g) = GenericQueryableAccess f ++ GenericQueryableAccess g
  GenericQueryableAccess (K1 _ a) = QueryableAccess a
  GenericQueryableAccess U1 = '[]

class GenericQueryable s m (f :: Type -> Type) where
  genericQueryableRep :: Components s (AccessToComponents (GenericQueryableAccess f)) -> Entities -> m [Maybe (f p)]

instance (Monad m) => GenericQueryable s m U1 where
  genericQueryableRep _ _ = pure [Just U1]

instance
  ( Monad m,
    GenericQueryable s m f,
    GenericQueryable s m g,
    Subset (AccessToComponents (GenericQueryableAccess f)) (AccessToComponents (GenericQueryableAccess f ++ GenericQueryableAccess g)),
    Subset (AccessToComponents (GenericQueryableAccess g)) (AccessToComponents (GenericQueryableAccess f ++ GenericQueryableAccess g))
  ) =>
  GenericQueryable s m (f :*: g)
  where
  genericQueryableRep components entitiesArg = do
    let componentsF = subset @(AccessToComponents (GenericQueryableAccess f)) components
        componentsG = subset @(AccessToComponents (GenericQueryableAccess g)) components
    fs <- genericQueryableRep componentsF entitiesArg
    gs <- genericQueryableRep componentsG entitiesArg
    return $ zipWith (\f g -> (:*:) <$> f <*> g) fs gs

instance (Functor m, GenericQueryable s m f) => GenericQueryable s m (M1 i c f) where
  genericQueryableRep components entitiesArg = map (fmap M1) <$> genericQueryableRep components entitiesArg

instance (Functor m, PrimMonad m, PrimState m ~ s, Queryable m a) => GenericQueryable s m (K1 i a) where
  genericQueryableRep components entitiesArg = map (fmap K1) <$> unQuery (queryable components entitiesArg)

genericQueryable ::
  forall a s m.
  ( Generic a,
    GenericQueryable s m (Rep a),
    Functor m
  ) =>
  Components s (AccessToComponents (GenericQueryableAccess (Rep a))) ->
  Entities ->
  m [Maybe a]
genericQueryable components entitiesArg = map (fmap to) <$> genericQueryableRep components entitiesArg

class (PrimMonad m) => Queryable m a where
  type QueryableAccess a :: [Type]
  type QueryableAccess a = GenericQueryableAccess (Rep a)

  queryable :: Components (PrimState m) (AccessToComponents (QueryableAccess a)) -> Entities -> Query m a
  default queryable ::
    ( Generic a,
      GenericQueryable (PrimState m) m (Rep a),
      QueryableAccess a ~ GenericQueryableAccess (Rep a),
      ValidAccess (QueryableAccess a)
    ) =>
    Components (PrimState m) (AccessToComponents (QueryableAccess a)) ->
    Entities ->
    Query m a
  queryable components entitiesArg = Query $ map (fmap to) <$> genericQueryableRep components entitiesArg

instance (Functor m, Monad m, PrimMonad m) => Queryable m Entity where
  type QueryableAccess Entity = '[]
  queryable _ ec = Query $ pure . map pure $ entities ec

instance
  ( Queryable m a,
    Queryable m b,
    Subset (AccessToComponents (QueryableAccess a)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b)),
    Subset (AccessToComponents (QueryableAccess b)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b)),
    ValidAccess (QueryableAccess a ++ QueryableAccess b)
  ) =>
  Queryable m (a, b)
  where
  type QueryableAccess (a, b) = QueryableAccess a ++ QueryableAccess b
  queryable components entitiesArg =
    let componentsA = subset @(AccessToComponents (QueryableAccess a)) components
        componentsB = subset @(AccessToComponents (QueryableAccess b)) components
        qa = queryable componentsA entitiesArg
        qb = queryable componentsB entitiesArg
     in (,) <$> qa <*> qb

instance
  ( Queryable m a,
    Queryable m b,
    Queryable m c,
    Queryable m d,
    Queryable m e,
    Queryable m f,
    Subset (AccessToComponents (QueryableAccess a)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f)),
    Subset (AccessToComponents (QueryableAccess b)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f)),
    Subset (AccessToComponents (QueryableAccess c)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f)),
    Subset (AccessToComponents (QueryableAccess d)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f)),
    Subset (AccessToComponents (QueryableAccess e)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f)),
    Subset (AccessToComponents (QueryableAccess f)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f)),
    ValidAccess (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f)
  ) =>
  Queryable m (a, b, c, d, e, f)
  where
  type QueryableAccess (a, b, c, d, e, f) = QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f
  queryable components entitiesArg =
    let componentsA = subset @(AccessToComponents (QueryableAccess a)) components
        componentsB = subset @(AccessToComponents (QueryableAccess b)) components
        componentsC = subset @(AccessToComponents (QueryableAccess c)) components
        componentsD = subset @(AccessToComponents (QueryableAccess d)) components
        componentsE = subset @(AccessToComponents (QueryableAccess e)) components
        componentsF = subset @(AccessToComponents (QueryableAccess f)) components
        qa = queryable componentsA entitiesArg
        qb = queryable componentsB entitiesArg
        qc = queryable componentsC entitiesArg
        qd = queryable componentsD entitiesArg
        qe = queryable componentsE entitiesArg
        qf = queryable componentsF entitiesArg
     in (,,,,,) <$> qa <*> qb <*> qc <*> qd <*> qe <*> qf

instance
  ( Queryable m a,
    Queryable m b,
    Queryable m c,
    Queryable m d,
    Queryable m e,
    Queryable m f,
    Queryable m g,
    Subset (AccessToComponents (QueryableAccess a)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g)),
    Subset (AccessToComponents (QueryableAccess b)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g)),
    Subset (AccessToComponents (QueryableAccess c)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g)),
    Subset (AccessToComponents (QueryableAccess d)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g)),
    Subset (AccessToComponents (QueryableAccess e)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g)),
    Subset (AccessToComponents (QueryableAccess f)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g)),
    Subset (AccessToComponents (QueryableAccess g)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g)),
    ValidAccess (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g)
  ) =>
  Queryable m (a, b, c, d, e, f, g)
  where
  type QueryableAccess (a, b, c, d, e, f, g) = QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g
  queryable components entitiesArg =
    let componentsA = subset @(AccessToComponents (QueryableAccess a)) components
        componentsB = subset @(AccessToComponents (QueryableAccess b)) components
        componentsC = subset @(AccessToComponents (QueryableAccess c)) components
        componentsD = subset @(AccessToComponents (QueryableAccess d)) components
        componentsE = subset @(AccessToComponents (QueryableAccess e)) components
        componentsF = subset @(AccessToComponents (QueryableAccess f)) components
        componentsG = subset @(AccessToComponents (QueryableAccess g)) components
        qa = queryable componentsA entitiesArg
        qb = queryable componentsB entitiesArg
        qc = queryable componentsC entitiesArg
        qd = queryable componentsD entitiesArg
        qe = queryable componentsE entitiesArg
        qf = queryable componentsF entitiesArg
        qg = queryable componentsG entitiesArg
     in (,,,,,,) <$> qa <*> qb <*> qc <*> qd <*> qe <*> qf <*> qg

instance
  ( Queryable m a,
    Queryable m b,
    Queryable m c,
    Queryable m d,
    Queryable m e,
    Queryable m f,
    Queryable m g,
    Queryable m h,
    Subset (AccessToComponents (QueryableAccess a)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)),
    Subset (AccessToComponents (QueryableAccess b)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)),
    Subset (AccessToComponents (QueryableAccess c)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)),
    Subset (AccessToComponents (QueryableAccess d)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)),
    Subset (AccessToComponents (QueryableAccess e)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)),
    Subset (AccessToComponents (QueryableAccess f)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)),
    Subset (AccessToComponents (QueryableAccess g)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)),
    Subset (AccessToComponents (QueryableAccess h)) (AccessToComponents (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)),
    ValidAccess (QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h)
  ) =>
  Queryable m (a, b, c, d, e, f, g, h)
  where
  type QueryableAccess (a, b, c, d, e, f, g, h) = QueryableAccess a ++ QueryableAccess b ++ QueryableAccess c ++ QueryableAccess d ++ QueryableAccess e ++ QueryableAccess f ++ QueryableAccess g ++ QueryableAccess h
  queryable components entitiesArg =
    let componentsA = subset @(AccessToComponents (QueryableAccess a)) components
        componentsB = subset @(AccessToComponents (QueryableAccess b)) components
        componentsC = subset @(AccessToComponents (QueryableAccess c)) components
        componentsD = subset @(AccessToComponents (QueryableAccess d)) components
        componentsE = subset @(AccessToComponents (QueryableAccess e)) components
        componentsF = subset @(AccessToComponents (QueryableAccess f)) components
        componentsG = subset @(AccessToComponents (QueryableAccess g)) components
        componentsH = subset @(AccessToComponents (QueryableAccess h)) components
        qa = queryable componentsA entitiesArg
        qb = queryable componentsB entitiesArg
        qc = queryable componentsC entitiesArg
        qd = queryable componentsD entitiesArg
        qe = queryable componentsE entitiesArg
        qf = queryable componentsF entitiesArg
        qg = queryable componentsG entitiesArg
        qh = queryable componentsH entitiesArg
     in (,,,,,,,) <$> qa <*> qb <*> qc <*> qd <*> qe <*> qf <*> qg <*> qh
