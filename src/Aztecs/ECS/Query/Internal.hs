{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Query.Internal where

import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Data.Kind
import Data.Maybe
import GHC.Generics
import Prelude hiding (Read)

type family (++) (xs :: [Type]) (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family AccessToComponents (accesses :: [Type]) :: [Type] where
  AccessToComponents '[] = '[]
  AccessToComponents (Read a ': rest) = a ': AccessToComponents rest
  AccessToComponents (Write a ': rest) = a ': AccessToComponents rest
  AccessToComponents (With a ': rest) = a ': AccessToComponents rest
  AccessToComponents (Without a ': rest) = AccessToComponents rest

type family ReadComponents (accesses :: [Type]) :: [Type] where
  ReadComponents '[] = '[]
  ReadComponents (Read a ': rest) = a ': ReadComponents rest
  ReadComponents (Write a ': rest) = ReadComponents rest
  ReadComponents (With a ': rest) = ReadComponents rest
  ReadComponents (Without a ': rest) = ReadComponents rest

type family WriteComponents (accesses :: [Type]) :: [Type] where
  WriteComponents '[] = '[]
  WriteComponents (Read a ': rest) = WriteComponents rest
  WriteComponents (Write a ': rest) = a ': WriteComponents rest
  WriteComponents (With a ': rest) = WriteComponents rest
  WriteComponents (Without a ': rest) = WriteComponents rest

type family WithComponents (accesses :: [Type]) :: [Type] where
  WithComponents '[] = '[]
  WithComponents (Read a ': rest) = WithComponents rest
  WithComponents (Write a ': rest) = WithComponents rest
  WithComponents (With a ': rest) = a ': WithComponents rest
  WithComponents (Without a ': rest) = WithComponents rest

type family WithoutComponents (accesses :: [Type]) :: [Type] where
  WithoutComponents '[] = '[]
  WithoutComponents (Read a ': rest) = WithoutComponents rest
  WithoutComponents (Write a ': rest) = WithoutComponents rest
  WithoutComponents (With a ': rest) = WithoutComponents rest
  WithoutComponents (Without a ': rest) = a ': WithoutComponents rest

type family Contains (a :: Type) (list :: [Type]) :: Bool where
  Contains a '[] = 'False
  Contains a (a ': rest) = 'True
  Contains a (b ': rest) = Contains a rest

type family HasOverlap (list1 :: [Type]) (list2 :: [Type]) :: Bool where
  HasOverlap '[] list2 = 'False
  HasOverlap (a ': rest) list2 = Or (Contains a list2) (HasOverlap rest list2)

type family HasDuplicates (list :: [Type]) :: Bool where
  HasDuplicates '[] = 'False
  HasDuplicates (a ': rest) = Or (Contains a rest) (HasDuplicates rest)

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

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True _ = 'True
  Or 'False 'True = 'True
  Or 'False 'False = 'False

type family Not (b :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

type ValidAccess accesses = (ValidateAccess accesses ~ 'True)

data Read (a :: Type)

data Write (a :: Type)

data With (a :: Type) = With

data Without (a :: Type) = Without

type family AccessComponent (access :: Type) :: Type where
  AccessComponent (Read a) = a
  AccessComponent (Write a) = a
  AccessComponent (With a) = a
  AccessComponent (Without a) = a

type family GenericQueryableAccess (f :: Type -> Type) :: [Type] where
  GenericQueryableAccess (M1 _ _ f) = GenericQueryableAccess f
  GenericQueryableAccess (f :*: g) = GenericQueryableAccess f ++ GenericQueryableAccess g
  GenericQueryableAccess (K1 _ a) = QueryableAccess a
  GenericQueryableAccess U1 = '[]

class GenericQueryable m (f :: Type -> Type) where
  genericQueryableRep :: m [Maybe (f p)]

instance (Monad m) => GenericQueryable m U1 where
  genericQueryableRep = pure [Just U1]
  {-# INLINE genericQueryableRep #-}

instance
  ( Monad m,
    GenericQueryable m f,
    GenericQueryable m g
  ) =>
  GenericQueryable m (f :*: g)
  where
  genericQueryableRep = do
    fs <- genericQueryableRep
    zipWith (\f g -> (:*:) <$> f <*> g) fs <$> genericQueryableRep
  {-# INLINE genericQueryableRep #-}

instance (Functor m, GenericQueryable m f) => GenericQueryable m (M1 i c f) where
  genericQueryableRep = map (fmap M1) <$> genericQueryableRep
  {-# INLINE genericQueryableRep #-}

instance (Functor m, Queryable m a) => GenericQueryable m (K1 i a) where
  genericQueryableRep = fmap (map (fmap K1) . unQuery) queryable
  {-# INLINE genericQueryableRep #-}

class Queryable m a where
  type QueryableAccess a :: [Type]
  type QueryableAccess a = GenericQueryableAccess (Rep a)

  queryable :: m (Query a)
  default queryable ::
    ( Functor m,
      Generic a,
      GenericQueryable m (Rep a),
      QueryableAccess a ~ GenericQueryableAccess (Rep a),
      ValidAccess (QueryableAccess a)
    ) =>
    m (Query a)
  queryable = fmap (Query . map (fmap to)) genericQueryableRep
  {-# INLINE queryable #-}

instance
  ( Monad m,
    Queryable m a,
    Queryable m b,
    ValidAccess (QueryableAccess a ++ QueryableAccess b)
  ) =>
  Queryable m (a, b)
  where
  type QueryableAccess (a, b) = QueryableAccess a ++ QueryableAccess b

instance
  ( Monad m,
    Queryable m a,
    Queryable m b,
    Queryable m c,
    ValidAccess (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
  ) =>
  Queryable m (a, b, c)
  where
  type QueryableAccess (a, b, c) = QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c)

instance
  ( Monad m,
    Queryable m a,
    Queryable m b,
    Queryable m c,
    Queryable m d,
    ValidAccess
      ((QueryableAccess a ++ QueryableAccess b) ++ (QueryableAccess c ++ QueryableAccess d))
  ) =>
  Queryable m (a, b, c, d)
  where
  type QueryableAccess (a, b, c, d) = (QueryableAccess a ++ QueryableAccess b) ++ (QueryableAccess c ++ QueryableAccess d)

instance
  ( Monad m,
    Queryable m a,
    Queryable m b,
    Queryable m c,
    Queryable m d,
    Queryable m e,
    ValidAccess
      ( (QueryableAccess a ++ QueryableAccess b)
          ++ (QueryableAccess c ++ (QueryableAccess d ++ QueryableAccess e))
      )
  ) =>
  Queryable m (a, b, c, d, e)
  where
  type
    QueryableAccess (a, b, c, d, e) =
      (QueryableAccess a ++ QueryableAccess b)
        ++ (QueryableAccess c ++ (QueryableAccess d ++ QueryableAccess e))

instance
  ( Monad m,
    Queryable m a,
    Queryable m b,
    Queryable m c,
    Queryable m d,
    Queryable m e,
    Queryable m f,
    ValidAccess
      ( (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
          ++ (QueryableAccess d ++ (QueryableAccess e ++ QueryableAccess f))
      )
  ) =>
  Queryable m (a, b, c, d, e, f)
  where
  type
    QueryableAccess (a, b, c, d, e, f) =
      ( (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
          ++ (QueryableAccess d ++ (QueryableAccess e ++ QueryableAccess f))
      )

instance
  ( Monad m,
    Queryable m a,
    Queryable m b,
    Queryable m c,
    Queryable m d,
    Queryable m e,
    Queryable m f,
    Queryable m g,
    ValidAccess
      ( (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
          ++ ((QueryableAccess d ++ QueryableAccess e) ++ (QueryableAccess f ++ QueryableAccess g))
      )
  ) =>
  Queryable m (a, b, c, d, e, f, g)
  where
  type
    QueryableAccess (a, b, c, d, e, f, g) =
      (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
        ++ ((QueryableAccess d ++ QueryableAccess e) ++ (QueryableAccess f ++ QueryableAccess g))

instance
  ( Monad m,
    Queryable m a,
    Queryable m b,
    Queryable m c,
    Queryable m d,
    Queryable m e,
    Queryable m f,
    Queryable m g,
    Queryable m h,
    ValidAccess
      ( ((QueryableAccess a ++ QueryableAccess b) ++ (QueryableAccess c ++ QueryableAccess d))
          ++ ((QueryableAccess e ++ QueryableAccess f) ++ (QueryableAccess g ++ QueryableAccess h))
      )
  ) =>
  Queryable m (a, b, c, d, e, f, g, h)
  where
  type
    QueryableAccess (a, b, c, d, e, f, g, h) =
      ((QueryableAccess a ++ QueryableAccess b) ++ (QueryableAccess c ++ QueryableAccess d))
        ++ ((QueryableAccess e ++ QueryableAccess f) ++ (QueryableAccess g ++ QueryableAccess h))
