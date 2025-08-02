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

module Aztecs.ECS.Queryable.Internal where

import Aztecs.ECS.Entities
import Aztecs.ECS.HSet
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Data.Kind
import Data.Maybe
import qualified Data.Set as Set
import Data.SparseSet.Strict.Mutable
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
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

instance (PrimMonad m, Lookup a cs) => Queryable cs m (With a) where
  type QueryableAccess (With a) = '[With a]
  queryable cs entitiesArg = Query $ do
    withComponent <- MS.toList $ HS.lookup @a cs
    let withComponentIndices = Set.fromList $ map fst $ catMaybes withComponent
        allEntities = entities entitiesArg
        result =
          map
            ( \e ->
                if Set.member (entityIndex e) withComponentIndices
                  then Just (With)
                  else Nothing
            )
            allEntities
    return result

instance (PrimMonad m, Lookup a cs) => Queryable cs m (Without a) where
  type QueryableAccess (Without a) = '[Without a]
  queryable cs entitiesArg = Query $ do
    withComponent <- MS.toList $ HS.lookup @a cs
    let withComponentIndices = Set.fromList $ map fst $ catMaybes withComponent
        allEntities = entities entitiesArg
        result =
          map
            ( \e ->
                if Set.member (entityIndex e) withComponentIndices
                  then Nothing
                  else Just (Without)
            )
            allEntities
    return result

type family AccessComponent (access :: Type) :: Type where
  AccessComponent (Read a) = a
  AccessComponent (Write a) = a
  AccessComponent (With a) = a
  AccessComponent (Without a) = a

type Components s = HSet (ComponentStorage s)

type ComponentStorage s = MSparseSet s Word32

type family GenericQueryableAccess (f :: Type -> Type) :: [Type] where
  GenericQueryableAccess (M1 _ _ f) = GenericQueryableAccess f
  GenericQueryableAccess (f :*: g) = GenericQueryableAccess f ++ GenericQueryableAccess g
  GenericQueryableAccess (K1 _ a) = QueryableAccess a
  GenericQueryableAccess U1 = '[]

class GenericQueryable cs m (f :: Type -> Type) where
  genericQueryableRep :: Components (PrimState m) cs -> Entities -> m [Maybe (f p)]

instance (Monad m) => GenericQueryable s m U1 where
  genericQueryableRep _ _ = pure [Just U1]
  {-# INLINE genericQueryableRep #-}

instance
  ( Monad m,
    GenericQueryable cs m f,
    GenericQueryable cs m g
  ) =>
  GenericQueryable cs m (f :*: g)
  where
  genericQueryableRep components entitiesArg = do
    fs <- genericQueryableRep components entitiesArg
    gs <- genericQueryableRep components entitiesArg
    return $ zipWith (\f g -> (:*:) <$> f <*> g) fs gs
  {-# INLINE genericQueryableRep #-}

instance (Functor m, GenericQueryable s m f) => GenericQueryable s m (M1 i c f) where
  genericQueryableRep components entitiesArg = map (fmap M1) <$> genericQueryableRep components entitiesArg
  {-# INLINE genericQueryableRep #-}

instance (Functor m, PrimMonad m, PrimState m ~ s, Queryable cs m a) => GenericQueryable cs m (K1 i a) where
  genericQueryableRep components entitiesArg = map (fmap K1) <$> unQuery (queryable components entitiesArg)
  {-# INLINE genericQueryableRep #-}

genericQueryable ::
  forall a cs m.
  ( Generic a,
    GenericQueryable cs m (Rep a),
    Functor m
  ) =>
  Components (PrimState m) cs ->
  Entities ->
  m [Maybe a]
genericQueryable components entitiesArg = map (fmap to) <$> genericQueryableRep components entitiesArg
{-# INLINE genericQueryable #-}

class (PrimMonad m) => Queryable cs m a where
  type QueryableAccess a :: [Type]
  type QueryableAccess a = GenericQueryableAccess (Rep a)

  queryable :: Components (PrimState m) cs -> Entities -> Query m a
  default queryable ::
    ( Generic a,
      GenericQueryable cs m (Rep a),
      QueryableAccess a ~ GenericQueryableAccess (Rep a),
      ValidAccess (QueryableAccess a)
    ) =>
    Components (PrimState m) cs ->
    Entities ->
    Query m a
  queryable components entitiesArg = Query $ map (fmap to) <$> genericQueryableRep components entitiesArg
  {-# INLINE queryable #-}

instance (Functor m, Monad m, PrimMonad m) => Queryable cs m Entity where
  type QueryableAccess Entity = '[]
  queryable _ ec = Query $ pure . map pure $ entities ec

instance
  ( Queryable cs m a,
    Queryable cs m b,
    ValidAccess (QueryableAccess a ++ QueryableAccess b)
  ) =>
  Queryable cs m (a, b)
  where
  type QueryableAccess (a, b) = QueryableAccess a ++ QueryableAccess b

instance
  ( Queryable cs m a,
    Queryable cs m b,
    Queryable cs m c,
    ValidAccess (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
  ) =>
  Queryable cs m (a, b, c)
  where
  type QueryableAccess (a, b, c) = QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c)

instance
  ( Queryable cs m a,
    Queryable cs m b,
    Queryable cs m c,
    Queryable cs m d,
    ValidAccess
      ( (QueryableAccess a ++ QueryableAccess b)
          ++ (QueryableAccess c ++ QueryableAccess d)
      )
  ) =>
  Queryable cs m (a, b, c, d)
  where
  type QueryableAccess (a, b, c, d) = (QueryableAccess a ++ QueryableAccess b) ++ (QueryableAccess c ++ QueryableAccess d)

instance
  ( Queryable cs m a,
    Queryable cs m b,
    Queryable cs m c,
    Queryable cs m d,
    Queryable cs m e,
    ValidAccess
      ( (QueryableAccess a ++ QueryableAccess b)
          ++ (QueryableAccess c ++ (QueryableAccess d ++ QueryableAccess e))
      )
  ) =>
  Queryable cs m (a, b, c, d, e)
  where
  type
    QueryableAccess (a, b, c, d, e) =
      (QueryableAccess a ++ QueryableAccess b)
        ++ (QueryableAccess c ++ (QueryableAccess d ++ QueryableAccess e))

instance
  ( Queryable cs m a,
    Queryable cs m b,
    Queryable cs m c,
    Queryable cs m d,
    Queryable cs m e,
    Queryable cs m f,
    ValidAccess
      ( (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
          ++ ( QueryableAccess d
                 ++ (QueryableAccess e ++ QueryableAccess f)
             )
      )
  ) =>
  Queryable cs m (a, b, c, d, e, f)
  where
  type
    QueryableAccess (a, b, c, d, e, f) =
      ( (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
          ++ ( QueryableAccess d
                 ++ (QueryableAccess e ++ QueryableAccess f)
             )
      )

instance
  ( Queryable cs m a,
    Queryable cs m b,
    Queryable cs m c,
    Queryable cs m d,
    Queryable cs m e,
    Queryable cs m f,
    Queryable cs m g,
    ValidAccess
      ( (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
          ++ ( (QueryableAccess d ++ QueryableAccess e)
                 ++ (QueryableAccess f ++ QueryableAccess g)
             )
      )
  ) =>
  Queryable cs m (a, b, c, d, e, f, g)
  where
  type
    QueryableAccess (a, b, c, d, e, f, g) =
      (QueryableAccess a ++ (QueryableAccess b ++ QueryableAccess c))
        ++ ( (QueryableAccess d ++ QueryableAccess e)
               ++ (QueryableAccess f ++ QueryableAccess g)
           )

instance
  ( Queryable cs m a,
    Queryable cs m b,
    Queryable cs m c,
    Queryable cs m d,
    Queryable cs m e,
    Queryable cs m f,
    Queryable cs m g,
    Queryable cs m h,
    ValidAccess
      ( ( (QueryableAccess a ++ QueryableAccess b)
            ++ (QueryableAccess c ++ QueryableAccess d)
        )
          ++ ( (QueryableAccess e ++ QueryableAccess f)
                 ++ (QueryableAccess g ++ QueryableAccess h)
             )
      )
  ) =>
  Queryable cs m (a, b, c, d, e, f, g, h)
  where
  type
    QueryableAccess (a, b, c, d, e, f, g, h) =
      ( (QueryableAccess a ++ QueryableAccess b)
          ++ (QueryableAccess c ++ QueryableAccess d)
      )
        ++ ( (QueryableAccess e ++ QueryableAccess f)
               ++ (QueryableAccess g ++ QueryableAccess h)
           )
