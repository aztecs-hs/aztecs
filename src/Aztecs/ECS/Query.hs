{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
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

module Aztecs.ECS.Query where

import Aztecs.ECS.Entities
import Aztecs.ECS.HSet
import Control.Monad.Identity
import Data.Kind
import Data.Maybe
import Data.SparseSet.Strict.Mutable (MSparseSet, PrimMonad (..))
import Data.Word
import Prelude hiding (Read)

newtype Query m a = Query {unQuery :: m [Maybe a]}
  deriving (Functor)

instance (Monad m) => Applicative (Query m) where
  pure x = Query $ return [Just x]
  {-# INLINE pure #-}
  Query f <*> Query x = Query $ do
    fs <- f
    zipWith (<*>) fs <$> x
  {-# INLINE (<*>) #-}

runQuery :: (Monad m) => Query m a -> m [a]
runQuery (Query q) = catMaybes <$> q
{-# INLINE runQuery #-}

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

class QueryItem s m a where
  type QueryItemAccess a :: [Type]
  queryable :: Components s (AccessToComponents (QueryItemAccess a)) -> Entities -> Query m a

instance (Monad m) => QueryItem s m Entity where
  type QueryItemAccess Entity = '[]
  queryable _ ec = Query . pure . map pure $ entities ec

type family CollectAccess (q :: [Type]) :: [Type] where
  CollectAccess '[] = '[]
  CollectAccess (a ': rest) = QueryItemAccess a ++ CollectAccess rest

type family (++) (xs :: [Type]) (ys :: [Type]) :: [Type] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type Q = HSet (Identity)

class QueryTo w m a where
  queryTo :: w -> Entities -> Query m a

instance
  ( Monad m,
    QueryFrom a m w,
    QueryTo w m (Q as),
    ValidAccess (CollectAccess (a ': as))
  ) =>
  QueryTo w m (Q (a ': as))
  where
  queryTo w ec = HCons <$> (Identity <$> queryFrom w ec) <*> queryTo w ec

instance (Monad m) => QueryTo w m (Q '[]) where
  queryTo _ _ = pure HEmpty

class QueryFrom a m w where
  queryFrom :: w -> Entities -> Query m a

instance
  forall s m a cs.
  ( QueryItem s m a,
    Subset (AccessToComponents (QueryItemAccess a)) cs,
    PrimState m ~ s
  ) =>
  QueryFrom a m (Components s cs)
  where
  queryFrom components =
    queryable (subset @(AccessToComponents (QueryItemAccess a)) components)

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
