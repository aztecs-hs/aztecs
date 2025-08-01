{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Data.Maybe
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
