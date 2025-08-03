{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable #-}
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

newtype Query a = Query {unQuery :: [Maybe a]}
  deriving (Functor, Foldable)

instance Applicative Query where
  pure x = Query $ [Just x]
  {-# INLINE pure #-}
  Query f <*> Query x = Query $ zipWith (<*>) f x
  {-# INLINE (<*>) #-}

runQuery :: Query a -> [a]
runQuery (Query q) = catMaybes q
{-# INLINE runQuery #-}
