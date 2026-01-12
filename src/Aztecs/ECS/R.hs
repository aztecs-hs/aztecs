{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RoleAnnotations #-}

module Aztecs.ECS.R (R (..)) where

-- | Read-only 'Queryable' component access.
data R s a = R {unR :: a}
  deriving (Show, Eq, Functor)

type role R phantom nominal
