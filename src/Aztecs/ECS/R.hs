{-# LANGUAGE DeriveFunctor #-}

module Aztecs.ECS.R (R (..)) where

-- | Read-only 'Queryable' component access.
newtype R a = R {unR :: a}
  deriving (Show, Eq, Functor)
