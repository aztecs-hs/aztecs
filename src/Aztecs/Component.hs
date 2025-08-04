{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.Component where

import Data.Kind

class Component m a where
  type ComponentStorage (m :: Type -> Type) a :: Type -> Type
