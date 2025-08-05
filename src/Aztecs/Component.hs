{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.Component where

import Aztecs.Entities (Entity)
import Data.Kind

-- | Component lifecycle hooks.
data Hooks m = Hooks
  { -- | Hook called when a component is inserted.
    onInsert :: Entity -> m (),
    -- | Hook called when a component is removed.
    onRemove :: Entity -> m ()
  }

instance (Monad m) => Semigroup (Hooks m) where
  h1 <> h2 =
    Hooks
      { onInsert = \e -> onInsert h1 e >> onInsert h2 e,
        onRemove = \e -> onRemove h1 e >> onRemove h2 e
      }

instance (Monad m) => Monoid (Hooks m) where
  mempty =
    Hooks
      { onInsert = \_ -> return (),
        onRemove = \_ -> return ()
      }

class (Monad m) => Component m a where
  type ComponentStorage (m :: Type -> Type) a :: Type -> Type

  -- | Component lifecycle `Hooks`.
  componentHooks :: proxy a -> Hooks m
  componentHooks _ = mempty
