{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.ECS.Event
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.ECS.Event
  ( Event,
    OnInsert (..),
    OnChange (..),
    OnRemove (..),
  )
where

import Data.Typeable
import GHC.Generics

-- | An event in the ECS.
class (Typeable e) => Event e

-- | Event triggered when a component is inserted.
newtype OnInsert a = OnInsert {unOnInsert :: a}
  deriving (Show, Eq, Generic)

instance (Typeable a) => Event (OnInsert a)

-- | Event triggered when a component is changed.
newtype OnChange a = OnChange {unOnChange :: a}
  deriving (Show, Eq, Generic)

instance (Typeable a) => Event (OnChange a)

-- | Event triggered when a component is removed.
newtype OnRemove a = OnRemove {unOnRemove :: a}
  deriving (Show, Eq, Generic)

instance (Typeable a) => Event (OnRemove a)
