{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.System.Queue
  ( -- * Systems
    QueueSystem (..),
    ArrowQueueSystem (..),
  )
where

import Aztecs.ECS.Access (Access)
import Aztecs.ECS.System.Queue.Class (ArrowQueueSystem (..))
import Control.Arrow (Arrow (..))
import Control.Category (Category (..))

-- | System to process entities.
newtype QueueSystem i o = QueueSystem {runQueueSystem :: i -> (o, Access ())}
  deriving (Functor)

instance Category QueueSystem where
  id = QueueSystem $ \i -> (i, pure ())
  QueueSystem f . QueueSystem g = QueueSystem $ \i ->
    let (b, access) = g i
        (c, access') = f b
     in (c, access >> access')

instance Arrow QueueSystem where
  arr f = QueueSystem $ \i -> (f i, pure ())
  first (QueueSystem f) = QueueSystem $ \(b, d) ->
    let (c, access) = f b in ((c, d), access)

instance ArrowQueueSystem QueueSystem where
  queue f = QueueSystem $ \i -> let !a = f i in ((), a)
