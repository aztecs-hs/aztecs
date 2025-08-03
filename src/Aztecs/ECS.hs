{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS
  ( module Aztecs.ECS.Class,
    module Aztecs.ECS.Commands,
    module Aztecs.ECS.Queryable,
    module Aztecs.ECS.Schedule,
    module Aztecs.ECS.Scheduler,
    PrimMonad (..),
    Query (..),
    runQuery,
    System (..),
    system,
  )
where

import Aztecs.ECS.Access.Internal
import qualified Aztecs.ECS.Access.Internal as A
import Aztecs.ECS.Class
import Aztecs.ECS.Commands
import Aztecs.ECS.Executor
import Aztecs.ECS.HSet (HSetT (..), Lookup (..))
import qualified Aztecs.ECS.HSet as HS
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.Schedule
import Aztecs.ECS.Scheduler
import qualified Aztecs.ECS.Scheduler as Scheduler
import Aztecs.ECS.System
import Aztecs.Entities
import qualified Aztecs.Entities as E
import Aztecs.World (ComponentStorage)
import qualified Aztecs.World as W
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.SparseSet.Strict.Mutable as MS
import Data.Word
import Prelude hiding (Read, lookup)
