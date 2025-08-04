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

import Aztecs.ECS.Class
import Aztecs.ECS.Commands
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Schedule
import Aztecs.ECS.Scheduler
import Aztecs.ECS.System
import Control.Monad.Primitive
