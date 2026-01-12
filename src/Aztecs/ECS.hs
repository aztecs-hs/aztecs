{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS
  ( module Aztecs.ECS.Bundle,
    module Aztecs.ECS.Bundle.Class,
    module Aztecs.ECS.Class,
    module Aztecs.ECS.Commands,
    module Aztecs.ECS.Component,
    module Aztecs.ECS.Query.Class,
    module Aztecs.ECS.Schedule,
    module Aztecs.ECS.Scheduler,
    PrimMonad (..),
    Query (..),
    mapQueryM_,
    foldQueryM,
    lookupQuery,
    System (..),
    system,
  )
where

import Aztecs.ECS.Bundle
import Aztecs.ECS.Bundle.Class
import Aztecs.ECS.Class
import Aztecs.ECS.Commands
import Aztecs.ECS.Component
import Aztecs.ECS.Query
import Aztecs.ECS.Query.Class
import Aztecs.ECS.Schedule
import Aztecs.ECS.Scheduler
import Aztecs.ECS.System
import Control.Monad.Primitive
