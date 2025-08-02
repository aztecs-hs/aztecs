{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.System where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.HSet
import Aztecs.ECS.Queryable.Internal
import Aztecs.ECS.World
import Control.Monad.Primitive

class (PrimMonad m) => System m sys where
  type SystemInputs sys
  runSystem :: sys -> SystemInputs sys -> m ()

runSystemWithWorld ::
  ( System m sys,
    Access cs m (SystemInputs sys),
    Subset (AccessToComponents (AccessType (SystemInputs sys))) cs,
    ValidAccessInput (AccessType (SystemInputs sys))
  ) =>
  sys ->
  World m cs ->
  m ()
runSystemWithWorld sys world = runSystem sys (access world)
