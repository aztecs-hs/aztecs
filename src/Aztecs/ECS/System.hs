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
import Aztecs.ECS.Class
import Aztecs.ECS.HSet
import Aztecs.ECS.Queryable.Internal

class System m sys where
  type SystemInputs m sys

  runSystem :: sys -> SystemInputs m sys -> m ()

instance (System m sys) => System m (Run constraints sys) where
  type SystemInputs m (Run constraints sys) = SystemInputs m sys
  runSystem (Run sys) = runSystem sys

system ::
  ( ECS m,
    Monad m,
    System (Task m) sys,
    Access m (SystemInputs (Task m) sys),
    ValidAccessInput (AccessType (SystemInputs (Task m) sys))
  ) =>
  sys ->
  m ()
system sys = access >>= task . runSystem sys
