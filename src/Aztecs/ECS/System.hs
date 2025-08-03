{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.System where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.Class
import Aztecs.ECS.HSet
import Aztecs.ECS.Queryable.Internal

class System m sys where
  type SystemIn m sys

  runSystem :: sys -> SystemIn m sys -> m ()

instance (System m sys) => System m (Run constraints sys) where
  type SystemIn m (Run constraints sys) = SystemIn m sys
  runSystem (Run sys) = runSystem sys

system ::
  ( ECS m,
    Monad m,
    System (Task m) sys,
    Access m (SystemIn (Task m) sys),
    ValidAccessInput (AccessType (SystemIn (Task m) sys))
  ) =>
  sys ->
  m ()
system sys = access >>= task . runSystem sys
