{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.System where

import Aztecs.ECS.Access.Internal
import Aztecs.ECS.Class

class System m sys where
  type SystemIn m sys

  runSystem :: sys -> SystemIn m sys -> m ()

system ::
  ( ECS m,
    Monad m,
    System (Task m) sys,
    Access m (SystemIn (Task m) sys)
  ) =>
  sys ->
  m ()
system sys = access >>= task . runSystem sys
{-# INLINE system #-}
