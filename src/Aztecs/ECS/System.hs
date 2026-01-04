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
  ( Monad m,
    System m sys,
    Access m (SystemIn m sys)
  ) =>
  sys ->
  m ()
system sys = access >>= runSystem sys
{-# INLINE system #-}
