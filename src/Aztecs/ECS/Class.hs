{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Aztecs.ECS.Class (ECS (..)) where

import Aztecs.ECS.Access.Internal hiding (access)
import Aztecs.ECS.HSet
import Aztecs.ECS.Query
import Aztecs.ECS.Queryable
import Aztecs.ECS.Queryable.Internal hiding (Components)
import Aztecs.ECS.System
import Data.Kind

class ECS m where
  type Entity m :: Type
  type Components m :: [Type]
  type Bundle m :: Type
  type Task m :: Type -> Type

  spawn :: Bundle m -> m (Entity m)

  insert :: Entity m -> Bundle m -> m ()

  remove :: Entity m -> m ()

  query :: (Queryable (Components m) (Task m) a) => m (Query (Task m) a)

  access ::
    ( Access (Components m) (Task m) a,
      ValidAccessInput (AccessType a),
      Subset (AccessToComponents (AccessType a)) (Components m)
    ) =>
    m a

  task :: (Task m) a -> m a

  runSystemWithWorld ::
    ( System (Task m) sys,
      Access (Components m) (Task m) (SystemInputs sys),
      Subset (AccessToComponents (AccessType (SystemInputs sys))) (Components m),
      ValidAccessInput (AccessType (SystemInputs sys)),
      Monad m
    ) =>
    sys ->
    m ()
  runSystemWithWorld sys = access >>= task . runSystem sys
