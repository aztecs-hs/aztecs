{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.System (System (..), system) where

import Aztecs.ECS.Class
import Aztecs.ECS.Query
import Aztecs.ECS.Query.Class
import Aztecs.ECS.W (Runner (..))
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

class (Monad m) => System m sys where
  type SystemIn m (s :: Type) sys :: Type

  -- | Run the system with a scoped query.
  -- The Runner monad ensures that W operations cannot escape the scope.
  runSystem :: sys -> (forall s. Query m s (SystemIn m s sys) -> Runner s m ())

  withSystemIn ::
    forall a.
    (Queryable m () (SystemIn m () sys)) =>
    sys ->
    (forall s. Query m s (SystemIn m s sys) -> Runner s m a) ->
    m a
  withSystemIn _ f = do
    q <- queryable @m @() @(SystemIn m () sys)
    unsafeRunRunner $ f (unsafeCoerce q)
  {-# INLINE withSystemIn #-}

system :: (System m sys, Queryable m () (SystemIn m () sys)) => sys -> m ()
system sys = withSystemIn sys (runSystem sys)
{-# INLINE system #-}
