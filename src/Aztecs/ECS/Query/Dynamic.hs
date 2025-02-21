{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Aztecs.ECS.Query.Dynamic
  ( -- * Dynamic queries
    DynamicQuery (..),
    ArrowDynamicQueryReader (..),
    ArrowDynamicQuery (..),
    fromDynReader,

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Aztecs.ECS.Entity (EntityID)
import Aztecs.ECS.Query.Dynamic.Class (ArrowDynamicQuery (..))
import Aztecs.ECS.Query.Dynamic.Reader (DynamicQueryFilter (..), DynamicQueryReader (..))
import Aztecs.ECS.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Aztecs.ECS.World.Archetype (Archetype)
import qualified Aztecs.ECS.World.Archetype as A
import Control.Arrow (Arrow (..), ArrowChoice (..))
import Control.Category (Category (..))
import Data.Either (partitionEithers)
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

-- | Dynamic query for components by ID.
newtype DynamicQuery i o
  = DynamicQuery {dynQueryAll :: [i] -> [EntityID] -> Archetype -> ([o], Archetype)}
  deriving (Functor)

instance Applicative (DynamicQuery i) where
  pure a = DynamicQuery $ \_ es arch -> (replicate (length es) a, arch)

  f <*> g =
    DynamicQuery
      { dynQueryAll = \i es arch ->
          let (as, arch') = dynQueryAll g i es arch
              (fs, arch'') = dynQueryAll f i es arch'
           in (zipWith ($) fs as, arch'')
      }

instance Category DynamicQuery where
  id = DynamicQuery $ \as _ arch -> (as, arch)

  f . g =
    DynamicQuery
      { dynQueryAll = \i es arch ->
          let (as, arch') = dynQueryAll g i es arch
           in dynQueryAll f as es arch'
      }

instance Arrow DynamicQuery where
  arr f = DynamicQuery $ \bs _ arch -> (fmap f bs, arch)
  first f =
    DynamicQuery
      { dynQueryAll = \bds es arch ->
          let (bs, ds) = unzip bds
              (cs, arch') = dynQueryAll f bs es arch
           in (zip cs ds, arch')
      }

instance ArrowChoice DynamicQuery where
  left f =
    DynamicQuery
      { dynQueryAll = \eds es arch ->
          let (es', ds) = partitionEithers eds
              (cs, arch') = dynQueryAll f es' es arch
           in (fmap Left cs ++ fmap Right ds, arch')
      }

instance ArrowDynamicQueryReader DynamicQuery where
  entityDyn = DynamicQuery $ \_ es arch -> (es, arch)

  fetchDyn cId =
    DynamicQuery $ \_ _ arch -> let !as = A.all cId arch in (fmap snd as, arch)

  fetchMaybeDyn cId =
    DynamicQuery $ \_ _ arch -> let as = A.allMaybe cId arch in (fmap snd as, arch)

instance ArrowDynamicQuery DynamicQuery where
  setDyn cId =
    DynamicQuery $ \is _ arch -> let !arch' = A.withAscList cId is arch in (is, arch')

fromDynReader :: DynamicQueryReader i o -> DynamicQuery i o
fromDynReader q = DynamicQuery $ \is es arch ->
  let os = dynQueryReaderAll q is es arch in (os, arch)
