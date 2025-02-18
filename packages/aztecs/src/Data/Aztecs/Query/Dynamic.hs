{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Aztecs.Query.Dynamic
  ( -- * Dynamic queries
    DynamicQuery (..),
    ArrowDynamicQueryReader (..),
    ArrowDynamicQuery (..),

    -- * Dynamic query filters
    DynamicQueryFilter (..),
  )
where

import Control.Arrow (Arrow (..))
import Control.Category (Category (..))
import Data.Aztecs.Entity (EntityID)
import Data.Aztecs.Query.Dynamic.Class (ArrowDynamicQuery (..))
import Data.Aztecs.Query.Dynamic.Reader (DynamicQueryFilter (..))
import Data.Aztecs.Query.Dynamic.Reader.Class (ArrowDynamicQueryReader (..))
import Data.Aztecs.World.Archetype (Archetype)
import qualified Data.Aztecs.World.Archetype as A
import Prelude hiding (all, any, id, lookup, map, mapM, reads, (.))

-- | Dynamic query for components by ID.
newtype DynamicQuery i o = DynamicQuery
  { dynQueryAll :: [i] -> [EntityID] -> Archetype -> ([o], Archetype)
  }

instance Functor (DynamicQuery i) where
  fmap f q =
    DynamicQuery $ \i es arch ->
      let (a, arch') = dynQueryAll q i es arch
       in (fmap f a, arch')

instance Applicative (DynamicQuery i) where
  pure a = DynamicQuery $ \_ es arch -> (take (length es) $ repeat a, arch)

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

instance ArrowDynamicQueryReader DynamicQuery where
  entityDyn = DynamicQuery $ \_ es arch -> (es, arch)

  fetchDyn cId =
    DynamicQuery $ \_ _ arch -> let !as = A.all cId arch in (fmap snd as, arch)

  fetchMaybeDyn cId =
    DynamicQuery $ \_ _ arch -> let as = A.allMaybe cId arch in (fmap snd as, arch)

instance ArrowDynamicQuery DynamicQuery where
  setDyn cId =
    DynamicQuery $ \is _ arch -> let !arch' = A.withAscList cId is arch in (is, arch')
