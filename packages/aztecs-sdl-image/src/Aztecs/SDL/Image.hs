{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Aztecs.SDL.Image
  ( -- * Components
    Texture (..),
    Image (..),
    Sprite (..),
    SpriteAnimation (..),
    spriteAnimation,
    spriteAnimationGrid,

    -- * Systems
    setup,
    load,
    draw,

    -- ** Primitive systems
    drawImages,
    drawSprites,
    animateSprites,
    animateSpritesQuery,
  )
where

import Aztecs.Asset (Asset (..), AssetServer, Handle, lookupAsset)
import qualified Aztecs.Asset as Asset
import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.SDL (Surface (..))
import Aztecs.Time
import Control.Arrow (Arrow (..), (>>>))
import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import GHC.Generics (Generic)
import SDL hiding (Surface, Texture, Window, windowTitle)
import qualified SDL
import qualified SDL.Image as IMG

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

setup :: System () ()
setup = Asset.setup @Texture

load :: (MonadIO m) => Schedule m () ()
load = Asset.loadAssets @Texture

draw :: System () ()
draw = const () <$> (drawImages &&& (animateSprites >>> drawSprites))

-- | Texture asset.
newtype Texture = Texture {textureSurface :: SDL.Surface}

instance Asset Texture where
  type AssetConfig Texture = ()
  loadAsset path _ = Texture <$> IMG.load path

-- | Image component.
newtype Image = Image {imageTexture :: Handle Texture}
  deriving (Generic)
  deriving newtype (Show, NFData)

instance Component Image

-- | Draw images to their target windows.
drawImages :: System () ()
drawImages = proc () -> do
  imgs <- S.filter (Q.entity &&& Q.fetch @_ @Image) (without @Surface) -< ()
  assets <- S.single (Q.fetch @_ @(AssetServer Texture)) -< ()
  let newAssets =
        mapMaybe (\(eId, img) -> (,img,eId) <$> lookupAsset (imageTexture img) assets) imgs
  S.queue (mapM_ go) -< newAssets
  where
    go (texture, _, eId) =
      A.insert eId Surface {sdlSurface = textureSurface texture, surfaceBounds = Nothing}

-- | Sprite component.
data Sprite = Sprite
  { spriteTexture :: !(Handle Texture),
    spriteBounds :: !(Maybe (Rectangle Int))
  }
  deriving (Show)

instance Component Sprite

instance NFData Sprite where
  rnf (Sprite texture bounds) = (fmap (fmap rnf) bounds) `seq` rnf texture

-- | Draw images to their target windows.
drawSprites :: System () ()
drawSprites = proc () -> do
  sprites <- S.all $ Q.entity &&& Q.fetch -< ()
  assets <- S.single (Q.fetch @_ @(AssetServer Texture)) -< ()
  let loadedAssets =
        mapMaybe (\(eId, sprite) -> (,sprite,eId) <$> lookupAsset (spriteTexture sprite) assets) sprites
  S.queue (mapM_ go) -< loadedAssets
  where
    go (texture, sprite, eId) =
      A.insert eId Surface {sdlSurface = textureSurface texture, surfaceBounds = spriteBounds sprite}

-- | Sprite animation component.
data SpriteAnimation = SpriteAnimation
  { spriteAnimationSteps :: ![Rectangle Int],
    spriteAnimationIndex :: !Int,
    spriteAnimationMS :: !Word32,
    spriteAnimationStart :: !Word32
  }
  deriving (Generic)

instance Component SpriteAnimation

instance NFData SpriteAnimation where
  rnf (SpriteAnimation steps index ms start) = (fmap (fmap rnf) steps) `seq` rnf index `seq` rnf ms `seq` rnf start

spriteAnimation :: SpriteAnimation
spriteAnimation =
  SpriteAnimation
    { spriteAnimationSteps = [],
      spriteAnimationIndex = 0,
      spriteAnimationMS = 100,
      spriteAnimationStart = 0
    }

-- | Create a sprite animation from a grid of sprites,
-- given the grid's tile size, and a list of tile indices.
spriteAnimationGrid :: V2 Int -> [V2 Int] -> SpriteAnimation
spriteAnimationGrid (V2 w h) tiles =
  spriteAnimation
    { spriteAnimationSteps =
        map (\(V2 x y) -> Rectangle (P $ V2 (x * w) (y * h)) (V2 w h)) tiles
    }

-- | Query to animate sprites based on the inputted `Time`.
animateSpritesQuery :: Query Time SpriteAnimation
animateSpritesQuery = proc currentTime -> do
  sprite <- Q.fetch @_ @Sprite -< ()
  animation <- Q.fetch @_ @SpriteAnimation -< ()
  let sprite' = sprite {spriteBounds = Just $ spriteAnimationSteps animation !! spriteAnimationIndex animation}
      animation' =
        if elapsedMS currentTime - spriteAnimationStart animation > spriteAnimationMS animation
          then
            animation
              { spriteAnimationIndex =
                  (spriteAnimationIndex animation + 1)
                    `mod` length (spriteAnimationSteps animation),
                spriteAnimationStart = elapsedMS currentTime
              }
          else animation
  Q.set -< sprite'
  Q.set -< animation'

-- | Animate sprites based on the current `Time`.
animateSprites :: System () ()
animateSprites = proc () -> do
  currentTime <- S.single (Q.fetch @_ @Time) -< ()
  S.map_ animateSpritesQuery -< currentTime
