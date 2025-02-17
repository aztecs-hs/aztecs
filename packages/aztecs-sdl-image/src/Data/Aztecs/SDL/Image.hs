{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.SDL.Image
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

import Control.Arrow (Arrow (..), (>>>))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (Asset (..), AssetServer, Handle, lookupAsset)
import qualified Data.Aztecs.Asset as Asset
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Surface (..), Time (..))
import qualified Data.Aztecs.System as S
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import SDL hiding (Surface, Texture, Window, windowTitle)
import qualified SDL
import qualified SDL.Image as IMG

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

setup :: System () ()
setup = Asset.setup @Texture

load :: System () ()
load = Asset.loadAssets @Texture

draw :: System () ()
draw = const () <$> (drawImages &&& (animateSprites >>> drawSprites))

-- | Texture asset.
newtype Texture = Texture {textureSurface :: SDL.Surface}

instance Asset Texture where
  type AssetConfig Texture = ()
  loadAsset path _ = Texture <$> IMG.load path

-- | Image component.
data Image = Image
  { imageTexture :: !(Handle Texture),
    imageSize :: !(V2 Int)
  }
  deriving (Show)

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
    go (texture, _, eId) = do
      A.insert
        eId
        Surface
          { sdlSurface = textureSurface texture,
            surfaceBounds = Nothing
          }

-- | Sprite component.
data Sprite = Sprite
  { spriteTexture :: !(Handle Texture),
    spriteBounds :: !(Maybe (Rectangle Int)),
    spriteSize :: !(V2 Int)
  }
  deriving (Show)

instance Component Sprite

-- | Draw images to their target windows.
drawSprites :: System () ()
drawSprites = proc () -> do
  sprites <- S.all $ Q.entity &&& Q.fetch -< ()
  assets <- S.single (Q.fetch @_ @(AssetServer Texture)) -< ()
  let loadedAssets =
        mapMaybe (\(eId, sprite) -> (,sprite,eId) <$> lookupAsset (spriteTexture sprite) assets) sprites
  S.queue (mapM_ go) -< loadedAssets
  where
    go (texture, sprite, eId) = do
      A.insert
        eId
        Surface
          { sdlSurface = textureSurface texture,
            surfaceBounds = spriteBounds sprite
          }

data SpriteAnimation = SpriteAnimation
  { spriteAnimationSteps :: ![Rectangle Int],
    spriteAnimationIndex :: !Int,
    spriteAnimationMS :: !Word32,
    spriteAnimationStart :: !Word32
  }

instance Component SpriteAnimation

spriteAnimation :: SpriteAnimation
spriteAnimation =
  SpriteAnimation
    { spriteAnimationSteps = [],
      spriteAnimationIndex = 0,
      spriteAnimationMS = 100,
      spriteAnimationStart = 0
    }

-- | Create a sprite animation from a grid of sprites,
-- given the grid's offset, size, and number of tiles.
spriteAnimationGrid :: V2 Int -> V2 Int -> Int -> SpriteAnimation
spriteAnimationGrid (V2 x y) (V2 w h) n =
  spriteAnimation
    { spriteAnimationSteps =
        map (\i -> Rectangle (P $ V2 (x + i * w) y) (V2 w h)) [0 .. n]
    }

-- | Query to animate sprites based on the inputted `Time`.
animateSpritesQuery :: (Monad m) => Query m Time SpriteAnimation
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
