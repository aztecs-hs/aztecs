{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Aztecs.SDL.Text
  ( Font (..),
    Text (..),
    drawText,
    setup,
    load,
    draw,
  )
where

import Control.Arrow (Arrow (..))
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (Asset (..), Handle, lookupAsset)
import qualified Data.Aztecs.Asset as Asset
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Draw (..))
import qualified Data.Aztecs.System as S
import Data.Aztecs.Transform (Transform (..))
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import SDL hiding (Texture, Window, windowTitle)
import qualified SDL.Font as F

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

newtype Font = Font {unFont :: F.Font}
  deriving (Eq, Show)

instance Asset Font where
  type AssetConfig Font = Int
  loadAsset fp size = Font <$> F.load fp size

data Text = Text {textContent :: T.Text, textFont :: Handle Font}
  deriving (Eq, Show)

instance Component Text

drawText :: T.Text -> Font -> Draw
drawText content f = Draw $ \transform renderer -> do
  surface <- F.solid (unFont f) (V4 255 255 255 255) content
  texture <- createTextureFromSurface renderer surface
  textureInfo <- queryTexture texture
  copyEx
    renderer
    texture
    Nothing
    ( Just
        ( Rectangle
            (fmap fromIntegral . P $ transformPosition transform)
            (V2 (textureWidth textureInfo) (textureHeight textureInfo))
        )
    )
    (realToFrac $ transformRotation transform)
    Nothing
    (V2 False False)
  destroyTexture texture
  freeSurface surface

-- | Setup SDL TrueType-Font (TTF) support.
setup :: System () ()
setup = const () <$> (Asset.setup @Font &&& S.task (const F.initialize))

-- | Load font assets.
load :: System () ()
load = Asset.loadAssets @Font

-- | Draw text components.
draw :: System () ()
draw = proc () -> do
  texts <- S.all $ Q.entity &&& Q.fetch -< ()
  assetServer <- S.single Q.fetch -< ()
  let textFonts =
        mapMaybe
          (\(eId, t) -> (eId,textContent t,) <$> lookupAsset (textFont t) assetServer)
          texts
      draws = map (\(eId, content, font) -> (eId, drawText content font)) textFonts
  S.queue . mapM_ $ uncurry A.insert -< draws
