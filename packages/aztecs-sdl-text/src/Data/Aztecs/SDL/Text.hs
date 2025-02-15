{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

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
drawText content f = Draw $ \t renderer -> do
  surface <- F.solid (unFont f) (V4 255 255 255 255) content
  texture <- createTextureFromSurface renderer surface
  copy renderer texture Nothing Nothing
  destroyTexture texture
  freeSurface surface

setup :: System () ()
setup = const () <$> (Asset.setup @Font &&& S.run (const F.initialize))

load :: System () ()
load = Asset.loadAssets @Font

draw :: System () ()
draw = proc () -> do
  texts <- S.all $ Q.entity &&& Q.fetch -< ()
  assetServer <- S.single Q.fetch -< ()
  let textFonts = mapMaybe (\(eId, t) -> (eId,textContent t,) <$> lookupAsset (textFont t) assetServer) texts
      draws = map (\(eId, content, font) -> (eId, drawText content font)) textFonts
  S.queue . mapM_ $ uncurry A.insert -< draws
