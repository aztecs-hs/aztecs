{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
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

import Control.Arrow (Arrow (..), returnA)
import Data.Aztecs
import qualified Data.Aztecs.Access as A
import Data.Aztecs.Asset (Asset (..), Handle, lookupAsset)
import qualified Data.Aztecs.Asset as Asset
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.SDL (Surface (..))
import qualified Data.Aztecs.System as S
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import SDL hiding (Surface, Texture, Window, windowTitle)
import qualified SDL.Font as F

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable (foldl')
#endif

newtype Font = Font {unFont :: F.Font}
  deriving (Eq, Show)

instance Asset Font where
  type AssetConfig Font = Int
  loadAsset fp size = Font <$> F.load fp size

data Text = Text {textContent :: !T.Text, textFont :: !(Handle Font)}
  deriving (Eq, Show)

instance Component Text

drawText :: T.Text -> Font -> IO Surface
drawText content f = do
  !s <- F.solid (unFont f) (V4 255 255 255 255) content
  return
    Surface
      { sdlSurface = s,
        surfaceBounds = Nothing
      }

-- | Setup SDL TrueType-Font (TTF) support.
setup :: System () ()
setup = const () <$> (Asset.setup @Font &&& S.task (const F.initialize))

-- | Load font assets.
load :: System () ()
load = Asset.loadAssets @Font

-- | Draw text components.
draw :: System () ()
draw = proc () -> do
  !texts <-
    S.all
      ( proc () -> do
          e <- Q.entity -< ()
          t <- Q.fetch -< ()
          s <- Q.fetchMaybe -< ()
          returnA -< (e, t, s)
      )
      -<
        ()
  !assetServer <- S.single Q.fetch -< ()
  let !textFonts =
        mapMaybe
          (\(eId, t, maybeSurface) -> (eId,textContent t,maybeSurface,) <$> lookupAsset (textFont t) assetServer)
          texts
  !draws <-
    S.task $
      mapM
        ( \(eId, content, maybeSurface, font) -> do
            case maybeSurface of
              Just lastSurface -> freeSurface $ sdlSurface lastSurface
              Nothing -> return ()
            surface <- drawText content font
            return (eId, surface)
        )
      -<
        textFonts
  S.queue . mapM_ $ uncurry A.insert -< draws
