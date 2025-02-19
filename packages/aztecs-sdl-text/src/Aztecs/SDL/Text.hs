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

module Aztecs.SDL.Text
  ( Font (..),
    Text (..),
    drawText,
    setup,
    load,
    draw,
  )
where

import Aztecs.Asset (Asset (..), Handle, lookupAsset)
import qualified Aztecs.Asset as Asset
import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.SDL (Surface (..))
import Control.Arrow (returnA, (>>>))
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
setup :: Schedule IO () ()
setup = system (Asset.setup @Font) >>> task (const F.initialize)

-- | Load font assets.
load :: Schedule IO () ()
load = Asset.loadAssets @Font

-- | Draw text components.
draw :: Schedule IO () ()
draw = proc () -> do
  !texts <-
    reader $
      S.all
        ( proc () -> do
            e <- Q.entity -< ()
            t <- Q.fetch -< ()
            s <- Q.fetchMaybe -< ()
            returnA -< (e, t, s)
        )
      -<
        ()
  !assetServer <- reader $ S.single Q.fetch -< ()
  let !textFonts =
        mapMaybe
          (\(eId, t, maybeSurface) -> (eId,textContent t,maybeSurface,) <$> lookupAsset (textFont t) assetServer)
          texts
  !draws <-
    task $
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
  access . mapM_ $ uncurry A.insert -< draws
