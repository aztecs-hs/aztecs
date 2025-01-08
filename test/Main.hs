{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import qualified Data.Aztecs.World as W
import Test.Hspec
import Data.Aztecs.Component

newtype X = X Int deriving (Eq, Show)

instance Component X

newtype Y = Y Int deriving (Eq, Show)

instance Component Y

main :: IO ()
main = hspec $ do
  describe "Data.Aztecs.World.get" $ do
    it "fetches a component" $ do
      let (e, w) = W.spawn (X 0) (W.empty)
          (x, _) = Q.lookup e (Q.fetch @X) w
      x `shouldBe` Just (X 0)
    it "fetches a group of components" $ do
      let (e, w) = W.spawn (X 0) (W.empty)
          w' = W.insert e (Y 0) w
          (x, _) = Q.lookup e ((,) <$> Q.fetch @X <*> Q.fetch @Y) w'
      x `shouldBe` Just (X 0, Y 0)
