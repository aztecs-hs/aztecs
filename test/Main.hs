module Main (main) where

import Control.Monad.IO.Class
import Data.Aztecs
import qualified Data.Aztecs.Query as Q
import Data.Aztecs.System (runSystemOnce)
import qualified Data.Aztecs.System as S
import Data.Aztecs.World (newWorld)
import qualified Data.Aztecs.World as W
import Test.Hspec

newtype X = X Int deriving (Eq, Show)

instance Component X

newtype Y = Y Int deriving (Eq, Show)

instance Component Y

main :: IO ()
main = hspec $ do
  describe "Data.Aztecs.World.get" $ do
    it "gets components" $ do
      (e, w) <- W.spawn (X 1) newWorld
      w' <- W.insert e (Y 2) w

      x <- W.get e w'
      x `shouldBe` Just (X 1)

      y <- W.get e w'
      y `shouldBe` Just (Y 2)
  describe "Data.Aztecs.System.all" $ do
    it "queries all components" $ do
      (_, w) <- W.spawn (X 1) newWorld
      (_, w') <- W.spawn (X 2) w
      let s = do
            xs <- S.all Q.read
            liftIO $ xs `shouldBe` [X 1, X 2]
            return ()
      _ <- runSystemOnce s w'
      return ()
  describe "Data.Aztecs.System.get" $ do
    it "queries grouped components" $ do
      (e, w) <- W.spawn (X 1) newWorld
      w' <- W.insert e (Y 2) w

      let s = do
            xs <- S.get e $ do
              X x <- Q.read
              Y y <- Q.read
              return (X x, Y y)
            liftIO $ xs `shouldBe` Just (X 1, Y 2)
            return ()
      _ <- runSystemOnce s w'
      return ()
