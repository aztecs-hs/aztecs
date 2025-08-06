{-# LANGUAGE MultiParamTypeClasses #-}

module Aztecs.ECS.Bundle.Class (Bundleable (..)) where

import Aztecs.ECS.Bundle
import Aztecs.ECS.Class

class Bundleable c m where
  bundle :: c -> Bundle (Entity m) m
