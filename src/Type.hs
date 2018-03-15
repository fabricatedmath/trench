{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

module Type where

import Control.Lens

import Data.Array.Repa hiding ((*^))
import Data.Maybe (isJust)

import Linear

data Camera a =
  Camera
  { _sensorWidth :: a
  , _resolution :: V2 Int
  , _hfov :: a
  , _location :: V3 a
  , _lookingAt :: V3 a
  } deriving (Show, Read)

defaultCamera :: Camera Double
defaultCamera = Camera 2 (V2 1080 1920) 90 (V3 0 0 (-10)) (V3 0 0 0)

data Ray a =
  Ray
  { _rayPos :: !(V3 a)
  , _rayDir :: !(V3 a)
  } deriving (Show, Read)

makeLenses ''Ray

ray :: (Epsilon a, Floating a) => V3 a -> V3 a -> Ray a
ray p = Ray p . normalize

data Hit a =
  Hit
  { _hitDist :: a
  , _hitPos :: !(V3 a)
  } deriving (Show, Read)

makeLenses ''Hit

class Intersectable o a where
  intersects :: o -> Ray a -> Maybe (Hit a)

  intersects' :: o -> Ray a -> Bool
  intersects' o r = isJust $ intersects o r

class Normal o a where
  normalOf :: o a -> V3 a -> V3 a

class Shade o a where
  shade :: Int -> a -> a -> o a -> V3 a -> a

class ShapeVector a f where
  shToVec :: a -> f Int

instance ShapeVector DIM2 V2 where
  shToVec (Z :. y :. x) = V2 y x

instance ShapeVector DIM3 V3 where
  shToVec (Z :. z :. y :. x) = V3 z y x

instance ShapeVector DIM4 V4 where
  shToVec (Z :. z :. y :. x :. i) = V4 z y x i
