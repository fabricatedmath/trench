{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Engine.Type where

import Control.Lens

import Data.Aeson
import Data.Array.Repa hiding ((*^), map)
import Data.Maybe (isJust)

import Linear

import GHC.Generics hiding (V1)

type Q = Quaternion

class DefaultParams o where
  defaultParams :: o

data AOParams a =
  AOParams
  { _shaderNumSamples :: Int
  , _shaderK :: a
  , _shaderDel :: a
  } deriving (Generic, Show, Read)

instance Fractional a => DefaultParams (AOParams a) where
  defaultParams = defaultAOParams
    where
      defaultAOParams :: Fractional a => AOParams a
      defaultAOParams =
        AOParams
        { _shaderNumSamples = 10
        , _shaderK = 5.1
        , _shaderDel = 0.1
        }

data Camera a =
  Camera
  { _sensorWidth :: a
  , _resolution :: V2 Int
  , _hfov :: a
  , _location :: V3 a
  , _lookingAt :: V3 a
  } deriving (Generic, Show, Read)

instance Fractional a => DefaultParams (Camera a) where
  defaultParams = defaultCamera
    where
      defaultCamera :: Fractional a => Camera a
      defaultCamera =
        Camera
        { _sensorWidth = 2
        , _resolution = V2 1000 1000
        , _hfov = 60
        , _location = V3 0 0 3
        , _lookingAt = V3 0 0 0
        }

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

type Accum a t = (t -> a -> t)

newtype Normal a =
  Normal
  { unNormal :: V3 a
  } deriving (Show, Read)

class DistanceFunction o a where
  distanceTo :: o -> V3 a -> a

class Intersectable o a where
  intersects :: o -> Ray a -> Maybe (Hit a)

  intersects' :: o -> Ray a -> Bool
  intersects' o r = isJust $ intersects o r

class Rotate o a where
  rotateObject :: o -> Q a -> o

class HasNormal o a where
  normalOf :: o a -> V3 a -> Normal a

class Shade o a where
  shade :: AOParams a -> o a -> V3 a -> a

class ShapeVector a f where
  shToVec :: a -> f Int

instance ShapeVector DIM2 V2 where
  shToVec (Z :. y :. x) = V2 y x

instance ShapeVector DIM3 V3 where
  shToVec (Z :. z :. y :. x) = V3 z y x

instance ShapeVector DIM4 V4 where
  shToVec (Z :. z :. y :. x :. i) = V4 z y x i

instance ToJSON a => ToJSON (AOParams a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (AOParams a)

instance ToJSON a => ToJSON (Camera a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Camera a)

instance ToJSON a => ToJSON (V1 a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (V1 a)

instance ToJSON a => ToJSON (V2 a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (V2 a)

instance ToJSON a => ToJSON (V3 a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (V3 a)

instance ToJSON a => ToJSON (V4 a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (V4 a)

instance ToJSON a => ToJSON (Quaternion a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Quaternion a)
