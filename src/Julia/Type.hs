{-# LANGUAGE DeriveGeneric #-}

module Julia.Type where

import Data.Aeson

import GHC.Generics

import Linear

import Engine.Type

data Julia a =
  Julia
  { _juliaIters :: Int
  , _juliaBailout :: a
  , _juliaC :: Q a
  , _juliaFudge :: a
  , _juliaThresh :: a
  , _juliaMarchIter :: Int
  } deriving (Generic, Show, Read)

instance ToJSON a => ToJSON (Julia a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Julia a)

instance Fractional a => DefaultParams (Julia a) where
  defaultParams = defaultJulia
    where
      defaultJulia :: Fractional a => Julia a
      defaultJulia =
        Julia
        { _juliaIters = 100
        , _juliaBailout = 10000
        , _juliaC = Quaternion (-0.125) $ V3 (-0.256) 0.847 0
        , _juliaFudge = 0.75
        , _juliaThresh = 0.001
        , _juliaMarchIter = 200
        }

initial
  :: Num a
  => V3 a
  -> (Q a, Q a)
initial (V3 x y z) = (q,dq)
  where q = Quaternion x $ V3 y z 0
        dq = Quaternion 1 $ V3 0 0 0
{-# INLINABLE initial #-}
