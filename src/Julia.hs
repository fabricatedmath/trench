{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Julia where

import Data.Reflection

import Linear
import Numeric.AD
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse

import Type

type Q = Quaternion

data Julia a =
  Julia
  { _juliaIters :: Int
  , _juliaBailout :: a
  , _juliaC :: Q a
  , _juliaFudge :: a
  , _juliaThresh :: a
  } deriving (Show, Read)

instance RealFloat a => Intersectable (Julia a) a where
  intersects = marchJulia 200

{-
normalOf
  :: RealFloat a
  => (forall s. Reifies s Tape => Julia (Reverse s a))
  -> V3 a
  -> V3 a
normalOf j p = grad (juliaDistance j) p
-}

testF :: Floating a => a -> V3 a -> a
testF a v = norm $ a*^v

defaultJulia :: Fractional a => Julia a
defaultJulia =
  Julia
  { _juliaIters = 200
  , _juliaBailout = 10000
  , _juliaC = Quaternion (-0.125) $ V3 (-0.256) 0.847 0
  , _juliaFudge = 0.8
  , _juliaThresh = 0.001
  }

initial
  :: Num a
  => V3 a
  -> (Q a, Q a)
initial (V3 x y z) = (q,dq)
  where q = Quaternion x $ V3 0 y z
        dq = Quaternion 1 $ V3 0 0 0

instance (Mode a, Num a) => Mode (V3 a) where
  type Scalar (V3 a) = V3 (Scalar a)
  auto (V3 x y z) = V3 (auto x) (auto y) (auto z)

instance (Mode a, RealFloat a, RealFloat (Scalar a)) => Mode (Quaternion a) where
  type Scalar (Quaternion a) = Quaternion (Scalar a)
  auto (Quaternion a v) = Quaternion (auto a) $ auto v

juliaDistance
  :: forall a. (Floating a, RealFloat a, Ord a)
  => Int
  -> a
  -> Quaternion a
  -> V3 a
  -> a
juliaDistance iters bailout c v =
  let qdqi@(!_qi,!_dqi) = initial v
      go :: Int -> (Q a, Q a) -> (Q a, Q a)
      go i qdq@(!q,!dq)
        | i == 0 || quadrance q > bailout = qdq
        | otherwise = go (i-1) (q',dq')
        where q' = q*q+c
              dq' = 2 * (q*dq)
      (!r,!dr) = (norm qf, norm dqf)
        where (!qf,!dqf) = go iters qdqi
  in 0.5 * r * log r / dr
{-# INLINABLE juliaDistance #-}

marchJulia
  :: RealFloat a
  => Int
  -> Julia a
  -> Ray a
  -> Maybe (Hit a)
marchJulia maxSteps j@(Julia iters bailout c f t) (Ray rp rd) =
  go maxSteps 0
  where
    go !i !d
      | i == 0 || d > 8 = Nothing
      | d' < t = Just $ Hit d rp'
      | otherwise = go (i-1) (d + d')
      where
        rp' = rp + d*^rd
        d' = f * juliaDistance iters bailout c rp'
{-# INLINABLE marchJulia #-}
