{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Julia.AD where

import Control.Arrow

import Linear

import Numeric.AD

import Type

type Q = Quaternion

instance (Mode a, Num a) => Mode (V3 a) where
  type Scalar (V3 a) = V3 (Scalar a)
  auto (V3 x y z) = V3 (auto x) (auto y) (auto z)

instance (Mode a, RealFloat a, RealFloat (Scalar a)) => Mode (Quaternion a) where
  type Scalar (Quaternion a) = Quaternion (Scalar a)
  auto (Quaternion a v) = Quaternion (auto a) $ auto v

instance RealFloat a => Intersectable (Julia a) a where
  intersects = marchJulia 200
  {-# INLINABLE intersects #-}

data Julia a =
  Julia
  { _juliaIters :: Int
  , _juliaBailout :: a
  , _juliaC :: Q a
  , _juliaFudge :: a
  , _juliaThresh :: a
  } deriving (Show, Read)

defaultJulia :: Fractional a => Julia a
defaultJulia =
  Julia
  { _juliaIters = 100
  , _juliaBailout = 10000
  , _juliaC = Quaternion (-0.125) $ V3 (-0.256) 0.847 0
  , _juliaFudge = 0.5
  , _juliaThresh = 0.01
  }

initial
  :: Num a
  => V3 a
  -> (Q a, Q a)
initial (V3 x y z) = (q,dq)
  where q = Quaternion x $ V3 y z 0
        dq = Quaternion 1 $ V3 0 0 0
{-# INLINABLE initial #-}

marchJulia
  :: RealFloat a
  => Int
  -> Julia a
  -> Ray a
  -> Maybe (Hit a)
marchJulia maxSteps (Julia iters bailout c f t) (Ray rp rd) =
  go maxSteps 0
  where
    go !i !d
      | i == 0 || d > 10 = Nothing
      | d' < t = Just $ Hit d rp'
      | otherwise = go (i-1) (d + d')
      where
        rp' = rp + d*^rd
        d' = f * juliaDistance iters bailout c rp'
{-# INLINABLE marchJulia #-}

juliaFunc :: Num p => p -> p -> (p,p)
juliaFunc c = diff' (\q -> q*q+auto c)
{-# INLINABLE juliaFunc #-}

juliaNormal
  :: RealFloat a
  => Int --iters
  -> a --bailout
  -> Quaternion a --c
  -> V3 a
  -> V3 a
juliaNormal iters bailout c =
  grad (juliaDistance iters (auto bailout) (auto c))
{-# INLINABLE juliaNormal #-}

instance RealFloat a => Normal Julia a where
  normalOf j = juliaNormal (_juliaIters j) (_juliaBailout j) (_juliaC j)
  {-# INLINABLE normalOf #-}

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
        where (q',dq') = second (*dq) $ juliaFunc c q
      (!r,!dr) = (norm qf, norm dqf)
        where (!qf,!dqf) = go iters qdqi
  in 0.5 * r * log r / dr
{-# INLINABLE juliaDistance #-}

instance (Epsilon a, RealFloat a, Fractional a) => Shade Julia a where
  shade num k del j p =
    let
      n = normalOf j p
      df = juliaDistance (_juliaIters j) (_juliaBailout j) (_juliaC j)
      f i =
        let i' = fromIntegral i
        in recip (2^i) * (i'*del - df (p + (n^*(i'*del))))

    in 1 - k * sum (map f [1..num])
  {-# INLINABLE shade #-}
