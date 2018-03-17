{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Julia where

import Control.Arrow
import Control.Lens

import Data.Aeson

import GHC.Generics

import Linear

import Type

type Q = Quaternion

instance RealFloat a => Intersectable (Julia a) a where
  intersects = marchJulia
  {-# INLINABLE intersects #-}

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

--add consideration for sphere cord cut distance
marchJulia
  :: RealFloat a
  => Julia a
  -> Ray a
  -> Maybe (Hit a)
marchJulia (Julia iters bailout c f t maxSteps) (Ray rp rd) =
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

instance (Epsilon a, RealFloat a) => Normal Julia a where
  normalOf j = juliaAnalyticNormal (_juliaIters j) (_juliaBailout j) (_juliaC j)
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
        where (!q',!dq') = second (*dq) $ juliaFunc c q
      (!r,!dr) = (norm qf, norm dqf)
        where (!qf,!dqf) = go iters qdqi
  in 0.5 * r * log r / dr
{-# INLINABLE juliaDistance #-}

juliaFunc :: Num p => p -> p -> (p,p)
juliaFunc c q = (q*q+c,2*q)
{-# INLINABLE juliaFunc #-}

-- | Analytical Normal of Julia set (z^2 + c) over the Quaternions
--
-- Full credit goes to Inigo Quilez for derivation and original code at
--
-- https://www.shadertoy.com/view/MsfGRr
juliaAnalyticNormal
  :: forall a. (Epsilon a, RealFloat a)
  => Int
  -> a
  -> Quaternion a
  -> V3 a
  -> V3 a
juliaAnalyticNormal iters bailout c v =
  let
    iz = (Quaternion x' $ V3 y' z' 0)
      where (V3 x' y' z') = v
    idz0 = _e .~ 1 $ 0
    idz1 = _i .~ 1 $ 0
    idz2 = _j .~ 1 $ 0
    idz3 = _k .~ 1 $ 0

    f :: Int -> (Q a, Q a, Q a, Q a, Q a) -> V3 a
    f i (!z,!dz0,!dz1,!dz2,!dz3)
      | i == 0 || quadrance z > bailout =
          normalize $ V3 (z `dot` dz0) (z `dot` dz1) (z `dot` dz2)
      | otherwise =
        let
          mz = Quaternion (z ^. _e) $ negate $ z ^. _ijk
          dzmul dz =
            Quaternion (mz `dot` dz) $
            ((z ^. _e) *^ (dz ^. _ijk) + (dz ^. _e) *^ (z ^. _ijk))
          dz0' = dzmul dz0
          dz1' = dzmul dz1
          dz2' = dzmul dz2
          dz3' = dzmul dz3
          z' = z*z + c
        in f (i-1) (z',dz0',dz1',dz2',dz3')

  in
    f iters (iz,idz0,idz1,idz2,idz3)
{-# INLINABLE juliaAnalyticNormal #-}

instance (Epsilon a, RealFloat a, Fractional a) => Shade Julia a where
  shade aoParams j p =
    let
      num = _shaderNumSamples aoParams
      k = _shaderK aoParams
      del = _shaderDel aoParams
      n = normalOf j p
      df = juliaDistance (_juliaIters j) (_juliaBailout j) (_juliaC j)
      f i =
        let i' = fromIntegral i
        in recip (2^i) * (i'*del - df (p + (n^*(i'*del))))

    in 1 - k * sum (map f [1..num])
  {-# INLINABLE shade #-}
