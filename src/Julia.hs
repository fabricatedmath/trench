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
{-# LANGUAGE ViewPatterns #-}

module Julia where

import Control.Lens
import Data.Aeson
import GHC.Generics

import Engine
import Engine.Type
import Julia.Type
import Linear

newtype JuliaAnalytic a =
  JuliaAnalytic
  { unJuliaAnalytic :: Julia a
  } deriving (Generic, Show, Read)

instance ToJSON a => ToJSON (JuliaAnalytic a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (JuliaAnalytic a)

instance Fractional a => DefaultParams (JuliaAnalytic a) where
  defaultParams = JuliaAnalytic defaultParams

instance (Epsilon a, RealFloat a) => HasNormal JuliaAnalytic a where
  normalOf (unJuliaAnalytic -> j) =
    Normal . juliaAnalyticNormal (_juliaIters j) (_juliaBailout j) (_juliaC j)
  {-# INLINABLE normalOf #-}

instance RealFloat a => DistanceFunction (JuliaAnalytic a) a where
  distanceTo (unJuliaAnalytic -> j) =
    juliaDistance (_juliaIters j) (_juliaBailout j) (_juliaC j)
  {-# INLINABLE distanceTo #-}

instance (Epsilon a, RealFloat a, Fractional a) => Shade JuliaAnalytic a where
  shade aoParams ja p = ambientOcclusion aoParams ja n p
    where n = normalOf ja p
  {-# INLINABLE shade #-}

instance RealFloat a => Intersectable (JuliaAnalytic a) a where
  intersects ja@(unJuliaAnalytic -> Julia _iters _bailout _c f t maxSteps) =
    marchDistance ja maxSteps 10 f t
  --TODO: add consideration for sphere cord cut distance
  {-# INLINABLE intersects #-}

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
        where (!q',!dq') = (q*q+c,2*q*dq)
      (!r,!dr) = (norm qf, norm dqf)
        where (!qf,!dqf) = go iters qdqi
  in 0.5 * r * log r / dr
{-# INLINABLE juliaDistance #-}

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
