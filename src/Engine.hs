{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Engine where

import Control.Lens

import Data.Array.Repa hiding ((*^), map)
import Data.Vector.Unboxed (Unbox)

import Linear

import Engine.Type

ambientOcclusion
  :: (Fractional a, DistanceFunction o a)
  => AOParams a
  -> o
  -> Normal a
  -> V3 a
  -> a
ambientOcclusion aoParams o (unNormal -> n) p =
  let
      num = _shaderNumSamples aoParams
      k = _shaderK aoParams
      del = _shaderDel aoParams
      df = distanceTo o
      f i =
        let i' = fromIntegral i
        in recip (2^i) * (i'*del - df (p + (n^*(i'*del))))
    in 1 - k * sum (map f [1..num])
{-# INLINABLE ambientOcclusion #-}

marchDistance
  :: (DistanceFunction o a, Eq a, Num a, Ord a)
  => o
  -> Int --max steps
  -> a --max radius
  -> a --fudge factor
  -> a --thresh
  -> Ray a
  -> Maybe (Hit a)
marchDistance o maxSteps r f t (Ray rp rd) =
  go maxSteps 0
  where
    go !i !d
      | t == 0 || d > r = Nothing
      | d' < t = Just $ Hit d rp'
      | otherwise = go (pred i) (d + d')
      where
        rp' = rp + d*^rd
        d' = f * distanceTo o rp'
{-# INLINABLE marchDistance #-}

buildViewPlane
  :: forall a m. (Epsilon a, Floating a, Fractional a, Ord a, Unbox a, Monad m)
  => Int -- aa
  -> Camera a
  -> m (Array U DIM3 (V3 a, V3 a))
buildViewPlane aa (Camera _w res@(V2 resY resX) hfov loc _) =
  let
    res'@(V2 resY' resX') = fromIntegral <$> res
    resInv = recip <$> res'
    hAspect = V2 (resY'/resX') 1
    aaSqInv = V2 aaSqInv' aaSqInv'
      where aaSqInv' = recip $ fromIntegral $ aa*aa
    dim = Z :. resY :. resX :. aa*aa
    theta = let a = tan(hfov / 2 * pi / 180) in V2 a a
    placeRay :: DIM3 -> (V3 a, V3 a)
    placeRay (shToVec -> (V3 y x i)) =
      let
        aaoffset = (*aaSqInv) $ fromIntegral <$> V2 (i `div` aa) (i `rem` aa)
        pixel = (aaoffset +) $ fromIntegral <$> V2 y x
        V2 pixelNDCy pixelNDCx = (pixel + 0.5) * resInv
        pixelScreen = V2 (1-2*pixelNDCy) (2*pixelNDCx-1)
        pixelCamera = pixelScreen * hAspect * theta
        dir = normalize $ _xy .~ pixelCamera $ (-1)
      in
        (loc, dir)
  in
    computeUnboxedP $ fromFunction dim placeRay

{-# INLINABLE buildViewPlane #-}
