{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Control.Lens
import Control.Monad.Identity (runIdentity)

import Data.Array.Repa hiding ((*^))
import Data.Function (on)
import Data.Maybe (isJust)
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as U

import Linear

import Type

buildViewPlane
  :: forall a. (Epsilon a, Floating a, Fractional a, Ord a, Unbox a)
  => Int -- aa
  -> Camera a
  -> Array U DIM3 (V3 a, V3 a)
buildViewPlane aa (Camera w res@(V2 resY resX) hfov loc _) =
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
        (V3 0 0 2, dir)
  in
    runIdentity $ computeUnboxedP $ fromFunction dim placeRay
