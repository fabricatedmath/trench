{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Sphere where

import Linear
import Type

import Control.Lens

data Sphere a =
    Sphere
    { _sphereRadius :: !a
    , _sphereCenter :: !(V3 a)
    } deriving (Show, Read)

makeLenses ''Sphere

instance (Floating a, Ord a) => Intersectable (Sphere a) a where
  intersects s (Ray p d) = uncurry Hit <$> rayAgainstSphere s (p,d)
  intersects' s (Ray p d) = rayAgainstSphere' s (p,d)

sphere :: Num a => a -> V3 a -> Sphere a
sphere r c = Sphere r c

convertSphereHitToRadius
  :: (Epsilon a, Floating a)
  => Sphere a
  -> a
  -> V3 a
  -> V3 a
convertSphereHitToRadius (Sphere _r c) r h = V3 r r r * normalize (h - c) + c

rayAgainstSphere
  :: (Floating a, Ord a)
  => Sphere a
  -> (V3 a, V3 a)
  -> Maybe (a, V3 a)
rayAgainstSphere (Sphere r sc) (p,d)
    | (c > 0 && b > 0) || discr < 0 = Nothing
    | otherwise = Just (t', q)
    where
      m = p - sc
      b = m `dot` d
      c = m `dot` m - r * r
      discr = b*b - c
      t = -b - sqrt discr
      t' = if t < 0 then 0 else t
      q = p + t *^ d

rayAgainstSphere'
  :: (Num a, Ord a)
  => Sphere a
  -> (V3 a, V3 a)
  -> Bool
rayAgainstSphere' (Sphere r sc) (p,d)
    | c <= 0 = True
    | b > 0 || discr < 0 = False
    | otherwise = True
    where
      m = p - sc
      c = m `dot` m - r * r
      b = m `dot` d
      discr = b*b - c
