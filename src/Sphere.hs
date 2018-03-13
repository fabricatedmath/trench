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

data BoundingSphere o a =
  BoundingSphere
  { _boundingSphere :: Sphere a
  , _boundingSphereRotation :: Quaternion a
  , _boundingSphereObject :: o
  } deriving (Show, Read)

boundingSphere
  :: Num a
  => Sphere a
  -> o
  -> BoundingSphere o a
boundingSphere s o = BoundingSphere s r o
  where r = Quaternion 1 0

makeLenses ''Sphere

instance (Floating a, Ord a) => Intersectable (Sphere a) a where
  intersects s (Ray p d) = uncurry Hit <$> rayAgainstSphere s (p,d)
  {-# INLINABLE intersects #-}

  intersects' s (Ray p d) = rayAgainstSphere' s (p,d)
  {-# INLINABLE intersects' #-}

instance (Conjugate a, Floating a, Intersectable o a, RealFloat a, Ord a) => Intersectable (BoundingSphere o a) a where
  intersects (BoundingSphere s rot o) r@(Ray _rp rd)  =
    intersects s r >>=
    (\h -> do
        let rp' = rotate rot $ _hitPos h
            rd' = rotate rot rd
        intersects o (Ray rp' rd')
    )
  {-# INLINABLE intersects #-}

  intersects' (BoundingSphere s rot o) r@(Ray _rp rd) =
    case intersects s r of
      Nothing -> False
      Just h ->
        let rp' = rotate rot $ _hitPos h
            rd' = rotate rot rd
        in
          intersects' o $ Ray rp' rd'
  {-# INLINABLE intersects' #-}

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
{-# INLINABLE rayAgainstSphere #-}

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
{-# INLINABLE rayAgainstSphere' #-}
