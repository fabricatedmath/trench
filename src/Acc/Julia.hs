{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Acc.Julia where

import Numeric.AD
import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.Linear as A

import Prelude as P
import Linear as P

type Q = Quaternion
--arr :: Array DIM2 Float
--arr = A.fromList (Z :. 100 :. 100 :: DIM2) ([V3 ]

--f :: P.Floating a => a -> a
--f = signum
{-
h :: (Elt a, A.Floating a) => Exp (V3 a) -> Exp a
h = A.norm

g = run1 (A.foldAll (+) 0 . A.map (\y -> (\([x1,y1,z1]) -> lift $ V3 x1 y1 z1) $ grad (\[x,z,w] -> P.norm $ V3 x z w) [y,0,0])) $ arr
-}

--g1 = run1 (A.map (\y -> grad (\(V3 x y z) -> h (lift $ V3 x y z)) (V3 y y y))) $ arr

juliaDistance
  :: forall a. (P.Floating a, P.RealFloat a, P.Ord a)
  => Int
  -> a
  -> Quaternion a
  -> V3 a
  -> a
juliaDistance iters bailout c v =
  let qdqi@(!_qi,!_dqi) = initial v
      go :: Int -> (Q a, Q a) -> (Q a, Q a)
      go i qdq@(!q,!dq)
        | i P.== 0 = qdq
        | otherwise = go (i-1) (q',dq')
        where q' = fmap (b*) q + fmap (notb*) (q*q+c)
              dq' = fmap (b*) dq + fmap (notb*) (2 * (q*dq))
              b = signum $ P.max 0 $ (P.quadrance q - bailout)
              notb = abs $ b - 1
      (!r,!dr) = (P.norm qf, P.norm dqf)
        where (!qf,!dqf) = go iters qdqi
  in 0.5 * r * log r / dr

{-# INLINABLE juliaDistance #-}

initial
  :: P.Num a
  => V3 a
  -> (Q a, Q a)
initial (V3 x y z) = (q,dq)
  where q = Quaternion x $ V3 0 y z
        dq = Quaternion 1 $ V3 0 0 0
