{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Julia.Monad where

--import Control.Monad (replicateM_)
import Control.Monad.Identity
--import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Linear

import Prelude hiding (break)

type Q = Quaternion

break :: Monad m => e -> ExceptT e m a
break = throwE

type Fractal q = FractalT q Identity

newtype FractalT q m a = FractalT (StateT q (ExceptT q m) a)
  deriving (Monad, Applicative, Functor)

execFractal :: Fractal s a -> s -> s
execFractal (FractalT m) s = either id id $ runIdentity $ runExceptT $ execStateT m s

runFractal :: Fractal a a -> a -> a
runFractal (FractalT m) s = either id id $ runIdentity $ runExceptT $ evalStateT m s

juliaFunc :: Num a => a -> (a,a) -> (a,a)
juliaFunc c (!q,!dq) = (q*q+c,2*q*dq)

initial
  :: Num a
  => V3 a
  -> (Q a, Q a)
initial (V3 x y z) = (q,dq)
  where q = Quaternion x $ V3 y z 0
        dq = Quaternion 1 $ V3 0 0 0
{-# INLINABLE initial #-}

{-
juliaDistance
  :: forall a. (Floating a, RealFloat a, Ord a)
  => Int
  -> a
  -> Q a
  -> V3 a
  -> a
juliaDistance iters bailout c v =
  let
    qdqi@(!_qi,!_dqi) = initial v
    m = replicateM_ iters $ julia bailout c
    (!qf,!dqf) = execFractal m qdqi
    (!r,!dr) = (norm qf, norm dqf)
  in 0.5 * r * log r / dr
-}

{-
--julia
--  :: RealFloat a
--  => a
--  -> Q a
--  -> Fractal (Q a, Q a) m ()
julia bailout c =
--  FractalT $
  do
    qdq@(!q,!_dq) <- get
    when (quadrance q > bailout) $ lift $ Left qdq
    put $ juliaFunc c qdq
-}
