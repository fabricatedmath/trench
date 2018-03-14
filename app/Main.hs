{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lib
import Julia
import Sphere
import Type

import Codec.Picture
import Control.Monad (forM_)
import Control.Monad.Identity (runIdentity)

import Data.Array.Repa as R
import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Data.Function (on)

import Data.Reflection

import Data.Word (Word8)

import Linear

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Graphics
import Pipes.Safe

import Numeric.AD
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse

boolToColor :: Bool -> V3 Word8
boolToColor True = 255
boolToColor False = 0

mergeColor :: V3 Word8 -> V3 Word8 -> V3 Word8
mergeColor v1 v2 =
  let
    v1' = fromIntegral <$> v1 :: V3 Int
    v2' = fromIntegral <$> v2 :: V3 Int
  in fromIntegral . (`div` 2) <$> (v1' + v2')

main :: IO ()
main =
  do
    fileS <- readFile "camera"
    --print fileS
    --let camera = defaultCamera { _resolution = V2 1080 1080, _hfov = 70 }
    let camera = read fileS :: Camera Double
        V2 width height = _resolution camera
        --let viewPlane =
          --buildViewPlane 1 camera :: Array U DIM3 (V3 Double, V3 Double)
    --viewPlane `seq` return ()
    print "built"
    let j = defaultJulia
--        j' = defaultJulia :: (forall s. Reifies s Tape => Julia (Reverse s Double))
        --r = axisAngle (V3 1 0 0) 0 --(pi/2)
        --o = BoundingSphere (Sphere 4 $ V3 0 0 0) r (j) :: BoundingSphere (Julia Double) Double
        viewPlane = runIdentity $ buildViewPlane 1 camera
    print $ grad (juliaDistance (_juliaIters j) (auto $ _juliaBailout j) (auto $ _juliaC j)) $ V3 2 2 2
    --print $ Julia.normalOf defaultJulia $ V3 2 2 2
    viewPlane `deepSeqArray` return ()
    let
      image :: MonadIO m => Producer' (Image PixelRGB8) m ()
      image = forM_ [0,0.05..10*pi] $ (\a -> (yield . image' $ a) >> liftIO (print "yielded"))
        where image' a =
                let
                  rot = axisAngle (V3 1 0 0) a
                  o = BoundingSphere (Sphere 4 $ V3 0 0 0) rot (j) :: BoundingSphere (Julia Double) Double
                  in repaToImage $ runIdentity $
                     do
--                       viewPlane <- buildViewPlane 1 camera
                       foldP mergeColor 0 $ R.map (\(p,d) -> boolToColor $ intersects' o $ Ray p d) viewPlane
    --let image = runIdentity $ foldP mergeColor (0,0,0) $
                --R.map (\ray -> boolToColor $ any (\s -> rayAgainstSphere' s ray) spheres) viewPlane
--              R.map (\(p,d) -> boolToColor $ intersects' o $ Ray p d) viewPlane
    --writeImageToBMP "sphere.bmp" image
      consumer = ffmpegConsumer $ FFmpegOpts width height 60 "/home/cdurham/Desktop/dog.mp4"
    runSafeT $ runEffect $ image >-> consumer
    print "dog"


spheres :: [Sphere Double]
spheres = [sphere 1 $ V3 y x z | y <- [-30,-27..30], x <- [-30,-27..30],  z <- [-30]] -- [ (sphere 1 $ V3 0 0 (-300))
          -- , (sphere 1 $ V3 0 3 (-300))
          -- , (sphere 1 $ V3 0 6 (-300))
          -- , (sphere 1 $ V3 0 9 (-300))
          -- , (sphere 1 $ V3 0 12 (-300))
          -- , (sphere 1 $ V3 0 15 (-300))
          -- , (sphere 1 $ V3 0 18 (-300))
          -- , (sphere 1 $ V3 0 21 (-300))
          -- , (sphere 1 $ V3 0 24 (-300))
          -- , (sphere 1 $ V3 0 27 (-300))
          -- ]
