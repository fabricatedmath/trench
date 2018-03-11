module Main where

import Lib
import Julia
import Sphere
import Type

import Control.Monad.Identity (runIdentity)

import Data.Array.Repa as R
import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Data.Function (on)

import Data.Word (Word8)

import Linear

boolToColor :: Bool -> (Word8,Word8,Word8)
boolToColor True = (255,255,255)
boolToColor False = (0,0,0)

averageWord8 :: Word8 -> Word8 -> Word8
averageWord8 w1 w2 = fromIntegral w3
  where w3 = (`div` 2) $ ((+) `on` fromIntegral) w1 w2 :: Int

mergeColor :: (Word8,Word8,Word8) -> (Word8,Word8,Word8) -> (Word8,Word8,Word8)
mergeColor (a1,a2,a3) (b1,b2,b3) = (averageWord8 a1 b1,averageWord8 a2 b2,averageWord8 a3 b3)

main :: IO ()
main =
  do
    let camera = defaultCamera { _resolution = V2 1080 1080, _hfov = 70 }
    let viewPlane = buildViewPlane 1 camera :: Array U DIM3 (V3 Double, V3 Double)
    viewPlane `seq` return ()
    print "built"
    let j = defaultJulia :: Julia Double
    let image = runIdentity $ foldP mergeColor (0,0,0) $
                --R.map (\ray -> boolToColor $ any (\s -> rayAgainstSphere' s ray) spheres) viewPlane
                R.map (\(p,d) -> boolToColor $ intersects' j $ Ray p d) viewPlane
    writeImageToBMP "/home/cdurham/Desktop/sphere.bmp" image


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
