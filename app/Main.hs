{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Codec.Picture
import Control.Monad (forM_, forever)
import Control.Monad.Identity (Identity(..),runIdentity)

import Data.Array.Repa as R
import Data.Array.Repa.IO.BMP (writeImageToBMP)

import Data.Function (on)

import Data.Reflection

import Data.Word (Word8)

import Linear

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Graphics (ffmpegWriter, repaToImage, FFmpegOpts(..), pngWriter)
import Pipes.Safe

import Numeric.AD
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse

import Text.Printf

import Lib
import Julia
import Sphere
import Type

main :: IO ()
main =
  do
    fileS <- readFile "camera"
    let camera = read fileS :: Camera Double
        V2 width height = _resolution camera
        j = defaultJulia
        aa = 2
        aaSq = fromIntegral $ aa*aa
        viewPlane = runIdentity $ buildViewPlane aa camera
    viewPlane `deepSeqArray` return ()
    let
      imageProducer :: MonadIO m => [Double] -> Producer' (Image PixelRGB8) m ()
      imageProducer l = forM_ l $ (\a -> let image'' = image' a in image'' `seq` return () >> (yield image'') >> liftIO (print "yielded"))
        where image' a =
                let
                  rot = axisAngle (V3 1 0 0) a
                  o = BoundingSphere (Sphere 4 $ V3 0 0 0) rot (j) :: BoundingSphere (Julia Double) Double
                in repaToImage $ runIdentity $
                   do
                     s <- sumP $ R.map (\(p,d) -> maybe 0 (max 0) . fmap (shade 10 28 0.02 j . _hitPos) $ intersects o (Ray p d)) viewPlane
                     --s <- sumP $ R.map (\(p,d) -> maybe 0 (const 1) $ intersects o (Ray p d)) viewPlane :: Identity (Array U DIM2 Double)
                     s `seq` (computeUnboxedP $ R.map (pure . min 255 . round . (*255) . (/aaSq)) s)
      consumerFFmpeg :: MonadSafe m => Consumer' (Image PixelRGB8) m ()
      consumerFFmpeg =
        ffmpegWriter $ FFmpegOpts width height 60 "/home/cdurham/Desktop/dog.mp4"

      consumerWriter :: forall m. MonadIO m => Consumer' (Image PixelRGB8) m ()
      consumerWriter = (await >>= yield) >-> pngWriter 3 "/home/cdurham/Desktop/" "dog"

      list = [0,0.01..2*pi]
    runSafeT $ runEffect $ imageProducer list >-> consumerFFmpeg --consumerWriter

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
