{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Codec.Picture
import Control.Monad (forM_)
import Control.Monad.Identity (Identity(..),runIdentity)

import Data.Array.Repa as R hiding ((++))

import Linear

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Graphics (ffmpegWriter, repaToImage, FFmpegOpts(..), pngWriter)
import Pipes.Safe

import Config
import Engine
import Sphere
import Type

main :: IO ()
main =
  do
    opts <- loadConfigFromArgs
    let
      rt = _optRaytracer opts
      fp = _optFile opts
      aoParams = _aoParams rt
      camera = _camera rt
      aa = _aa rt
      JuliaBounding bs = _object rt
      viewPlane = runIdentity $ buildViewPlane aa camera
    viewPlane `deepSeqArray` return ()
    let
      imageProducer :: MonadIO m => [Double] -> Producer' (Image PixelRGB8) m ()
      imageProducer l =
        forM_ l
        (\a -> let image = genImage a in image `seq` yield image)
        where
          aaSq = fromIntegral $ aa*aa
          genImage a =
            let
              rot = axisAngle (V3 1 0 0) a
              o = rotateObject bs rot
            in repaToImage $ runIdentity $
               do
                 foldedAA <- sumP $ R.map
                   (\(p,d) ->
                      maybe 0 (max 0)
                      . fmap (shade aoParams o . _hitPos) $
                      intersects o (Ray p d)
                   ) viewPlane
                 foldedAA `deepSeqArray`
                   (computeUnboxedP $
                     R.map (pure . min 255 . round . (*255) . (/aaSq)) foldedAA
                   )
      consumer :: MonadSafe m => Consumer' (Image PixelRGB8) m ()
      consumer =
        case _optVideo opts of
          True ->
            do
              let
                num = length list
                V2 width height = _resolution camera
                notifier :: MonadIO m => Int -> Pipe a a m ()
                notifier i =
                  do
                    o <- await
                    yield o
                    liftIO $
                      putStrLn $
                      "Yielded " ++ show i ++ "/" ++ show num ++ " images"
                    notifier $ succ i
              liftIO $
                putStrLn $ "Writing video with " ++ show num ++ " frames"
              notifier 0 >->
                ffmpegWriter (FFmpegOpts width height 60 $ fp ++ ".mp4")
          False -> P.take 1 >-> pngWriter 3 fp
      list = [0,0.01..2*pi]
    runSafeT $ runEffect $ imageProducer list >-> consumer
