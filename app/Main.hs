{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Codec.Picture
import Control.Monad (forM_)
import Control.Monad.Identity (Identity(..),runIdentity)

import Data.Aeson
  (ToJSON, FromJSON, decode, toEncoding, defaultOptions, genericToEncoding)
import Data.Aeson.Encode.Pretty
import Data.Array.Repa as R hiding ((++))

import qualified Data.ByteString.Lazy.Char8 as C (putStrLn, readFile, writeFile)

import Linear

import GHC.Generics

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Graphics (ffmpegWriter, repaToImage, FFmpegOpts(..), pngWriter)
import Pipes.Safe

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Lib
import Julia
import Sphere
import Type

data Object a =
  JuliaBounding (BoundingSphere (Julia a) a)
  -- add more
  deriving (Generic, Show, Read)

instance ToJSON a => ToJSON (Object a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Object a)

data Raytracer a =
  Raytracer
  { _object :: Object a
  , _camera :: Camera a
  , _aa :: Int
  } deriving (Generic, Show, Read)

defaultRaytracer :: (Epsilon a, Floating a, Fractional a) => Raytracer a
defaultRaytracer =
  Raytracer
  { _camera = defaultCamera
  , _object =
      let
        rot = axisAngle (V3 1 0 0) 0
        j = defaultJulia
        s = (Sphere 4 $ V3 0 0 0)
        b = BoundingSphere s rot j
      in JuliaBounding b
  , _aa = 1
  }

instance ToJSON a => ToJSON (Raytracer a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Raytracer a)

getUsage :: IO String
getUsage =
  do
    prg <- getProgName
    return $ usageInfo prg options

data Options =
  Options
  { _optFile :: FilePath
  , _optVideo :: Bool
  , _optRaytracer :: Raytracer Double
  } deriving Show

startOptions :: Options
startOptions =
  Options
  { _optFile = "default"
  , _optVideo = False
  , _optRaytracer = defaultRaytracer
  }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "f" []
    (ReqArg
      (\arg opt -> return $ opt {_optFile = arg})
      "FilePath")
    "File to save image/video to"

  , Option "j" []
    (ReqArg
     (\arg opt ->
         do
           mrt <- decode <$> C.readFile arg
           case mrt of
             Nothing -> die "Failed to read json"
             Just rt -> return $ opt {_optRaytracer = rt})
     "FilePath")
    "File to read JSON from"

  , Option "d" []
    (NoArg
      (\_ ->
         do
           let bs = encodePretty (defaultRaytracer :: Raytracer Double)
           C.writeFile "default.json" bs
           putStrLn $ "Wrote " ++ show "default.json"
           exitWith ExitSuccess
      )
    ) $ "Write a default JSON file as " ++ show "default.json"

  , Option "v" []
    (NoArg
      (\opt -> pure $ opt {_optVideo = True})
    ) "Write a mpeg video"

  , Option "h" ["help"]
    (NoArg
      (\_ -> do
          prg <- getProgName
          hPutStrLn stderr $ usageInfo prg options
          exitWith ExitSuccess
      )
    ) "Show help"
  ]

main :: IO ()
main =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    let
      rt = _optRaytracer opts
      fp = _optFile opts
      camera = _camera rt
      aa = _aa rt
      JuliaBounding (BoundingSphere sphere' _rot j) = _object rt
      V2 width height = _resolution camera
      aaSq = fromIntegral $ aa*aa
      viewPlane = runIdentity $ buildViewPlane aa camera
    viewPlane `deepSeqArray` return ()
    let
      imageProducer :: MonadIO m => [Double] -> Producer' (Image PixelRGB8) m ()
      imageProducer l =
        forM_ l
        (\a ->
            do
              let image = genImage a
              image `seq` return ()
              yield image
              liftIO (print "yielded")
        )
        where
          genImage a =
            let
              rot = axisAngle (V3 1 0 0) a
              o = BoundingSphere sphere' rot j
            in repaToImage $ runIdentity $
               do
                 foldedAA <- sumP $ R.map
                   (\(p,d) ->
                      maybe 0 (max 0)
                      . fmap (shade 10 28 0.02 j . _hitPos) $
                      intersects o (Ray p d)
                   ) viewPlane
                 foldedAA `deepSeqArray`
                   (computeUnboxedP $
                     R.map (pure . min 255 . round . (*255) . (/aaSq)) foldedAA
                   )
      consumer :: MonadSafe m => Consumer' (Image PixelRGB8) m ()
      consumer = case _optVideo opts of
                   True -> consumerFFmpeg
                   False -> consumerWriter

      consumerFFmpeg :: MonadSafe m => Consumer' (Image PixelRGB8) m ()
      consumerFFmpeg =
        do
          let num = length list
          liftIO $ putStrLn $ "Writing video with " ++ show num ++ " frames"
          ffmpegWriter $ FFmpegOpts width height 60 $ fp ++ ".mp4"

      consumerWriter :: MonadIO m => Consumer' (Image PixelRGB8) m ()
      consumerWriter = P.take 1 >-> pngWriter 3 fp

      list = [0,0.01..2*pi]
    runSafeT $ runEffect $ imageProducer list >-> consumer
