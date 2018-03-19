{-# LANGUAGE DeriveGeneric #-}
module Config
  ( loadConfigFromArgs
  , Object(..)
  , Options(..)
  , Raytracer(..)
  ) where

import Data.Aeson
  (ToJSON, FromJSON, decode, toEncoding, defaultOptions, genericToEncoding)
import Data.Aeson.Encode.Pretty

import qualified Data.ByteString.Lazy.Char8 as C (readFile, writeFile)

import GHC.Generics

import Linear

import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit
import System.IO

import Engine.Type
import Julia
import Sphere

data Object a =
  JuliaBounding (BoundingSphere (JuliaAnalytic a) a)
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
  , _aoParams :: AOParams a
  } deriving (Generic, Show, Read)

instance (Epsilon a, Floating a, Fractional a) =>
  DefaultParams (Raytracer a) where
  defaultParams = defaultRaytracer
    where
      defaultRaytracer
        :: (Epsilon a, Floating a, Fractional a) => Raytracer a
      defaultRaytracer =
        Raytracer
        { _camera = defaultParams
        , _object =
          let
            rot = axisAngle (V3 1 0 0) 0
            j = defaultParams
            s = (Sphere 4 $ V3 0 0 0)
            b = BoundingSphere s rot j
          in JuliaBounding b
        , _aa = 1
        , _aoParams = defaultParams
        }

instance ToJSON a => ToJSON (Raytracer a) where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON a => FromJSON (Raytracer a)

loadConfigFromArgs :: IO Options
loadConfigFromArgs =
  do
    args <- getArgs
    let (actions,_,_) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return startOptions) actions
    return opts

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
  , _optRaytracer = defaultParams
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
           let bs = encodePretty (defaultParams :: Raytracer Double)
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
