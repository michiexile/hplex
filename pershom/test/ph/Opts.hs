-- Taken from http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Opts where

import System.Console.CmdArgs

data Options = Options
    {
      scale :: Double
    , divisions :: Int
    , dimension :: Maybe Int
    , javaplex :: Bool
    , outDimension :: Maybe Int
    } deriving (Data, Typeable, Eq)

data SanitizedOptions = SanitizedOptions
    {
      sanScale :: Double,
      sanDivisions :: Int,
      sanDimension :: Int,
      sanJavaplex :: Bool,
      sanOutAllDimensions :: Bool,
      sanOutDimension :: Int
    }

_PROGRAM_NAME = "testph"
_PROGRAM_OPTION = "0"
_PROGRAM_INFO = _PROGRAM_NAME
_PROGRAM_ABOUT = "Persistent homology in Haskell. Test program."
_COPYRIGHT = "(C) Gard Spreemann 2011"

options :: Options
options = Options
          {
            scale = def &= explicit &= name "s" &= name "scale" &= help "maximum scale of the Vietoris-Rips complex"
          , divisions = def &= explicit &= name "N" &= name "divisions" &= help "number of divisions [5]"
          , dimension = def &= explicit &= name "d" &= name "dimension" &= help "top dimension [2]"
          , outDimension = def &= explicit &= name "D" &= name "printdimension" &= help "dimension to output (leave out for all dimensions) []"
          , javaplex = def &= explicit &= name "j" &= name "javaplex" &= help "output barcode in a way compatible with javaPlex (note: the ordering of the simplices may still be different from that of javaPlex) [false]"
          }

getOpts :: IO Options
getOpts = cmdArgs $ options
          &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
          &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
          &= help _PROGRAM_ABOUT
          &= helpArg [explicit, name "help", name "h"]
          &= program _PROGRAM_NAME