-- Taken from http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Opts where

import System.Console.CmdArgs

data Options = Options
    {
      scale :: Double
    , divisions :: Int
    , level :: Maybe Int
    , dimension :: Maybe Int
    , javaplex :: Bool
    , compact :: Bool
    } deriving (Data, Typeable, Eq)

data SanitizedOptions = SanitizedOptions
    {
      sanScale :: Double,
      sanDivisions :: Int,
      sanLevel :: Int,
      sanAllLevels :: Bool,
      sanDimension :: Int,
      sanJavaplex :: Bool,
      sanCompact :: Bool
    }

_PROGRAM_NAME = "testvr"
_PROGRAM_OPTION = "0"
_PROGRAM_INFO = _PROGRAM_NAME
_PROGRAM_ABOUT = "Persistent homology in Haskell. Test program for generation of Vietoris-Rips complexes."
_COPYRIGHT = "(C) Gard Spreemann 2011"

options :: Options
options = Options
          {
            scale = def &= explicit &= name "s" &= name "scale" &= help "maximum scale of the Vietoris-Rips complex"
          , divisions = def &= explicit &= name "N" &= name "divisions" &= help "number of divisions [5]"
          , level = def &= explicit &= name "l" &= name "level" &= help "filtration level to output, between 0 and divisions-1 (leave out for all levels) []"
          , dimension = def &= explicit &= name "d" &= name "dimension" &= help "top dimension [2]"
          , javaplex = def &= explicit &= name "j" &= name "javaplex" &= help "output simplices in a way compatible with javaPlex (note: the ordering of the simplices may still be different from that of javaPlex) [false]"
          , compact = def &= explicit &= name "c" &= name "compact" &= help "compact output well suited for post-processing [false]"
          }

getOpts :: IO Options
getOpts = cmdArgs $ options
          &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
          &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
          &= help _PROGRAM_ABOUT
          &= helpArg [explicit, name "help", name "h"]
          &= program _PROGRAM_NAME