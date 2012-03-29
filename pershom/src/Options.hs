-- See http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Options where

import System.Console.CmdArgs
import Data.Maybe
import Control.Monad

data Options = Options
    {
      maxScale :: Maybe Double,
      autoscale :: Maybe Double,
      divisions :: Maybe Int,
      topDimension :: Maybe Int,
      javaplex :: Bool,
      compact :: Bool,
      landmarks :: Maybe Int,
      landmarkProbability :: Maybe Double,
      input :: Maybe String,
      output :: Maybe String,
      quiet :: Bool,
      randomSeed :: Maybe Int
    } deriving (Data, Typeable, Eq)

data OutputStyle = Standard | Javaplex | Compact
                   deriving (Eq)

outputStyle :: Options -> OutputStyle
outputStyle opts = if (compact opts)
                   then Compact
                   else if (javaplex opts)
                        then Javaplex
                        else Standard

data ComplexType = MaxMinWitness | RandomWitness | VietorisRips
    deriving (Eq)

complexType :: Options -> ComplexType
complexType opts = if isJust (landmarks opts)
                   then MaxMinWitness
                   else if isJust (landmarkProbability opts)
                        then RandomWitness
                        else VietorisRips

useWitness :: Options -> Bool
useWitness = (/= VietorisRips) . complexType

useStdin :: Options -> Bool
useStdin opts = input opts == Just "-" || input opts == Nothing

useStdout :: Options -> Bool
useStdout opts = output opts == Just "-" || output opts == Nothing                

_NAME = "ph"
_ABOUT = "Persistent homology in Haskell."
_COPYRIGHT = "Copyright 2011 Gard Spreemann"
_HELP = ""

options :: Options
options = Options
          {
            maxScale = def &= explicit &= name "s" &= name "scale" &=
            help "Maximum scale in the 1-skeleton (also highest possible filtration time). Incompatible with -a.",
            
            autoscale = def &= explicit &= name "a" &= name "autoscale" &=
            help "Heuristically set the maximum scale. A supplied positive real number will be multiplied with the largest distance from the set of landmarks to the set of witnesses, and the result used as the maximum scale parameter. Only applicable to when using witness complexes. Incompatible with -s. [0.1]",
            
            divisions = def &= explicit &= name "N" &= name "divisions" &=
            help "The number of filtration times (from 0, up to and including the chosen maxium scale). [1000]",
            
            topDimension = def &= explicit &= name "d" &= name "dimension" &=
            help "The highest dimension of simplices to construct. Note that homology is computed only for dimensions below this. [3]",

            javaplex = def &= explicit &= name "j" &= name "javaplex" &=
            help "Output barcode in a style similar to that of JavaPlex. Note: The ordering of the bars need not be the same as that produced Javaplex. [false]",

            compact = def &= explicit &= name "c" &= name "compact" &=
            help "Output barcode in a style more easily parsed by other programs. [false]",

            landmarks = def &= explicit &= name "l" &= name "landmarks" &=
            help "Use max-min landmark selection with the specified number of landmarks. Incompatible with -p.",

            landmarkProbability = def &= explicit &= name "p" &= name "probability" &=
            help "Use random landmark selection with the specified probability. Incompatible with -l.",

            output = def &= explicit &= name "o" &= name "out" &=
            help "Output barcode to specified file. \"-\" signifies standard output. [-]",

            input = def &= explicit &= name "i" &= name "in" &=
            help "Input cloud from specified file. \"-\" signified standard input. [-]",

            quiet = def &= explicit &= name "q" &= name "quiet" &=
            help "Do not output anything during computation. [false]",

            randomSeed = def &= explicit &= name "S" &= name "seed" &=
            help "Set the random generator seed (e.g. for random landmark selection). If left out, the generator is seeded in a system-dependent way. []"
          }
            



getOpts :: IO Options
getOpts = cmdArgs $ options
          &= versionArg [explicit, name "version", name "v", summary _ABOUT]
          &= summary (_ABOUT ++ _COPYRIGHT)
          &= help _HELP
          &= helpArg [explicit, name "help", name "h"]
          &= program _NAME