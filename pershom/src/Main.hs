module Main where

import qualified Options as O
import System.Console.CmdArgs
import System.Environment
import System.IO
import Data.Maybe
import Control.Monad
import Misc.Misc
import System.Exit
import qualified System.Random as R

import qualified Math.Misc.Nat as Nat
import qualified Math.VectorSpaces.Euclidean as Euc
import qualified Math.Simplicial.LandmarkSelection as LS
import qualified Math.Simplicial.PreScale as PS
import qualified Math.Simplicial.NeighborhoodGraph as NG
import qualified Math.Simplicial.VietorisRipsExpansion as VRE
import qualified Math.Simplicial.FilteredComplex as FC
import qualified Math.PersistentHomology.PersistentHomology as PH
import qualified Math.PersistentHomology.BarcodeCollection as BCC


-- | I suspect this is horribly inefficient.
stringToEuclidean :: String -> Euc.Euclidean
stringToEuclidean = Euc.fromList . (map read) . words

inHandle :: O.Options -> IO Handle
inHandle opts = if O.useStdin opts
                   then return stdin
                   else openFile (fromJust (O.input opts)) ReadMode

outHandle :: O.Options -> IO Handle
outHandle opts = if O.useStdout opts
                   then return stdout
                   else openFile (fromJust (O.output opts)) WriteMode

validate :: O.Options -> IO ()
validate opts = when (isNothing (O.maxScale opts) && isNothing (O.autoscale opts) && not (O.useWitness opts))
                (
                 putStrLn "When not using the witness complex, you must specify a maximum scale for the 1-skeleton using the \"-s\" option." >>
                 exitWith (ExitFailure 1)
                ) >>
                when (isJust (O.landmarks opts) && isJust (O.landmarkProbability opts))
                (
                 putStrLn "Options -l and -p are incompatible." >>
                 exitWith (ExitFailure 1)
                ) >>
                when (isJust (O.topDimension opts) && fromJust (O.topDimension opts) < 1)
                (
                 putStrLn "Argument to -d option must be at least 1." >>
                 exitWith (ExitFailure 1)
                )

randomGenerator :: O.Options -> IO R.StdGen
randomGenerator opts = if isNothing (O.randomSeed opts)
                       then R.getStdGen
                       else return $ R.mkStdGen (fromJust (O.randomSeed opts))

main :: IO ()
main = getArgs >>= \args -> 
       (
        if null args
        then withArgs ["--help"] O.getOpts
        else O.getOpts
       ) >>= \opts ->
       validate opts >>
       randomGenerator opts >>= \rGen ->
       inHandle opts >>= hGetContents >>= \input ->
       let
           cloud = map stringToEuclidean (lines input)
           landmarkSelection = if O.complexType opts == O.MaxMinWitness
                               then LS.maxmin (fromJust (O.landmarks opts)) cloud
                               else if O.complexType opts == O.RandomWitness
                                    then fst $ LS.uniform rGen (fromJust (O.landmarkProbability opts)) cloud
                                    else undefined
           maxScale = if isJust (O.autoscale opts)
                      then PS.ConditionFactor $ fromJust (O.autoscale opts)
                      else PS.Absolute $ fromJust (O.maxScale opts)
           g = if O.useWitness opts
               then NG.lazyWitness landmarkSelection 2 maxScale
               else NG.exact cloud maxScale
           actualMaxScale = NG.scale g
           topDimension = if isJust (O.topDimension opts)
                          then fromJust (O.topDimension opts)
                          else 3
           flag = VRE.inductive topDimension g
           divisions = if isJust (O.divisions opts)
                      then fromJust (O.divisions opts)
                      else 1000
           filt = VRE.filtration flag (linspace 0 actualMaxScale divisions)
           hom = PH.rationalPersistentHomology filt
           bc = PH.toBarcode hom (Nat.fromInt (topDimension - 1))
           bc' = BCC.sort $ BCC.timeIndex (FC.filtrationTimes filt) bc
           quiet = O.quiet opts
       in
         unless quiet
         (
          putStrLn "Beginning calculations." >>
          when (O.useWitness opts)
          (
           putStrLn "Using witness complex." >>
           when (O.complexType opts == O.MaxMinWitness)
           (
            putStrLn "Max-min witness selection..."
           ) >>
           when (O.complexType opts == O.RandomWitness)
           (
            putStrLn "Random witness selection..."
           ) >>
           putStrLn ("Chose " ++ show (LS.numLandmarks landmarkSelection) ++ " landmarks and " ++ show (LS.numWitnesses landmarkSelection) ++ " witnesses.")
          ) >>
          unless (O.useWitness opts)
          (
           putStrLn "Using full Vietoris-Rips complex."
          ) >>
          putStrLn ("Using maximum scale " ++ show actualMaxScale ++ ".") >>
          putStrLn ("Total number of simplices in complex: " ++ show (FC.count filt) ++ ".")
         ) >>
         outHandle opts >>= \outH ->
         (
          case O.outputStyle opts of
            O.Standard -> hPrint outH bc'
            O.Javaplex -> hPutStrLn outH (BCC.javaPlexShow bc')
            O.Compact -> hPutStrLn outH ("MaxScale " ++ show actualMaxScale) >>
                         hPutStrLn outH (BCC.compactShow bc')
         ) >>
         hClose outH


