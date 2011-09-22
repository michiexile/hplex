module Main where

import qualified VietorisRips as VR
import qualified Euclidean as Euc
import qualified Simplex as S
import qualified NeighborhoodGraph as NG
import qualified FilteredComplex as FC
import SimpleTypes
import Cloud
import Misc
import Data.List
import Data.Maybe
import Control.Monad
import System.Environment
import System.Exit
import Opts
import qualified Nat as Nat

stringToEuclidean :: String -> Euc.Euclidean
stringToEuclidean = Euc.fromList . (map read) . words

euclideanToString :: Euc.Euclidean -> String
euclideanToString = concat . (intersperse " ") . (map show) . Euc.toList

simplexToString :: (Show a) => S.Simplex a -> String
simplexToString = concat . (intersperse " ") . (map show) . S.toList

simplexToJavaPlexString :: S.Simplex Int -> String
simplexToJavaPlexString = show . S.toList

main :: IO ()
main = getArgs >>=
       \args -> (if null args
             then withArgs ["--help"] getOpts
             else getOpts)
            >>=
       sanitizer
             
sanitizer :: Options -> IO ()
sanitizer opts = when (divisions opts <= 0) ( putStrLn "Specify a positive number of divisions with -N." >> 
                                              exitWith (ExitFailure 1) ) >>
                 when (scale opts <= 0) ( putStrLn "Specify a positive maximum scale with -s." >>
                                             exitWith (ExitFailure 1) ) >>
                 let
                     opts' = SanitizedOptions {
                       sanScale = scale opts,
                       sanDivisions = if divisions opts >= 0
                                   then divisions opts
                                   else 5,
                       sanLevel = if isJust (level opts)
                               then fromJust (level opts)
                               else -1,
                       sanAllLevels = isNothing (level opts),
                       sanDimension = if isJust (dimension opts)
                                      then fromJust (dimension opts)
                                      else 2,
                       sanJavaplex = javaplex opts,
                       sanCompact = compact opts
                       }
                 in
                 exec opts'
             
exec :: SanitizedOptions -> IO ()
exec opts = getContents >>=
            \input ->
                let
                    cloud = map stringToEuclidean (lines input)
                    g = NG.exact cloud (sanScale opts)
                    vr = VR.inductive (sanDimension opts) g
                    filt = VR.filtration'' vr (linspace 0 (sanScale opts) (sanDivisions opts))
                    simps = concat $ VR.simplices vr
                    simplexPrinter = if sanCompact opts
                                     then Just simplexToString
                                     else if sanJavaplex opts
                                          then Just simplexToJavaPlexString
                                          else Nothing
                in
                  if isJust simplexPrinter
                  then if sanAllLevels opts
                       then putStrLn $ unlines' (map (fromJust simplexPrinter) (FC.allSimplices filt))
                       else putStrLn $ unlines' (map (fromJust simplexPrinter) (FC.degree' filt (sanLevel opts)))
                  else if sanAllLevels opts
                       then print filt
                       else putStrLn $ FC.showDegree filt (sanLevel opts)
                  