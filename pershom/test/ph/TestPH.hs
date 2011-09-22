module Main where

import qualified VietorisRips as VR
import qualified Euclidean as Euc
import qualified Simplex as S
import qualified NeighborhoodGraph as NG
import qualified FilteredComplex as FC
import qualified PersistentHomology as PH
import qualified Barcode as BC
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

main :: IO ()
main = getArgs >>=
       \args -> (if null args
             then withArgs ["--help"] getOpts
             else getOpts)
            >>=
       sanitizer
             
sanitizer :: Options -> IO ()
sanitizer opts = when (scale opts <= 0) ( putStrLn "Specify a positive maximum scale with -s." >>
                                          exitWith (ExitFailure 1) ) >>
                 let
                     opts' = SanitizedOptions {
                       sanScale = scale opts,
                       sanDivisions = if divisions opts >= 0
                                   then divisions opts
                                   else 5,
                       sanDimension = if isJust (dimension opts)
                                      then fromJust (dimension opts)
                                      else 2,
                       sanOutAllDimensions = isNothing (outDimension opts),
                       sanOutDimension = if isJust (outDimension opts)
                                         then fromJust (outDimension opts)
                                         else -1,
                       sanJavaplex = javaplex opts
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
                    filt = VR.filtration vr (linspace 0 (sanScale opts) (sanDivisions opts))
                    hom = PH.rationalPersistentHomology filt
                    bc = PH.toBarcode hom (Nat.fromInt (sanDimension opts - 1))
                    bc' = BC.sort $ if sanOutAllDimensions opts
                                    then bc
                                    else bc `BC.dimension'` (sanOutDimension opts)
                in
                  if sanJavaplex opts
                  then putStrLn $ BC.javaPlexShow bc'
                  else print bc'


                  