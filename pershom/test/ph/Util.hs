module Util where

import qualified Euclidean as Euc
import qualified Simplex as S

stringToEuclidean :: String -> Euc.Euclidean
stringToEuclidean = Euc.fromList . (map read) . words

euclideanToString :: Euc.Euclidean -> String
euclideanToString = concat . (intersperse " ") . (map show) . Euc.toList

simplexToString :: (Show a) => S.Simplex a -> String
simplexToString = concat . (intersperse " ") . (map show) . S.toList

simplexToJavaPlexString :: S.Simplex Int -> String
simplexToJavaPlexString = show . S.toList

