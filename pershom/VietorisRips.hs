module VietorisRips where

import qualified NeighborhoodGraph as NG
import qualified Simplex as S
import qualified FilteredComplex as FC
import qualified Data.Map as Map
import Parity
import Misc
import Data.List

data VietorisRips = C 
                    Int -- Number of vertices
                    Double -- Maximum scale
                    [[(S.Simplex Int, Double)]] -- Simplices and weights in dimensions 0, 1, ..., d-1
                    Int -- Dimension, d
                    (Map.Map (S.Simplex Int) Double) -- Simplices and weights in dimension d 
                  | Pre -- Empty "-1"-skeleton.

test :: VietorisRips -> String
test x = unlines' (map foo (toList x))
    where
      foo ss = unlines' $ map show ss

-- | @inductive d g@ computes the @d@-skeleton of the Vietoris-Rips
-- complex of @g@ using the "inductive algorithm" from Afra
-- Zomorodian's paper //Fast Construction of the Vietoris-Ripls
-- complex//.
inductive :: Int -> NG.NeighborhoodGraph -> VietorisRips
inductive d g = compose (replicate (d+1) (induct g)) Pre

simplices :: VietorisRips -> [[S.Simplex Int]]
simplices (C _ _ x _ y) = (map (map fst) x) ++ [(Map.keys y)]

toList :: VietorisRips -> [[(S.Simplex Int, Double)]]
toList (C _ _ x _ y) = x ++ [(Map.toAscList y)]

toList' :: VietorisRips -> [(S.Simplex Int, Double)]
toList' (C _ _ x _ y) = concat x ++ Map.toAscList y

induct :: NG.NeighborhoodGraph -> VietorisRips -> VietorisRips
induct g Pre = C n scale [] 0 (Map.fromList (zip simplices weights))
    where
      n = NG.numVertices g
      scale = NG.scale g
      weights = replicate n 0
      simplices = map S.positiveVertex (NG.vertices g)
induct g (C n scale _ 0 highSimps) = C n scale [highSimps'] 1 (Map.fromList (zip simplices weights))
    where
      highSimps' = Map.toAscList highSimps
      edges = NG.edges g
      simplices = map S.fromPair edges
      weights = map (NG.weight' g) edges
induct g (C n scale simps d highSimps) = C n scale (simps ++ [highSimps']) (d+1) (Map.fromList (go highSimps''))
    where
      highSimps' = Map.toAscList highSimps
      highSimps'' = map fst highSimps'
      go [] = []
      go (s:ss) = zip newSimplices (map weight newSimplices) ++ go ss
          where
            vertices = S.toList s
            minVertex = minimum vertices
            lowerVertices = [ v | v <- [0..minVertex], all (NG.edge g v) vertices]
            newSimplices = map (\v -> S.fromAscendingList Even (v:vertices)) lowerVertices
            weight simplex = maximum $ map (highSimps Map.!) (S.faces' simplex)

filtration :: VietorisRips -> [Double] -> FC.FilteredComplex Double Int
filtration vr filts = FC.fromList $ go filts vr'
    where
      vr' = sortBy (\(_, w1) (_, w2) -> compare w1 w2) (toList' vr)
      go [] _ = []
      go (f:fs) [] = (f, []):(go fs [])
      go (f:fs) xs = (f, map fst left):(go fs right)
          where
            (left, right) = span (\ (_, w) -> w <= f ) xs


scaleSplit :: Double -> [[(S.Simplex Int, Double)]] -> ([[(S.Simplex Int, Double)]], [[(S.Simplex Int, Double)]])
scaleSplit scale xs = (reverse a, reverse r)
    where
      (a, r) = scaleSplit' [] xs
      scaleSplit' :: [[(S.Simplex Int, Double)]] ->
                     [[(S.Simplex Int, Double)]] -> 
                     ([[(S.Simplex Int, Double)]], [[(S.Simplex Int, Double)]])
      scaleSplit' accepted [] = (accepted, [])
      scaleSplit' accepted (cand:cands)
          | (not . null) reject = ((accept:accepted), (reject:cands))
          | otherwise = scaleSplit' (accept:accepted) cands
          where
            (accept, reject) = partition (\(_, w) -> w <= scale) cand
            
              
