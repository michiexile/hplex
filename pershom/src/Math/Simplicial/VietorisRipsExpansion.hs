module Math.Simplicial.VietorisRipsExpansion where

import qualified Math.Simplicial.NeighborhoodGraph as NG
import qualified Math.Simplicial.Simplex as S
import qualified Math.Simplicial.FilteredComplex as FC
import qualified Data.Map as Map
import Math.Misc.Parity
import Misc.Misc
import Data.List

data Inductive = C 
                 Int -- Number of vertices
                 [[(S.Simplex Int, Double)]] -- Simplices and weights in dimensions 0, 1, ..., d-1
                 Int -- Dimension, d
                 (Map.Map (S.Simplex Int) Double) -- Simplices and weights in dimension d 
               | Pre -- Empty "-1"-skeleton.

-- | @inductive d g@ computes the @d@-skeleton using the "inductive
-- algorithm" from Afra Zomorodian's paper //Fast Construction of the
-- Vietoris-Ripls complex//.
inductive :: Int -> NG.NeighborhoodGraph -> Inductive
inductive d g = compose (replicate (d+1) (induct g)) Pre

simplices :: Inductive -> [[S.Simplex Int]]
simplices (C _ x _ y) = (map (map fst) x) ++ [(Map.keys y)]

toList :: Inductive -> [[(S.Simplex Int, Double)]]
toList (C _ x _ y) = x ++ [(Map.toAscList y)]

toList' :: Inductive -> [(S.Simplex Int, Double)]
toList' (C _ x _ y) = concat x ++ Map.toAscList y

induct :: NG.NeighborhoodGraph -> Inductive -> Inductive
induct g Pre = C n [] 0 (Map.fromList (zip simplices weights))
    where
      n = NG.numVertices g
      weights = replicate n 0
      simplices = map S.positiveVertex (NG.vertices g)
induct g (C n _ 0 highSimps) = C n [highSimps'] 1 (Map.fromList (zip simplices weights))
    where
      highSimps' = Map.toAscList highSimps
      edges = NG.edges g
      simplices = map S.fromPair edges
      weights = map (NG.weight' g) edges
induct g (C n simps d highSimps) = C n (simps ++ [highSimps']) (d+1) (Map.fromList (go highSimps''))
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

-- | Convert an inductive VR structure into a filtered complex. The
-- supplied list of filtration times must be monotonically increasing (not checked).
filtration :: Inductive -> [Double] -> FC.FilteredComplex Double Int
filtration vr filts = FC.fromList $ go filts vr'
    where
      vr' = sortBy (\(_, w1) (_, w2) -> compare w1 w2) (toList' vr)
      go [] _ = []
      go (f:fs) [] = (f, []):(go fs [])
      go (f:fs) xs = (f, map fst left):(go fs right)
          where
            (left, right) = span (\ (_, w) -> w <= f ) xs

