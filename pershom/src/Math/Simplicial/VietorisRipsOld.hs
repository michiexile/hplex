module VietorisRipsOld where

import qualified NeighborhoodGraph as NG
import qualified Simplex as S
import qualified FilteredComplex as FC
import Parity
import Misc

data VietorisRips = C Int Double [S.Simplex Int] Int [S.Simplex Int]
----------------------vs--maxscale-simplices<d----d--simplices=d

inductive :: NG.NeighborhoodGraph -> VietorisRips
inductive = undefined

empty :: Int -> Double -> VietorisRips
empty n s = C n s [] (-1) []

simplices :: VietorisRips -> [S.Simplex Int]
simplices (C _ _ x _ y) = x ++ y

induct :: NG.NeighborhoodGraph -> VietorisRips -> VietorisRips
induct g (C n scale _ (-1) _) = C n scale [] 0 (map S.positiveVertex (NG.vertices g))
induct g (C n scale _ 0 highSimps) = C n scale highSimps 1 (map S.fromPair (NG.edges g))
induct g (C n scale simps d highSimps) = C n scale (simps ++ highSimps) (d+1) (go highSimps)
    where
      go [] = []
      go (s:ss) = newSimplices ++ go ss
          where
            vertices = S.toList s
            minVertex = minimum vertices
            lowerVertices = [ v | v <- [0..minVertex], all (NG.edge g v) vertices]
            newSimplices = map (\v -> S.fromAscendingList Even (v:vertices)) lowerVertices

 -- weights :: VietorisRips -> [Double]
 -- weights vr = go 0 (simplices vr) []
 --     where
 --       go 0 ss _ = ws ++ go 1 y []
 --           where
 --             (x,y) = span (\z -> S.dimension' z == 0) ss
 --             ws = replicate (length x) 0.0
 --       go 1 ss _ = ws ++ go 1 y (x, ws)
 --           where
 --             (x,y) = span (\z -> S.dimension' z == 1) ss
 --             ws = replicate (length x)
 --       go d simps (highSimps, highWeights) = weights ++ go (d+1) y (x, weights)
 --           where
 --             (x,y) = span (\z -> S.dimension' z == d) simps
 --             weights 

weight :: NG.NeighborhoodGraph -> S.Simplex Int -> Double
weight g s
    | S.dimension' s == 0 = 0.0
    | otherwise = maximum $ map (uncurry (NG.weight g)) (symmetricPairs' (S.toList s))

filtration :: NG.NeighborhoodGraph -> VietorisRips -> [Double] -> FC.FilteredComplex Double Int
filtration g vr fs = FC.fromList $ zip fs (go fs (simplices vr))
    where
      go [] _ = []
      go _ [] = []
      go (x:xs) ss = ss' : (go xs ss'')
          where
            (ss', ss'') = span (\y -> weight g y <= x) ss
      