module Math.Simplicial.NeighborhoodGraph where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as BV
import qualified Math.VectorSpaces.Metric as Met
import qualified Math.Simplicial.LandmarkSelection as LS
import qualified Math.Misc.Matrix as Mat
import qualified Math.VectorSpaces.DistanceMatrix as DMat
import Math.Simplicial.PreScale
import Math.Cloud.Cloud
import Data.List
import Misc.Misc

data NeighborhoodGraph = C
                         Int -- Number of nodes.
                         Double -- Maximum scale.
                         DMat.RealDistanceMatrix -- Edge weights.
                         DMat.BooleanDistanceMatrix -- Edge table.

exact :: (Met.Metric a) => Cloud a -> PreScale -> NeighborhoodGraph
exact _ (ConditionFactor _) = error "Exact neighborhood graphs only available with absolute maximum scale."
exact c (Absolute scale) = C n scale ws es
    where
      c' = BV.fromList c
      n = BV.length c'
      ws = DMat.generate n (\ (i,j) -> Met.distance (c' BV.! i) (c' BV.! j))
      es = DMat.map (<= scale) ws

-- | Follows de Silva, Carlsson: Topological estimation using witness complexes.
lazyWitness :: (Met.Metric a) => LS.LandmarkSelection a -> Int -> PreScale -> NeighborhoodGraph
lazyWitness ls nu preScale = C nl scale weights edges
    where
      scale = case preScale of
                Absolute s -> s
                ConditionFactor c -> c * (UV.maximum (UV.generate nw (Mat.rowMinimum d)))
      witnesses = LS.witnesses ls
      landmarks = LS.landmarks ls
      d = LS.witnessToLandmarkDistances ls
      m = if nu <= 0 
          then UV.replicate nw 0.0
          else UV.generate nw (\i -> smallest' nu (UV.toList (d `Mat.row` i)))
      nl = LS.numLandmarks ls
      nw = LS.numWitnesses ls
      weights = DMat.generate nl weight
      edges = DMat.map (<= scale) weights
      weight :: (Int, Int) -> Double
      weight (i, j) = UV.minimum (UV.generate nw (witnessWeight (i,j)))
      witnessWeight :: (Int, Int) -> Int -> Double
      witnessWeight (i, j) k = max 0 (dMax - m UV.! k)
          where
            dMax = max (d Mat.! (k,i)) (d Mat.! (k,j))


weight :: NeighborhoodGraph -> Int -> Int -> Double
weight (C _ _ ws _) i j = ws DMat.? (i, j)
    
edge :: NeighborhoodGraph -> Int -> Int -> Bool
edge (C _ _ _ es) i j = es DMat.? (i, j)

weight' :: NeighborhoodGraph -> (Int, Int) -> Double
weight' g = uncurry (weight g)

numVertices :: NeighborhoodGraph -> Int
numVertices (C n _ _ _) = n

vertices :: NeighborhoodGraph -> [Int]
vertices g = [0..(numVertices g - 1)]

edges :: NeighborhoodGraph -> [(Int, Int)]
edges g = go (vertices g)
    where
      go [] = []
      go (i:is) = zip (repeat i) (filter (edge g i) is) ++ go is

scale :: NeighborhoodGraph -> Double
scale (C _ s _ _) = s


