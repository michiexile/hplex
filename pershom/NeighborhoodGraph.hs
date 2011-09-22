-- | A neighborhood graph is just a list of distances between vertices.

module NeighborhoodGraph where

import qualified Data.Vector.Unboxed as Vect
import qualified Euclidean as Euc -- For testing only.
import qualified Metric as Met
import Cloud
import Data.List

data NeighborhoodGraph = C Int Double (Vect.Vector Double) (Vect.Vector Bool)

exact :: (Met.Metric a) => Cloud a -> Double -> NeighborhoodGraph
exact c scale = C n scale ws es
    where
      n = length c
      n' = (n*(n-1)) `quot` 2 -- Elements in storage vector.
      ws = Vect.fromList (weights c)
      es = Vect.map (<= scale) ws
      weights [] = []
      weights (x:xs) = map (Met.distance x) xs ++ weights xs

weight :: NeighborhoodGraph -> Int -> Int -> Double
weight (C n _ ws _) i j
    | i == j      = 0
    | i > j       = ws Vect.! (unroll n (j, i))
    | otherwise   = ws Vect.! (unroll n (i, j))

weight' :: NeighborhoodGraph -> (Int, Int) -> Double
weight' g = uncurry (weight g)

unroll :: Int -> (Int, Int) -> Int
unroll n (i, j) = n*i + j - (((i+1)*(i+2)) `quot` 2)

edge :: NeighborhoodGraph -> Int -> Int -> Bool
edge (C n _ _ es) i j
    | i == j      = False
    | i > j       = es Vect.! unroll n (j, i)
    | otherwise   = es Vect.! unroll n (i, j)

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


-- Testing
a = Euc.fromList [0, 0]
b = Euc.fromList [1, 1]
c = Euc.fromList [-1, -1]
d = Euc.fromList [0, 1.5]

cloud = [a,b,c,d]

ng = exact cloud 5