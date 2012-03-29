module Math.Simplicial.LandmarkSelection where

import qualified System.Random as R
import qualified Math.VectorSpaces.Metric as Met
import qualified Math.Misc.Matrix as Mat
import qualified Data.Vector as V
import Math.Cloud.Cloud
import Data.List
import Data.Maybe

-- | A landmark selection consists of a vector of landmarks and a vector
-- of witnesses. 
data LandmarkSelection a = C (V.Vector a) (V.Vector a)

uniform :: (R.RandomGen b) => b -> Double -> Cloud a -> (LandmarkSelection a, b)
uniform g p xs = (C landmarks' witnesses', g')
    where
      landmarks' = V.fromList landmarks
      witnesses' = V.fromList witnesses
      (landmarks, witnesses, g') = foldl' helper ([], [], g) xs
      helper (ls, ws, gen) x = if r <= abs p
                               then (x:ls, ws, gen')
                               else (ls, x:ws, gen')
          where
            (r, gen') = R.random gen

uniform2 :: (R.RandomGen b) => b -> Double -> Double -> Cloud a -> (LandmarkSelection a, b)
uniform2 g pl pw xs = (C landmarks' witnesses', g')
    where
      landmarks' = V.fromList landmarks
      witnesses' = V.fromList witnesses
      (landmarks, witnesses, g') = foldl' helper ([], [], g) xs
      helper (ls, ws, gen) x = if r <= abs pl
                               then (x:ls, ws, gen')
                               else if r' <= abs pw 
                                    then (ls, x:ws, gen'')
                                    else (ls, ws, gen'')
          where
            (r, gen') = R.random gen
            (r', gen'') = R.random gen'

-- | A non-efficient implementation of the maxmin selection
-- algorithm. It's probably good enough for relatively small point
-- clouds. TODO: Use vectors instead of lists.
maxmin :: (Met.Metric a, Eq a) => Int -> Cloud a -> LandmarkSelection a
maxmin n c = C (V.fromList landmarks) (V.fromList witnesses)
    where
      witnesses = map fst witnessesAndMinDistances
      (landmarks, witnessesAndMinDistances) = go 1 [head c] (zip (tail c) (map (Met.distance (head c)) (tail c)))
      go k ls ws
          | k == n || null ws = (ls, ws)
          | otherwise = go (k+1) (l:ls) ws'
          where
            l = fst $ maximumBy (\ (_, d1) (_, d2) -> compare d1 d2) ws
            ws' = mapMaybe helper ws
                where
                  helper (w, minLandmarkDist)
                      | w == l = Nothing
                      | otherwise = Just (w, min (w `Met.distance` l) minLandmarkDist)


manual :: Cloud a -> Cloud a -> LandmarkSelection a
manual ls ws = C (V.fromList ls) (V.fromList ws)

landmarks :: LandmarkSelection a -> V.Vector a
landmarks (C x _ ) = x

numLandmarks :: LandmarkSelection a -> Int
numLandmarks = V.length . landmarks

witnesses :: LandmarkSelection a -> V.Vector a
witnesses (C _ x ) = x

numWitnesses :: LandmarkSelection a -> Int
numWitnesses = V.length . witnesses

-- | Entry @(i,j)@ in the matrix is the distance between witness
-- number @i@ and landmark number @j@. 
witnessToLandmarkDistances :: (Met.Metric a) => LandmarkSelection a -> Mat.RealMatrix
witnessToLandmarkDistances x = Mat.generate (numWitnesses x, numLandmarks x) generator
    where
      ws = witnesses x
      ls = landmarks x
      generator (i, j) = Met.distance (ws V.! i) (ls V.! j)

