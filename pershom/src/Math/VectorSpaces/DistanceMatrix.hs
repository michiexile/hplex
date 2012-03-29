-- | Half-memory implementation of symmetric matrices with
-- null-diagonal.
module Math.VectorSpaces.DistanceMatrix (
                                         DistanceMatrix, RealDistanceMatrix, BooleanDistanceMatrix,
                                         generate, (?), (!), Math.VectorSpaces.DistanceMatrix.map
                                        ) where

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Math.Algebra.Monoid as M

data DistanceMatrix v a = C Int (v a)

type RealDistanceMatrix = DistanceMatrix UV.Vector Double
type BooleanDistanceMatrix = DistanceMatrix UV.Vector Bool

generate :: (GV.Vector v a, M.AdditiveMonoid a) => Int -> ((Int, Int) -> a) -> DistanceMatrix v a
generate n f = C n (GV.unfoldrN ((n*(n-1)) `quot` 2) helper (0,1))
    where
      helper (i, j) 
          | j == n-1  = Just (f (i, j), (i+1, i+2))
          | otherwise = Just (f (i, j), (i, j+1))

-- | Indexing as if the matrix were infact square.
{-# INLINE (?) #-}
(?) :: (GV.Vector v a, M.AdditiveMonoid a) => DistanceMatrix v a -> (Int, Int) -> a
(C n x) ? (i,j)
    | i == j    = M.nil
    | j < i     = x GV.! (unroll n (j,i))
    | otherwise = x GV.! (unroll n (i,j))

-- | Indexing in upper right triangle (off-diagonal) only.
{-# INLINE (!) #-}
(!) :: (GV.Vector v a) => DistanceMatrix v a -> (Int, Int) -> a
(C n x) ! (i,j) = x GV.! (unroll n (i,j))

map :: (GV.Vector v a, GV.Vector v b) => (a -> b) -> DistanceMatrix v a -> DistanceMatrix v b
map f (C n x) = C n (GV.map f x)

-- | Index unrolling.
{-# INLINE unroll #-}
unroll :: Int -> (Int, Int) -> Int
unroll n (i, j) = n*i + j - (((i+1)*(i+2)) `quot` 2)
