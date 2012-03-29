-- | A very simple and dumb 2D array based on vectors.
module Math.Misc.Matrix where

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV

data Matrix v a = C Int Int (v a)

type RealMatrix = Matrix UV.Vector Double

generate :: (GV.Vector v a) => (Int, Int) -> ((Int, Int) -> a) -> Matrix v a
generate (m, n) f = C m n (GV.generate (m*n) f')
    where
      f' k = f (k `divMod` n)

fromList :: (GV.Vector v a) => (Int, Int) -> [a] -> Matrix v a
fromList (m, n) xs 
    | m*n /= GV.length xs' = error ("Matrix.fromList: dimension mismatch. Needed " 
                                    ++ show (m*n) ++ " elements, but was given " ++ show (GV.length xs') ++ ".")
    | otherwise            = C m n xs'
    where
      xs' = GV.fromList xs

{-# INLINE (!) #-} -- Has a huge effect.
(!) :: (GV.Vector v a) => Matrix v a -> (Int, Int) -> a
(C _ n x) ! (i,j) = {-# SCC "Matrix.hs:(!)" #-} x GV.! (i*n + j)

numRows :: Matrix v a -> Int
numRows (C m _ _) = m

numColumns :: Matrix v a -> Int
numColumns (C _ n _) = n

minimum :: (Ord a, GV.Vector v a) => Matrix v a -> a
minimum (C _ _ x) = GV.minimum x

maximum :: (Ord a, GV.Vector v a) => Matrix v a -> a
maximum (C _ _ x) = GV.maximum x   

row :: (GV.Vector v a) => Matrix v a -> Int -> v a
row (C _ n v) i = GV.slice (i*n) n v

rowMinimum :: (GV.Vector v a, Ord a) => Matrix v a -> Int -> a
rowMinimum x i = GV.minimum (x `row` i)