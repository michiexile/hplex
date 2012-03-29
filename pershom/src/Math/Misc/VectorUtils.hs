{-# LANGUAGE ExistentialQuantification #-}

module Math.Misc.VectorUtils where

import qualified Data.Vector as Vect
import qualified Data.Vector.Unboxed as UVect

-- | See 'minIndex'.
minIndex2 :: (Ord a) => Vect.Vector a -> (Int, Int)
minIndex2 v 
    | Vect.length v < 2 = error "minIndex2 called on a vector of length < 2."
    | otherwise         = Vect.ifoldl' helper (initial1, initial2) (Vect.drop 2 v)
    where
      (initial1, initial2) = if v Vect.! 0 <= v Vect.! 1
                             then (0,1)
                             else (1,0)
      helper (i1, i2) i x
          | x < v Vect.! i1 = (i, i1)
          | x < v Vect.! i2 = (i1, i)
          | otherwise       = (i1, i2)


