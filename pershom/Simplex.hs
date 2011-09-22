-- | Simplices are modelled as vertex lists, together with a parity
-- indicating the orientation of the simplex. Vertices come from an
-- ordered set, so the ascending vertex list is assumed to be
-- positively oriented. The orientation of a simplex with vertex list
-- @v@ is thus taken as the signature of the permutation taking @v@
-- into @'sort' v@.
module Simplex where

import qualified Monoid as M
import qualified Nat as Nat
import Parity
import Permutation
import Misc
import Data.List

-- | Do not construct simplices directly using the constructor. Use
-- 'fromList'.
data Simplex a = C Parity Int [a]

type ISimplex = Simplex Int
type CSimplex = Simplex Char

-- | Construct a simplex from a list of its vertices. The list cannot
-- contain duplicates, a condition that is /not checked/. If you are
-- uncertain, pass the list through 'nub' first or use 'fromList''.
fromList :: (Eq a, Ord a) => [a] -> Simplex a
fromList v = C (Permutation.parity (v', v)) (length v' - 1) v'
    where
      v' = sort v

-- | A 0-dimensional simplex.
vertex :: (Eq a, Ord a) => Parity -> a -> Simplex a
vertex p v = C p 0 [v]

-- | A 0-dimensional simplex.
positiveVertex :: (Eq a, Ord a) => a -> Simplex a
positiveVertex = vertex Even

fromPair :: (Eq a, Ord a) => (a, a) -> Simplex a
fromPair (v, w) 
    | v == w = error "Unhandled error in Simplex.hs."
    | v < w = fromAscendingList Even [v, w]
    | otherwise = fromAscendingList Even [w, v]

-- | A (significantly slower) version of 'fromList' that passes the
-- input list through 'nub', removing duplicates.
fromList' :: (Eq a, Ord a) => [a] -> Simplex a
fromList' = fromList . nub

-- | Construct a simplex from a sorted (/strictly/ ascending) list of
-- its vertices, and orientation parity. The condition that the list
-- is sorted is not checked.
fromAscendingList :: Parity -> [a] -> Simplex a
fromAscendingList p vs = C p (length vs - 1) vs

toList :: Simplex a -> [a]
toList (C _ _ vs) = vs

instance (Show a) => Show (Simplex a) where
    show (C Even _ v) = "simplex" ++ show v
    show (C Odd _ v) = "-simplex" ++ show v

-- | A version of 'show' ignoring orientation.
showSansSign :: (Show a) => Simplex a -> String
showSansSign (C _ d v) = show (C Even d v)

instance (Eq a) => Eq (Simplex a) where
    (C p d v) == (C q e u) = p == q && d == e && v == u -- Dimension comparison is there for quicker failure.

instance (Ord a) => Ord (Simplex a) where
    compare (C p1 d1 v1) (C p2 d2 v2) = {-# SCC "Simplex.hs:compare" #-} compare (d1, v1, p1) (d2, v2, p2)


isNullSimplex :: Simplex a  -> Bool
isNullSimplex (C _ _ []) = True
isNullSimplex (C _ _ (_:_)) = False

nullSimplex :: Simplex a
nullSimplex = C Even (-1) []

dimension :: Simplex a -> Nat.N
dimension (C _ d v)
    | null v = M.nil
    | otherwise = Nat.fromInt d

dimension' :: Simplex a -> Int
dimension' (C _ d v)
    | null v = 0
    | otherwise = d

-- | @'remove' k@ is /signed/ removal of the @k@'th vertex.
remove :: Nat.N -> Simplex a -> Simplex a
remove k (C p d v) = C (p M.<*> (Parity.fromIntegral k')) (d - 1) (removeAt k' v)
    where
      k' = Nat.toInt k

-- | @'remove''@ is the /unsigned/ version of 'remove'.
remove' :: Nat.N -> Simplex a -> Simplex a
remove' k (C p d v) = C p (d - 1) (removeAt k' v)
    where
      k' = Nat.toInt k

-- | Signed codimension-1 faces.
faces :: Simplex a -> [Simplex a]
faces s = map (\i -> remove i s) (takeWhile (<= dimension s) Nat.naturals)

-- | Unsigned codimension-1 faces.
faces' :: Simplex a -> [Simplex a]
faces' s = map (\i -> remove' i s) (takeWhile (<= dimension s) Nat.naturals)

parity :: Simplex a -> Parity
parity (C p _ _) = p

reverse :: Simplex a -> Simplex a
reverse (C p d v) = C (opposite p) d v

evenVersion :: Simplex a -> Simplex a
evenVersion (C _ d v) = C Even d v

-- Testing

s0 :: Simplex Char
s0 = fromList "a"

s1 :: Simplex Char
s1 = fromList "b"

s2 :: Simplex Char
s2 = fromList "c"

s3 :: Simplex Char
s3 = fromList "d"

s4 :: Simplex Char
s4 = fromList "ab"

s5 :: Simplex Char
s5 = fromList "bc"

s6 :: Simplex Char
s6 = fromList "cd"

s7 :: Simplex Char
s7 = fromList "ad"

s8 :: Simplex Char
s8 = fromList "ac"

s9 :: Simplex Char
s9 = fromList "abc"

s10 :: Simplex Char
s10 = fromList "acd"

longS1 :: Simplex Int
longS1 = fromList [0..100000]

longS2 :: Simplex Int
longS2 = fromList ([0..50000] ++ [50002, 50001] ++ [50003..100000])

benchmark = compare longS1 longS2
