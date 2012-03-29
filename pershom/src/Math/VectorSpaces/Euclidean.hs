{-# LANGUAGE MultiParamTypeClasses #-}

-- | Euclidean space... sort of. The data type 'Euclidean' represents
-- the union of R^n for n=1,2,..., but addition is only defined for
-- elements of equal dimension. This trades having a datatype close to
-- mathematics, and compile-time type safety, for efficiency and
-- convenience. (While it is possible to have length-typed vectors in
-- Haskell [1], using a non-length-typed plain Data.Vector for storage
-- is simple and great for performance).
--
-- [1] http://conal.net/blog/posts/doing-more-with-length-typed-vectors
module Math.VectorSpaces.Euclidean where

import qualified Math.Algebra.AbelianMonoid as AbM
import qualified Math.Algebra.Monoid as M
import qualified Math.Algebra.Group as G
import qualified Math.Algebra.Module as Mod
import qualified Math.VectorSpaces.Metric as Met
import qualified Math.Algebra.Vector as V
import qualified Math.VectorSpaces.Norm as N
import qualified Math.Misc.Nat as Nat
import qualified Math.VectorSpaces.InnerProduct as IP
import qualified Data.Vector.Unboxed as Vect
import Math.Misc.SimpleTypes

-- | Do not construct directly, except when using 'Null'. The
-- rationale for having a separate 'Null' constructor (which we chose
-- not to have for, say, simplices) is the need for an any-dimensional
-- null vector. The need for such a creature of course stems from the
-- typesystem's lack of knowledge of dimensions. The null vector is
-- really considered 0-dimensional, and we allow considering it as
-- any-dimensional.
--
-- It should be noted that there is a slight difference between 'Null'
-- and a non-zero-dimensional vector @v@ with all components
-- 0. Although @v == 'Null'@, you're not allowed to add @v@ to a
-- vector of different dimension. Use 'Null' for that. Also general
-- you should use 'Null' over such a @v@, as @'Null' == 'Null'@ is
-- O(1), while all other comparisons are potentially O(n).
data Euclidean = C (Vect.Vector Double) | Null

instance Eq Euclidean where
    Null == Null      = True
    C x == C y        = x == y
    C x == Null       = Vect.null (Vect.dropWhile (==0) x)
    Null == C x       = Vect.null (Vect.dropWhile (==0) x)

instance Show Euclidean where
    show Null = "0"
    show (C x) 
        | Vect.null x = "0"
        | otherwise = (show . Vect.toList) x

fromList :: [Double] -> Euclidean
fromList = C . Vect.fromList

fromVector :: Vect.Vector Double -> Euclidean
fromVector = C

components :: Euclidean -> Vect.Vector Double
components (C x) = x
components Null = Vect.empty

toList :: Euclidean -> [Double]
toList = Vect.toList . components

dimension :: Euclidean -> Nat.N
dimension (C x) = (Nat.fromInt . Vect.length) x
dimension Null = Nat.zero

dimension' :: Euclidean -> Int
dimension' (C x) = Vect.length x
dimension' Null = 0

-- | Turn a (machine-approximate) null vector into 'Null'. Don't use
-- needlessly, it's an O(n) operation in the worst case.
reduce :: Euclidean -> Euclidean
reduce v 
    | v == Null = Null
    | otherwise = v

instance Ord Euclidean where
    compare v w = compare (dimension v, components v) (dimension w, components w)

instance M.AdditiveMonoid Euclidean where
    (C x) <+> (C y)
        | Vect.length x == Vect.length y = C (Vect.zipWith (M.<+>) x y)
        | otherwise = error "Trying to add Euclidean vectors of different dimension."
    x <+> Null = x
    Null <+> x = x
    nil = Null

instance AbM.AbelianAdditiveMonoid Euclidean

instance G.AdditiveGroup Euclidean where
    negative Null = Null
    negative (C x) = C (Vect.map G.negative x)

instance Mod.Module Double Euclidean where
    action c (C x) = C (Vect.map (c M.<*>) x)
    action _ Null = Null

instance V.Vector Double Euclidean

instance IP.InnerProduct Double Euclidean where
    _ <.> Null = M.nil
    Null <.> _ = M.nil
    (C x) <.> (C y) 
        | Vect.length x == Vect.length y = Vect.sum (Vect.zipWith (M.<*>) x y)
        | otherwise = error "Trying to compute the inner product of Euclidean vectors of different length."

instance N.Norm Double Euclidean where
    norm v = sqrt (v IP.<.> v)
    squareNorm Null = 0
    squareNorm (C x) = Vect.sum (Vect.map M.square x)

instance Met.Metric Euclidean where
    distance u v = N.norm (u G.<-> v)

projectionOntoFirst :: Nat.N -> Euclidean -> Euclidean
projectionOntoFirst _ Null = Null
projectionOntoFirst n (C v) = C (Vect.take (Nat.toInt n) v)



