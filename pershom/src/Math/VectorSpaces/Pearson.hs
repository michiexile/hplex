{-# LANGUAGE MultiParamTypeClasses #-}


module Math.VectorSpaces.Pearson where

import qualified Math.Algebra.AbelianMonoid as AbM
import qualified Math.Algebra.Monoid as M
import qualified Math.Algebra.Group as G
import qualified Math.Algebra.Module as Mod
import qualified Math.VectorSpaces.Metric as Met
import qualified Math.Algebra.Vector as V
import qualified Math.Misc.Nat as Nat
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
data Pearson = C (Vect.Vector Double) | Null

instance Eq Pearson where
    Null == Null      = True
    C x == C y        = x == y
    C x == Null       = Vect.null (Vect.dropWhile (==0) x)
    Null == C x       = Vect.null (Vect.dropWhile (==0) x)

instance Show Pearson where
    show Null = "0"
    show (C x) 
        | Vect.null x = "0"
        | otherwise = (show . Vect.toList) x

fromList :: [Double] -> Pearson
fromList = C . Vect.fromList

components :: Pearson -> Vect.Vector Double
components (C x) = x
components Null = Vect.empty

toList :: Pearson -> [Double]
toList = Vect.toList . components

dimension :: Pearson -> Nat.N
dimension (C x) = (Nat.fromInt . Vect.length) x
dimension Null = Nat.zero

dimension' :: Pearson -> Int
dimension' (C x) = Vect.length x
dimension' Null = 0

-- | Turn a (machine-approximate) null vector into 'Null'. Don't use
-- needlessly, it's an O(n) operation in the worst case.
reduce :: Pearson -> Pearson
reduce v 
    | v == Null = Null
    | otherwise = v

instance Ord Pearson where
    compare v w = compare (dimension v, components v) (dimension w, components w)

instance M.AdditiveMonoid Pearson where
    (C x) <+> (C y)
        | Vect.length x == Vect.length y = C (Vect.zipWith (M.<+>) x y)
        | otherwise = error "Trying to add Pearson vectors of different dimension."
    x <+> Null = x
    Null <+> x = x
    nil = Null

instance AbM.AbelianAdditiveMonoid Pearson

instance G.AdditiveGroup Pearson where
    negative Null = Null
    negative (C x) = C (Vect.map G.negative x)

instance Mod.Module Double Pearson where
    action c (C x) = C (Vect.map (c M.<*>) x)
    action _ Null = Null

instance V.Vector Double Pearson


-- FIXME: Should really use pseudometric here. This "metric" can certainly take infinite values.
instance Met.Metric Pearson where
    distance _ Null = 1.0 / 0.0
    distance Null _ = 1.0 / 0.0
    distance (C x) (C y) 
        | Vect.length x /= Vect.length y = error "Trying to compute the distance between Pearson vectors of different dimension."
        | otherwise = 1.0 - ( (n*ip-sx*sy)/( (sqrt (n*ssx-sx^2)) * (sqrt (n*ssy-sy^2)) )  )
        where
          n = fromIntegral $ Vect.length x
          sx = Vect.sum x
          sy = Vect.sum y
          ssx = Vect.sum (Vect.map M.square x)
          ssy = Vect.sum (Vect.map M.square y)
          ip = Vect.sum (Vect.zipWith (M.<*>) x y)

