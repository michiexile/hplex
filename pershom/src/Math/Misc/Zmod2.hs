module Math.Misc.Zmod2 where

import qualified Math.Algebra.Monoid as M
import qualified Math.Algebra.AbelianMonoid as AbM
import qualified Math.Algebra.Group as G
import qualified Math.Algebra.Ring as R
import qualified Math.Algebra.Field as F

newtype Zmod2 = C Int
    deriving (Eq, Ord)

fromInt :: Int -> Zmod2
fromInt m = C (m `mod` 2)

instance Show Zmod2 where
    show (C m) = show m ++ " mod 2"

instance M.AdditiveMonoid Zmod2 where
    (C m) <+> (C n) = C ((m+n) `mod` 2)
    nil = C 0

instance M.MultiplicativeMonoid Zmod2 where
    (C m) <*> (C n) = C ((m*n) `mod` 2)
    one = C 1

instance AbM.AbelianAdditiveMonoid Zmod2
instance AbM.AbelianMultiplicativeMonoid Zmod2

instance G.AdditiveGroup Zmod2 where
    negative = id

instance R.Ring Zmod2

instance F.Field Zmod2 where
    reciprocal (C 0) = error "Division by zero."
    reciprocal (C n) = C n