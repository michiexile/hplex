-- | Append a distinguished "point at infinity" to a type.
module Math.Misc.PlusInfinity where

data PlusInfinity a = Regular a | Infinity
                      deriving (Eq)

instance (Show a) => Show (PlusInfinity a) where
    show (Regular x) = show x
    show Infinity = "∞"

instance (Ord a) => Ord (PlusInfinity a) where
    compare (Regular x) (Regular y) = compare x y
    compare (Regular _) Infinity = LT
    compare Infinity (Regular _) = GT
    compare Infinity Infinity = EQ

isRegular :: PlusInfinity a -> Bool
isRegular Infinity = False
isRegular (Regular _) = True

isInfinite :: PlusInfinity a -> Bool
isInfinite = not . isRegular

extract :: PlusInfinity a -> a
extract (Regular x) = x
extract Infinity = error "Trying to extract ∞."