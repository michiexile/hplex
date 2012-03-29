module Math.Misc.Parity where

import Prelude hiding (fromIntegral)
import Math.Algebra.Monoid
import Math.Algebra.AbelianMonoid
import Data.List
import Data.Hashable

data Parity = Odd | Even
              deriving (Eq, Show)

instance Hashable Parity where
    hash Even = 0
    hash Odd = 1

opposite :: Parity -> Parity
opposite Odd = Even
opposite Even = Odd

instance MultiplicativeMonoid Parity where
    Odd <*> Odd = Even
    Odd <*> Even = Odd
    Even <*> Odd = Odd
    Even <*> Even = Even
    one = Even

instance AbelianMultiplicativeMonoid Parity

instance Ord Parity where
    compare Odd Odd = EQ
    compare Even Even = EQ
    compare Even Odd = LT
    compare Odd Even = GT

fromIntegral :: (Integral a) => a -> Parity
fromIntegral m
        | even m = Even
        | otherwise = Odd

fromBool :: Bool -> Parity
fromBool True = Even
fromBool False = Odd

fromLength :: [a] -> Parity
fromLength = foldl' (\p _ -> (opposite p)) Even