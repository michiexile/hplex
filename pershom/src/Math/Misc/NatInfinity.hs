-- | Natural numbers union infinity. This version is modelled on 'Int's.
module Math.Misc.NatInfinity where

import qualified Math.Misc.Nat as Nat

data NInfinity = C Nat.N | Infinity
         deriving Eq

fromNat :: Nat.N -> NInfinity
fromNat = C

fromInt :: Int -> NInfinity
fromInt = fromNat . Nat.fromInt

toInt :: NInfinity -> Int
toInt Infinity = error "Attempting to convert Infinity to an Int."
toInt (C m) = Nat.toInt m

instance Show NInfinity where
    show (C m) = show m
    show Infinity = "∞"

instance Ord NInfinity where
    compare Infinity Infinity = EQ
    compare Infinity (C _) = GT
    compare (C _) Infinity = LT
    compare (C m) (C n) = compare m n