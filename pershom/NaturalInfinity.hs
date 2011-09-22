-- | Natural numbers union infinity. This version is modelled on 'Integer's.
module NaturalInfinity where

import qualified Natural as Natural
import qualified Nat as Nat

data NInfinity = C Natural.N | Infinity
         deriving Eq

fromNatural :: Natural.N -> NInfinity
fromNatural = C

fromNat :: Nat.N -> NInfinity
fromNat = fromNatural . Natural.fromNat

instance Show NInfinity where
    show (C m) = show m
    show Infinity = "∞"

instance Ord NInfinity where
    compare Infinity Infinity = EQ
    compare Infinity (C _) = GT
    compare (C _) Infinity = LT
    compare (C m) (C n) = compare m n
