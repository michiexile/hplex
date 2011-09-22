-- | Additive and multiplicative monoids.

module Monoid where

import Prelude hiding (sum, product)
import Data.List

infixl 6 <+>
infixl 7 <*>

class (Eq a) => AdditiveMonoid a where
    -- | Addition.
    (<+>) :: a -> a -> a
    -- | Additive neutral element.
    nil :: a

class (Eq a) => MultiplicativeMonoid a where
    -- | Multiplication.
    (<*>) :: a -> a -> a
    -- | Multiplicative neutral element.
    one :: a

sum :: (AdditiveMonoid a) => [a] -> a
sum = foldl' (<+>) nil

product :: (MultiplicativeMonoid a) => [a] -> a
product = foldl' (<*>) one

square :: (MultiplicativeMonoid a) => a -> a
square x = x <*> x

cube :: (MultiplicativeMonoid a) => a -> a
cube x = x <*> x <*> x