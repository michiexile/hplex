-- | Additive and multiplicative monoids.

module Math.Algebra.Monoid where

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

instance (AdditiveMonoid a, AdditiveMonoid b) => AdditiveMonoid (a, b) where
    (x1, y1) <+> (x2,y2) = (x1 <+> x2, y1 <+> y2)
    nil = (nil, nil)

sum :: (AdditiveMonoid a) => [a] -> a
sum = foldl' (<+>) nil

product :: (MultiplicativeMonoid a) => [a] -> a
product = foldl' (<*>) one

square :: (MultiplicativeMonoid a) => a -> a
square x = x <*> x

cube :: (MultiplicativeMonoid a) => a -> a
cube x = x <*> x <*> x