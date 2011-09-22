module CheckAbelianMonoid where

import Test.QuickCheck

import Monoid
import CheckMonoid

prop_AbelianAdditiveMonoid x y z =
    prop_AdditiveMonoid x y z &&
    x <+> y == y <+> x

prop_AbelianMultiplicativeMonoid x y z =
    prop_MultiplicativeMonoid x y z    &&
    x <*> y == y <*> x



