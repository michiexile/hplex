module CheckRing where

import Test.QuickCheck
import CheckGroup
import CheckMonoid

import Monoid

prop_Ring x y z =
    prop_AdditiveGroup x y z                        &&
    prop_MultiplicativeMonoid x y z                 &&
    x <*> (y <+> z) == x <*> y <+> x <*> z



