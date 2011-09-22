module CheckGroup where

import Test.QuickCheck
import CheckMonoid
import Monoid
import Group

prop_AdditiveGroup x y z =
    prop_AdditiveMonoid x y z &&
    x <-> x == nil                      

