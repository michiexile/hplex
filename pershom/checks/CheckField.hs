module CheckField where

import Test.QuickCheck
import CheckRing
import CheckAbelianMonoid

import Monoid 
import Field

prop_Field x y z =
    prop_Ring x y z                         &&
    prop_AbelianMultiplicativeMonoid x y z  &&
    if x == nil
    then True
    else x <*> (reciprocal x) == one


