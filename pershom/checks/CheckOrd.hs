module CheckOrd where

import Test.QuickCheck

prop_transitive x y z
    | (x <= y) && (y <= z) = x <= z
    | (x <= z) && (z <= y) = x <= y
    | (y <= z) && (z <= x) = y <= x
    | (y <= x) && (x <= z) = y <= z
    | (z <= x) && (x <= y) = z <= y
    | (z <= y) && (y <= x) = z <= x

prop_reflexive x = x <= x

prop_antisymmetric x y
    | (x <= y) && (y <= x) = x == y
    | otherwise = True

prop_ordered x y z = prop_transitive x y z &&
                     prop_reflexive x &&
                     prop_antisymmetric x y
