module CheckModule where

import Test.QuickCheck
import CheckGroup
import CheckRing
import CheckAbelianMonoid

import Group
import Ring
import Monoid
import Module

prop_Module r s t x y z =
    prop_Ring r s t                                              &&
    prop_AbelianMultiplicativeMonoid r s t                       &&
    prop_AbelianAdditiveMonoid x y z                             &&
    prop_AdditiveGroup x y z                                     &&
    (r <+> s) *> (x <+> y) == r *> x <+> r *> y <+> s *> x <+> s *> y
