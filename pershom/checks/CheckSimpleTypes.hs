module CheckSimpleTypes where

import Test.QuickCheck
import CheckMonoid
import CheckGroup
import CheckRing
import CheckField
import CheckModule

import SimpleTypes

prop_Z_Ring x y z = prop_Ring (x :: Z) (y :: Z) (z :: Z)

prop_Q_Field x y z = prop_Field (x :: Q) (y :: Q) (z :: Q)

prop_Q_Q_Module r s t x y z = prop_Module (r :: Q) (s :: Q) (t :: Q) (x :: Q) (y :: Q) (z :: Q)
