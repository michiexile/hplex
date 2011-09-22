module CheckVector where

import Test.QuickCheck
import CheckModule
import CheckField

prop_Vector r s t x y z =
    prop_Field r s t        &&
    prop_Module r s t x y z

