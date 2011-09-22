module CheckParity where

import Test.QuickCheck
import CheckAbelianMonoid

import Parity
import SimpleTypes

instance Arbitrary Parity where
    arbitrary = arbitrary >>=
                \b -> return (fromBool b)

prop_Parity_AbelianMultiplicativeMonoid x y z = prop_AbelianMultiplicativeMonoid (x :: Parity) (y :: Parity) (z :: Parity)
