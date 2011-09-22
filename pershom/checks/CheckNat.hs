module CheckNat where

import Test.QuickCheck
import CheckAbelianMonoid

import qualified Nat as Nat

instance Arbitrary Nat.N where
    arbitrary = arbitrary >>=
                \n -> return (Nat.fromInt n)

prop_N_AbelianAdditiveMonoid x y z = prop_AbelianAdditiveMonoid (x :: Nat.N) (y :: Nat.N) (z :: Nat.N)

prop_N_AbelianMultiplicativeMonoid x y z = prop_AbelianMultiplicativeMonoid (x :: Nat.N) (y :: Nat.N) (z :: Nat.N)
