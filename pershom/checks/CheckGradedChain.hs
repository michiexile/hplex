module CheckGradedChain where

import Test.QuickCheck
import CheckSimplex
import GradedChain
import Misc
import SimpleTypes
import qualified Group as G
import qualified Ring as R
import qualified Monoid as M
import qualified Nat as Nat
import CheckMonoid
import CheckGroup
import CheckModule
import CheckNat

instance (Arbitrary a, Ord a, Arbitrary b, R.Ring b) => Arbitrary (GradedChain a b) where
    arbitrary = arbitrary >>=
                \x -> return (fromList x)

prop_IChainZ_Module r s t x y z = prop_Module (r :: Z) (s :: Z) (t :: Z) (x :: IChainZ) (y :: IChainZ) (z :: IChainZ)

prop_CChainZ_Module r s t x y z = prop_Module (r :: Z) (s :: Z) (t :: Z) (x :: CChainZ) (y :: CChainZ) (z :: CChainZ)

prop_Boundary x = (boundary . boundary) x == M.nil

prop_IChainZ_Boundary x = prop_Boundary (x :: IChainZ)
prop_CChainZ_Boundary x = prop_Boundary (x :: CChainZ)

prop_IsNullChain x = isNullChain x == (x == M.nil) && isNullChain (x G.<-> x)

prop_IChainZ_IsNullChain x = prop_IsNullChain (x :: IChainZ)
prop_CChainZ_IsNullChain x = prop_IsNullChain (x :: CChainZ)

prop_HomogenizedHomogeneous x n = GradedChain.homogeneous (homogenize n x) || isNullChain x

prop_IChainZ_HomogenizedHomogeneous x n = prop_HomogenizedHomogeneous (x :: IChainZ) (n :: Nat.N)
prop_CChainZ_HomogenizedHomogeneous x n = prop_HomogenizedHomogeneous (x :: CChainZ) (n :: Nat.N)

prop_HomogenizedHomogeneousDegree x n = isNullChain x || homogeneousDegree (homogenize n x) == n

prop_IChainZ_HomogenizedHomogeneousDegree x n = prop_HomogenizedHomogeneousDegree (x :: IChainZ) (n :: Nat.N)
prop_CChainZ_HomogenizedHomogeneousDegree x n = prop_HomogenizedHomogeneousDegree (x :: CChainZ) (n :: Nat.N)