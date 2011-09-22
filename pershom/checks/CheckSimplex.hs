module CheckSimplex where

import Test.QuickCheck
import CheckOrd
import qualified Simplex as S

instance (Arbitrary a, Ord a) => Arbitrary (S.Simplex a) where
    arbitrary = arbitrary >>=
                \vs -> return (S.fromList' vs)

prop_ISimplex_ordered x y z = prop_ordered (x :: S.ISimplex) (y :: S.ISimplex) (z :: S.ISimplex)