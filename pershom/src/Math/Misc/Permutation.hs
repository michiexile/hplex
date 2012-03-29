module Math.Misc.Permutation where

import Math.Misc.Parity
import Misc.Misc
import Math.Algebra.Monoid

-- | A permutation is represented as a pair of lists. The condition
-- that the two lists contain precisely the same elements is /not
-- checked/.
type Permutation a = ([a], [a])

-- | Calculate the parity of a given permutation. The parity is 'Even'
-- if the permutation can be decomposed into an even number of
-- transpositions, and 'Odd' otherwise.
parity :: (Eq a) => Permutation a -> Parity
parity = go Even
    where
      go p (u, v)
          | u == v            = p
          | otherwise         = go (p <*> fromLength v1) (tail u, v1 ++ tail' v2)
          where
            pivot = head u
            (v1, v2) = break (== pivot) v