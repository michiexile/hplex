-- | Fields.

module Math.Algebra.Field where

import qualified Math.Algebra.AbelianMonoid as AbM
import qualified Math.Algebra.Ring as R
import qualified Math.Algebra.Monoid as M

class (AbM.AbelianMultiplicativeMonoid a, R.Ring a) => Field a where
    -- | 'reciprocal' must be defined for every ring element different
    -- from the additive neutral element. It is the user's
    -- responsibility that 'reciprocal' is never called for this
    -- element. In such cases, behavior is undefined.
    reciprocal :: a -> a

infixr 7 </>

-- | @x </> y = x <*> ('reciprocal' y)@.
(</>) :: (Field a) => a -> a -> a
x </> y = x M.<*> (reciprocal y)
