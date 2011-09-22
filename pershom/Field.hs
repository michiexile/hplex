-- | Fields.

module Field where

import qualified AbelianMonoid as AbM
import qualified Ring as R
import qualified Monoid as M

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
