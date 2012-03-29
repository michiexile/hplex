module Math.Misc.Z2 where

import qualified Math.Algebra.Monoid as M
import qualified Math.Algebra.AbelianMonoid as AbM
import qualified Math.Algebra.Group as G
import qualified Math.Algebra.Ring as R
import qualified Math.Algebra.Field as F

-- | This is of course dead slow. It's only here for testing.
data Z2 = Zero | One
        deriving (Eq, Show)

instance Ord Z2 where
    compare Zero Zero = EQ
    compare Zero One = LT
    compare One Zero = GT
    compare One One = EQ

instance M.AdditiveMonoid Z2 where
    Zero <+> Zero = Zero
    Zero <+> One = One
    One <+> Zero = One
    One <+> One = Zero
    nil = Zero

instance M.MultiplicativeMonoid Z2 where
    Zero <*> _ = Zero
    _ <*> Zero = Zero
    One <*> One = One
    one = One

instance AbM.AbelianAdditiveMonoid Z2
instance AbM.AbelianMultiplicativeMonoid Z2

instance G.AdditiveGroup Z2 where
    negative Zero = Zero
    negative One = One

instance R.Ring Z2

instance F.Field Z2 where
    reciprocal One = One