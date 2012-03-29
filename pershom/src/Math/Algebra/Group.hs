-- | (Additive) groups.

module Math.Algebra.Group where
import Math.Algebra.Monoid 

infixl 6 <->

class (AdditiveMonoid a) => AdditiveGroup a where
    negative :: a -> a   

(<->) :: (AdditiveGroup a) => a -> a -> a
x <-> y = x <+> (negative y)

