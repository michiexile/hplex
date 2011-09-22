-- | (Additive) groups.

module Group where
import qualified Monoid as M

infixl 6 <->

class (M.AdditiveMonoid a) => AdditiveGroup a where
    negative :: a -> a   

(<->) :: (AdditiveGroup a) => a -> a -> a
x <-> y = x M.<+> (negative y)

