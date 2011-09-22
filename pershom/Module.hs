{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Modules over /commutative/ rings. This restriction may be lifted
-- in the future, but for now this simplification allows us to avoid
-- considering both left- and right modules.
module Module where -- No pun intended.
import qualified Monoid as M
import qualified AbelianMonoid as AbM
import qualified Ring as R
import qualified Group as G

infixr 8 *>

-- | @'Module' a b@ means that @b@ is a (left- and right-) @a@-module.
class (AbM.AbelianMultiplicativeMonoid a, R.Ring a,
       AbM.AbelianAdditiveMonoid b, G.AdditiveGroup b) => Module a b where
    -- | @'action' r x@ is the action of @r@ on @x@.
    action :: a -> b -> b 

-- | @r '*>' x = 'action' r x@.
(*>) :: (Module a b) => a -> b -> b
x *> y = action x y

-- | Commutative rings are modules over themselves.
instance (AbM.AbelianMultiplicativeMonoid a, R.Ring a) => Module a a where
    action = (M.<*>)