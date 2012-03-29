-- | Rings with unit.
module Math.Algebra.Ring where
import qualified Math.Algebra.Monoid as M
import qualified Math.Algebra.AbelianMonoid as AbM
import qualified Math.Algebra.Group as G

class (G.AdditiveGroup a,
       AbM.AbelianAdditiveMonoid a,
       M.MultiplicativeMonoid a) => Ring a
