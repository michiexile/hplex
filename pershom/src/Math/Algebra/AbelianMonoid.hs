-- | Abelian monoids.

module Math.Algebra.AbelianMonoid where
import qualified Math.Algebra.Monoid as M

class (M.AdditiveMonoid a) => AbelianAdditiveMonoid a
class (M.MultiplicativeMonoid a) => AbelianMultiplicativeMonoid a
