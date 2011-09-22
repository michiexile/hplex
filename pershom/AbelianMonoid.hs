-- | Abelian monoids.

module AbelianMonoid where
import qualified Monoid as M

class (M.AdditiveMonoid a) => AbelianAdditiveMonoid a
class (M.MultiplicativeMonoid a) => AbelianMultiplicativeMonoid a
