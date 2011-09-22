-- | Rings with unit.
module Ring where
import qualified Monoid as M
import qualified AbelianMonoid as AbM
import qualified Group as G

class (G.AdditiveGroup a,
       AbM.AbelianAdditiveMonoid a,
       M.MultiplicativeMonoid a) => Ring a
