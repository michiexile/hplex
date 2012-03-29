{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Aliases for well-known default data types.
module Math.Misc.SimpleTypes where

import qualified Math.Algebra.Monoid as M
import qualified Math.Algebra.AbelianMonoid as AbM
import qualified Math.Algebra.Group as G
import qualified Math.Algebra.Ring as R
import qualified Math.Algebra.Field as F
import qualified Math.VectorSpaces.Metric2 as Met
import Data.Ratio

type Q = Ratio Integer
type R = Double
type Z = Integer

instance M.AdditiveMonoid Q where
    (<+>) = (+)
    nil = 0 % 1

instance M.MultiplicativeMonoid Q where
    (<*>) = (*)
    one = 1 % 1

instance M.AdditiveMonoid R where
    (<+>) = (+)
    nil = 0.0

instance M.MultiplicativeMonoid R where
    (<*>) = (*)
    one = 1.0

instance M.AdditiveMonoid Z where
    (<+>) = (+)
    nil = 0

instance M.MultiplicativeMonoid Z where
    (<*>) = (*)
    one = 1
    
instance M.AdditiveMonoid Int where
    (<+>) = (+)
    nil = 0

instance M.MultiplicativeMonoid Int where
    (<*>) = (*)
    one = 1

instance M.AdditiveMonoid Bool where
    (<+>) = (||)
    nil = False

instance M.MultiplicativeMonoid Bool where
    (<*>) = (&&)
    one = True

instance AbM.AbelianAdditiveMonoid Q
instance AbM.AbelianMultiplicativeMonoid Q
instance AbM.AbelianAdditiveMonoid R
instance AbM.AbelianMultiplicativeMonoid R
instance AbM.AbelianAdditiveMonoid Z
instance AbM.AbelianMultiplicativeMonoid Z
instance AbM.AbelianAdditiveMonoid Int
instance AbM.AbelianMultiplicativeMonoid Int
instance AbM.AbelianAdditiveMonoid Bool
instance AbM.AbelianMultiplicativeMonoid Bool

instance G.AdditiveGroup Z where
    negative = negate
    
instance G.AdditiveGroup Int where
    negative = negate

instance G.AdditiveGroup Q where
    negative = negate

instance G.AdditiveGroup R where
    negative = negate

instance R.Ring Z
instance R.Ring Int
instance R.Ring Q
instance R.Ring R

instance F.Field Q where
    reciprocal = recip

instance F.Field R where
    reciprocal x = 1.0/x


instance Met.Metric R R where
    distance x y = abs (y - x)

instance Met.Metric Z Z where
    distance m n = abs (n - m)

instance Met.Metric Int Int where
    distance m n = abs (n - m)
