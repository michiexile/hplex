{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Aliases for well-known default data types.
module SimpleTypes where

import qualified Monoid as M
import qualified AbelianMonoid as AbM
import qualified Group as G
import qualified Ring as R
import qualified Field as F
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

instance AbM.AbelianAdditiveMonoid Q
instance AbM.AbelianMultiplicativeMonoid Q
instance AbM.AbelianAdditiveMonoid R
instance AbM.AbelianMultiplicativeMonoid R
instance AbM.AbelianAdditiveMonoid Z
instance AbM.AbelianMultiplicativeMonoid Z
instance AbM.AbelianAdditiveMonoid Int
instance AbM.AbelianMultiplicativeMonoid Int

instance G.AdditiveGroup Z where
    negative m = (-m)
    
instance G.AdditiveGroup Int where
    negative m = (-m)

instance G.AdditiveGroup Q where
    negative r = (-r)

instance G.AdditiveGroup R where
    negative x = (-x)

instance R.Ring Z
instance R.Ring Int
instance R.Ring Q
instance R.Ring R

instance F.Field Q where
    reciprocal = recip

instance F.Field R where
    reciprocal x = 1.0/x

