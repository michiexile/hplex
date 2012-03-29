{-# LANGUAGE MultiParamTypeClasses #-}

-- | Pseudo-metric spaces.
module Math.VectorSpaces.PseudoMetric where

import Math.Misc.PlusInfinity
import qualified Math.Algebra.AbelianMonoid as AbM

class (Eq a, AbM.AbelianAdditiveMonoid b, Ord b) => PseudoMetric a b where
    distance :: a -> a -> PlusInfinity b
