{-# LANGUAGE MultiParamTypeClasses #-}

-- | Metric spaces. Alternative version where the metric takes values in
-- any ordered abelian monoid.
module Math.VectorSpaces.Metric2 where

import qualified Math.Algebra.AbelianMonoid as AbM

class (Eq a, AbM.AbelianAdditiveMonoid b, Ord b) => Metric a b where
    distance :: a -> a -> b

