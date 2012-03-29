-- | Metric spaces.
module Math.VectorSpaces.Metric where

import Math.Misc.SimpleTypes

class (Eq a) => Metric a where
    distance :: a -> a -> R