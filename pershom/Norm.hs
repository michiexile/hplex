{-# LANGUAGE MultiParamTypeClasses #-}

-- | Normed spaces.
module Norm where

import qualified Vector as V
import qualified Monoid as M

class (V.Vector a b) => Norm a b where
    norm :: b -> a
    -- | Override 'squareNorm' if the square of the norm is more
    -- efficiently computed directly (such as in the Euclidean case).
    squareNorm :: b -> a 
    squareNorm = M.square . norm 
