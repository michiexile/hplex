{-# LANGUAGE MultiParamTypeClasses #-}

module Math.VectorSpaces.InnerProduct where

import qualified Math.Algebra.Vector as V

infixl 7 <.>

class (V.Vector a b) => InnerProduct a b where
    (<.>) :: b -> b -> a




