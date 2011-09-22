{-# LANGUAGE MultiParamTypeClasses #-}

module InnerProduct where

import qualified Vector as V

infixl 7 <.>

class (V.Vector a b) => InnerProduct a b where
    (<.>) :: b -> b -> a




