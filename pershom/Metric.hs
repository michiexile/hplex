{-# LANGUAGE MultiParamTypeClasses #-}

-- | Metric spaces.
module Metric where

import SimpleTypes

class (Eq a) => Metric a where
    distance :: a -> a -> R