{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Vector spaces.
module Math.Algebra.Vector where

import qualified Math.Algebra.Module as Mod
import qualified Math.Algebra.Field as F

class (F.Field a, Mod.Module a b) => Vector a b

-- | Fields are vector spaces over themselves.
instance (F.Field a) => Vector a a
