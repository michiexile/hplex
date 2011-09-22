{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Vector spaces.
module Vector where

import qualified Module as Mod
import qualified Field as F

class (F.Field a, Mod.Module a b) => Vector a b

-- | Fields are vector spaces over themselves.
instance (F.Field a) => Vector a a
