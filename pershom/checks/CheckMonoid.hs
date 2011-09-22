module CheckMonoid where

import Test.QuickCheck

import Monoid

prop_AdditiveMonoid x y z =
    (x <+> y) <+> z == x <+> (y <+> z) &&
    x <+> nil == x                        

prop_MultiplicativeMonoid x y z =
    (x <*> y) <*> z == x <*> (y <*> z) &&
    x <*> one == x                        
      


