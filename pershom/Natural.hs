-- | 'Integer'-based natural numbers.

module Natural where

import Prelude hiding (fromInteger)
import qualified Monoid as M
import qualified Nat as Nat
import SimpleTypes
import Data.List

-- | Only construct directly using the constructor if you can
-- guarantee that the 'Integer' you wrap is positive.
data N = C Integer
         deriving (Eq, Ord)

instance Show N where
    show (C m) = show m

instance M.AdditiveMonoid N where
    (C m) <+> (C n) = C (m + n)
    nil = C 0

instance M.MultiplicativeMonoid N where
    (C m) <*> (C n) = C (m * n)
    one = C 1

mod :: N -> N -> N
(C m) `mod ` (C n) = C (Prelude.mod m n)

toInteger :: N -> Integer
toInteger (C m) = m

fromInteger :: Integer -> N
fromInteger = C . abs

fromInt :: Int -> N
fromInt = C . fromIntegral . abs

fromNat :: Nat.N -> N
fromNat = C . Nat.toInteger

infixr 9 <^>

(<^>) :: (M.MultiplicativeMonoid a) => a -> N -> a
x <^> n = M.product $ genericReplicate (Natural.toInteger n) x

naturals :: [N]
naturals = map fromInteger [0..]

next :: N -> N
next = (M.<+>) one

zero = C 0
one = C 1
two = C 2
three = C 3
four = C 4
five = C 5
six = C 6
seven = C 7
eight = C 8
nine = C 9
ten = C 10
