-- | 'Int'-based natural numbers.

module Nat where

import qualified Monoid as M
import SimpleTypes

-- | Only construct directly using the constructor if you can
-- guarantee that the 'Int' you wrap is positive.
data N = C Int
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

toInt :: N -> Int
toInt (C m) = m

toInteger :: N -> Integer
toInteger = fromIntegral . toInt

fromInt :: Int -> N
fromInt = C . abs

infixr 9 <^>

(<^>) :: (M.MultiplicativeMonoid a) => a -> N -> a
x <^> n = M.product $ replicate (toInt n) x

naturals :: [N]
naturals = map fromInt [0..maxBound]

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
hundred = C 100

