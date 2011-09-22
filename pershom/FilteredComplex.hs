-- | @FilteredComplex a b@ represents a complex of simplices (with
-- vertex type @b@) filtered over @a@s. A filtered (simplicial)
-- complex is stored as a vector of filtration values and a vector of
-- lists of simplices. The @n@'th filtration of the complex is the
-- collection of simplices from the lists in positions @0@ through
-- @n@. Index @k@ in the first vector represents the value at which
-- the simplices at index @k@ in the second vector enter the
-- filtration.

module FilteredComplex where

import qualified Data.Vector as Vect
import qualified Simplex as S
import qualified Nat as Nat
import Misc

data FilteredComplex a b = C (Vect.Vector a) (Vect.Vector [S.Simplex b])

instance (Show a, Show b) => Show (FilteredComplex a b) where
    show c@(C fs _) = "### " ++ show (Vect.length fs) ++ " degrees\n"
                     ++ unlines' (map (showDegree c) [0..(Vect.length fs - 1)])

showDegree :: (Show a, Show b) => FilteredComplex a b -> Int -> String
showDegree (C fs ss) i = "### " ++ show i ++ ": " ++ show (fs Vect.! i) ++
                         if null (ss Vect.! i)
                         then ""
                         else "\n" ++ unlines' (map show (ss Vect.! i)) 

degree :: FilteredComplex a b -> Nat.N -> [S.Simplex b]
degree (C _ s) n = s Vect.! (Nat.toInt n)

degree' :: FilteredComplex a b -> Int -> [S.Simplex b]
degree' (C _ s) n = s Vect.! n

degrees :: FilteredComplex a b -> Nat.N 
degrees = Nat.fromInt . degrees'

degrees' :: FilteredComplex a b -> Int
degrees' (C fs _) = Vect.length fs

toVect :: FilteredComplex a b -> Vect.Vector [S.Simplex b]
toVect (C _ s) = s

allSimplices :: FilteredComplex a b -> [S.Simplex b]
allSimplices = concat . Vect.toList . toVect

fromList :: [(a, [S.Simplex b])] -> FilteredComplex a b
fromList x = C fs ss
    where
      x' = unzip x
      fs = Vect.fromList (fst x')
      ss = Vect.fromList (snd x')

toList :: FilteredComplex a b -> [(a, [S.Simplex b])]
toList (C fs ss) = zip (Vect.toList fs) (Vect.toList ss)

toList' :: FilteredComplex a b -> [(Nat.N, S.Simplex b)]
toList' x = concatMap (\ n -> zip (repeat (Nat.fromInt n)) (x `degree'` n)) [0..(degrees' x - 1)]


--- TESTING

testComplex :: FilteredComplex Nat.N Char
testComplex = fromList [
               (Nat.zero, [S.s0, S.s1]),
               (Nat.one, [S.s2, S.s3, S.s4, S.s5]),
               (Nat.two, [S.s6, S.s7]),
               (Nat.three, [S.s8]),
               (Nat.four, [S.s9]),
               (Nat.five, [S.s10])
              ]