-- FIXME: Should probably be merged with the Barcode module.
module Math.PersistentHomology.Bar where

import Math.Misc.PlusInfinity
import qualified Math.VectorSpaces.Metric2 as Met

-- | A bar in a barcode. Construct with 'bar'.
data Bar a = C a (PlusInfinity a)
             deriving (Eq)

-- | 'bar @l u@' constructs the bar @[l, u]@. If @l > u@, then
-- the bar @[l, l]@ is created instead.
bar :: (Ord a) => a -> PlusInfinity a -> Bar a
bar l u
    | (Regular l) <= u = C l u
    | otherwise = C l (Regular l)

instance (Ord a) => Ord (Bar a) where
    compare (C l1 u1) (C l2 u2) = compare (l1, u1) (l2, u2)

instance (Show a) => Show (Bar a) where
    show (C l u) = "[" ++ show l ++ ", " ++ show u ++ ")"

javaPlexShow :: (Show a) => Bar a -> String
javaPlexShow (C l Infinity) = "[" ++ show l ++ ", infinity)"
javaPlexShow (C l (Regular r)) = "[" ++ show l ++ ", " ++ show r ++ ")"

compactShow :: (Show a) => Bar a -> String
compactShow (C l1 (Regular u1)) = show l1 ++ " " ++ show u1
compactShow (C l1 Infinity) = show l1 ++ " inf"

finiteBar :: (Ord a) => a -> a -> Bar a
finiteBar l u = bar l (Regular u)

infiniteBar :: (Ord a) => a -> Bar a
infiniteBar l = bar l Infinity

reIndex :: (Ord a, Ord b) => (a -> b) -> Bar a -> Bar b
reIndex f (C l Infinity) = C (f l) Infinity
reIndex f (C l (Regular u)) = C (f l) (Regular (f u))

intersection :: (Ord a) => Bar a -> Bar a -> Bar a
intersection (C l1 Infinity) (C l2 Infinity) = infiniteBar (max l1 l2)
intersection (C l1 Infinity) (C l2 (Regular u2)) = finiteBar (max l1 l2) u2
intersection (C l1 (Regular u1)) (C l2 Infinity) = finiteBar (max l1 l2) u1
intersection (C l1 (Regular u1)) (C l2 (Regular u2)) = finiteBar (max l1 l2) (min u1 u2)

measure :: (Met.Metric a a) => Bar a -> PlusInfinity a
measure (C _ Infinity) = Infinity
measure (C l (Regular u)) = Regular (Met.distance l u)