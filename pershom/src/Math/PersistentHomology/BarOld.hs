-- FIXME: Should probably be merged with the Barcode module.
module Bar where

import PlusInfinity

-- | A bar in a barcode. Construct with 'bar'.
data Bar a = C a (PlusInfinity a)
             deriving (Eq)

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


-- | 'bar @l u@' constructs the bar @[l, u]@. If @l > u@, then
-- the bar @[l, l]@ is created instead.
bar :: (Ord a) => a -> PlusInfinity a -> Bar a
bar l u
    | (Regular l) <= u = C l u
    | otherwise = C l (Regular l)

finiteBar :: (Ord a) => a -> a -> Bar a
finiteBar l u = bar l (Regular u)

infiniteBar :: (Ord a) => a -> Bar a
infiniteBar l = bar l Infinity

reIndex :: (Ord a, Ord b) => (a -> b) -> Bar a -> Bar b
reIndex f (C l Infinity) = C (f l) Infinity
reIndex f (C l (Regular u)) = C (f l) (Regular (f u))


-- asciiArt :: Int -> Int -> Double -> Bar -> String
-- asciiArt w offset density (C l r) = replicate left ' ' ++
--                                     leftString ++
--                                     replicate (right' - left - length leftString - length rightString) '-' ++
--                                     rightString
                                  
--     where
--       leftString = show l ++ "|"
--       rightString = if right > w || r == NatInf.Infinity
--                     then ">>" ++ show r
--                     else "|" ++ show r
--       left = convert density ((NatInf.toInt l) + offset)
--       right = convert density ((NatInf.toInt r) + offset)
--       right' = if r == NatInf.Infinity
--                then w
--                else min w right
--       convert dens n = round ((fromIntegral n) / dens)
