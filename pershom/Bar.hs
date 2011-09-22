-- FIXME: Should probably be merged with the Barcode module.
module Bar where

import qualified NatInfinity as NatInf
import qualified Nat as Nat

-- | A bar in a barcode. Construct with 'bar'.
data Bar = C NatInf.NInfinity NatInf.NInfinity
           deriving (Eq, Ord)

instance Show Bar where
    show (C l u) = "[" ++ show l ++ ", " ++ show u ++ ")"

javaPlexShow :: Bar -> String
javaPlexShow (C l NatInf.Infinity) = "[" ++ show l ++ ", infinity)"
javaPlexShow (C l r) = "[" ++ show l ++ ", " ++ show r ++ ")"


-- | 'bar @l u@' constructs the bar @[l, u]@. If @l > u@, then
-- the bar @[u, l]@ is created instead.
bar :: NatInf.NInfinity -> NatInf.NInfinity -> Bar
bar l u
    | l <= u = C l u
    | otherwise = C u l


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
