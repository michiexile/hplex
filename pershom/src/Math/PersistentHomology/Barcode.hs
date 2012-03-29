{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.PersistentHomology.Barcode where

import qualified Math.Algebra.AbelianMonoid as AbM
import qualified Math.Misc.Nat as Nat
import qualified Data.Vector as Vect
import Math.Misc.PlusInfinity
import qualified Math.VectorSpaces.PseudoMetric as PMet
import qualified Math.PersistentHomology.Bar as B
import qualified Data.List as L
import Misc.Misc
import Math.Misc.SimpleTypes

-- FIXME: Check out Control.Newtype. Helps to get rid of newtype boilerplate.
newtype Barcode a = Barcode [B.Bar a]
    deriving (Eq)

instance (Eq a, AbM.AbelianAdditiveMonoid a, Ord a) => PMet.PseudoMetric (Barcode a) a where
    distance bs cs = undefined -- FIXME: This is not yet done. Complete using the Hungarian method.
                               -- For now, just do the barcode distances in post-processing in some other
                               -- language

instance (Show a) => Show (Barcode a) where
    show (Barcode bs) = unlines' (map show bs)

bars :: Barcode a -> [B.Bar a]
bars (Barcode bs) = bs

javaPlexShow :: (Show a) => Barcode a -> String
javaPlexShow (Barcode bs) = unlines' (map B.javaPlexShow bs)

compactShow :: (Show a) => Barcode a -> String
compactShow (Barcode bs) = unlines' (map B.compactShow bs)

infixr 5 ++
(++) :: Barcode a -> Barcode a -> Barcode a
(Barcode x) ++ (Barcode y) = Barcode (x Prelude.++ y)

timeIndex :: (Ord a) => Vect.Vector a -> Barcode Nat.N -> Barcode a
timeIndex ts (Barcode bs) = Barcode $ map (B.reIndex (\n -> ts Vect.! (Nat.toInt n))) bs

sort :: (Ord a) => Barcode a -> Barcode a
sort (Barcode bs) = Barcode (L.sort bs)



-- Testing
test1 :: Barcode Int
test1 = Barcode [B.finiteBar 0 2, B.finiteBar 1 4, B.finiteBar 2 6, B.infiniteBar 2, B.finiteBar 7 9]

test2 :: Barcode Int
test2 = Barcode [B.finiteBar 0 3, B.finiteBar 2 4, B.finiteBar 6 7, B.infiniteBar 9]