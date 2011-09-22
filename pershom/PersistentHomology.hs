{-# LANGUAGE ScopedTypeVariables #-} -- See SO question 4041111. 

module PersistentHomology where

import qualified Data.Map as Map
import qualified Nat as Nat
import qualified Simplex as S
import qualified GradedChain as Ch
import qualified Field as F
import qualified FilteredComplex as FC
import qualified Group as G
import qualified Module as Mod
import qualified Barcode as BC
import qualified Bar as B
import qualified NatInfinity as NatInf
import Data.Maybe
import Data.List
import SimpleTypes
import qualified Monoid as M



data Slot a b = Slot
                {
                  highestInBoundaryOfDegree :: Nat.N ,
                  highestInBoundaryOf :: S.Simplex a ,
                  highestInBoundary :: Ch.GradedChain a b
                }

data Entry a b = Entry
    {
      degree :: Nat.N ,  
      marked :: Bool ,
      slot :: Maybe (Slot a b)
    }

isHighestInBoundary :: Entry a b -> Bool
isHighestInBoundary = isJust . slot

slotEmpty :: Entry a b -> Bool
slotEmpty = not . isHighestInBoundary

-- | If you know that the 'Entry' has a 'Slot' (not 'Nothing'), then
-- 'highestInBoundary'' extracts that 'Slot''s 'highestInBoundary'
-- field.
highestInBoundary' :: Entry a b -> Ch.GradedChain a b
highestInBoundary' = highestInBoundary . fromJust . slot

highestInBoundaryOfDegree' :: Entry a b -> Nat.N
highestInBoundaryOfDegree' = highestInBoundaryOfDegree . fromJust . slot

type PersistentHomology a b = Map.Map (S.Simplex a) (Entry a b)

removeUnmarked :: (Ord a) => PersistentHomology a b -> Ch.GradedChain a b -> Ch.GradedChain a b
removeUnmarked hom = Ch.filterTerms p
    where
      p _ s _ = case (Map.lookup s hom) of
                  Just entry -> marked entry
                  Nothing -> True

reducedBoundary :: (Ord a, F.Field b) => PersistentHomology a b -> Nat.N -> S.Simplex a -> Ch.GradedChain a b
reducedBoundary homology degree simplex = go (removeUnmarked homology (Ch.simplexBoundary degree simplex))
    where
      go d
          | Ch.isNullChain d || S.isNullSimplex s || (not . isHighestInBoundary) t    = d
          | otherwise = go (d G.<-> 
                            (p F.</> q) Mod.*> 
                            Ch.homogenize (Ch.homogeneousDegree d) (highestInBoundary' t)
                           )
          where
            s = maxSimplex homology d
            t = homology Map.! s
            p = Ch.homogeneousCoefficientOf s d
            q = Ch.homogeneousCoefficientOf s (highestInBoundary' t)

persistentHomology :: (Ord a, F.Field b) => FC.FilteredComplex c a -> PersistentHomology a b
persistentHomology filtration = go Map.empty (FC.toList' filtration)
    where
      go homology [] = homology
      go homology ((deg, simp):simps) = go homology' simps
          where
            d = reducedBoundary homology deg simp
            s = maxSimplex homology d
            homology' = if Ch.isNullChain d
                        then Map.insert simp
                             Entry { degree = deg, marked = True, slot = Nothing } homology
                        else Map.insert simp
                             Entry { degree = deg, marked = False, slot = Nothing }
                             (Map.adjust ( \entry ->
                                           entry { slot = Just (Slot {highestInBoundaryOfDegree = deg,
                                                                      highestInBoundaryOf = simp,
                                                                      highestInBoundary = d}) }
                                         ) s homology)  

rationalPersistentHomology :: (Ord a) => FC.FilteredComplex b a -> PersistentHomology a Q
rationalPersistentHomology = persistentHomology

realPersistentHomology :: (Ord a) => FC.FilteredComplex b a -> PersistentHomology a R
realPersistentHomology = persistentHomology


-- | @'maxSimplex' h c@ is the simplex in @c@ that has the highest
-- order in the persistent homology structure @h@.
maxSimplex :: (Ord a, F.Field b) => PersistentHomology a b -> Ch.GradedChain a b -> S.Simplex a
maxSimplex h = snd . (maxSimplex' h)

-- | See 'maxSimplex'. This version gives the degree of the simplex as well.
maxSimplex' :: (Ord a, F.Field b) => PersistentHomology a b -> Ch.GradedChain a b -> (Nat.N, S.Simplex a)
maxSimplex' h c = maximumBy comparator (Ch.simplices' c)
    where 
      comparator (_, s1) (_, s2) = compare 
                                   (degree (h Map.! (S.evenVersion s1)), s1)
                                   (degree (h Map.! (S.evenVersion s2)), s2)


toBarcode :: (Ord a) => PersistentHomology a b -> Nat.N -> BC.Barcode
toBarcode homology topDimension = BC.fromMap $ Map.foldrWithKey helper Map.empty homology
    where
      helper simplex entry barcode
          | S.dimension simplex > topDimension = barcode
          | slotEmpty entry && marked entry = Map.insertWith (++) (S.dimension simplex)
                                              [B.bar 
                                               (NatInf.fromNat (degree entry)) NatInf.Infinity   ]
                                              barcode
          | isHighestInBoundary entry &&  degree entry /= highestInBoundaryOfDegree' entry
                                            = Map.insertWith (++) (S.dimension simplex)
                                              [B.bar 
                                               (NatInf.fromNat (degree entry))
                                               (NatInf.fromNat (highestInBoundaryOfDegree' entry))   ]
                                              barcode
          | otherwise                       = barcode
      
