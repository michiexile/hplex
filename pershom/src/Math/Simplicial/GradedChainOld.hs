{-# LANGUAGE ScopedTypeVariables #-} -- See SO question 4041111.
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Chain complexes graded over the natural numbers.
module GradedChain where

import qualified Monoid as M
import qualified AbelianMonoid as AbM
import qualified Ring as R
import qualified Group as G
import qualified Simplex as S
import qualified Module as Mod
import qualified Nat as Nat
import Parity
import Misc
import SimpleTypes
import qualified Data.Map as Map
import Data.List

-- | @'GradedChain' a b@ is a chain of @a@-simplices with
-- @b@-coefficients, graded over the natural numbers. Do not construct
-- directly, but use 'fromList' as the internal data structure may
-- change.
data GradedChain a b = C [(Nat.N, S.Simplex a, b)]

type CChainZ = GradedChain Char Z
type IChainZ = GradedChain Int Z

-- | Construct a chain from a list of triples @(d, s, c)@, where @d@
-- is the degree of the term with simplex @s@ having coefficient @c@.
fromList :: [(Nat.N, S.Simplex a, b)] -> GradedChain a b
fromList = C

-- | Extract the underlying list of triples, as described in
-- 'fromList'.
toList :: GradedChain a b -> [(Nat.N, S.Simplex a, b)]
toList (C x) = x

-- | Same as 'toList'.
coefficients :: GradedChain a b -> [(Nat.N, S.Simplex a, b)]
coefficients = toList

-- | See 'trimCoefficients'.
trim :: (Ord a,  R.Ring b) => GradedChain a b -> GradedChain a b
trim = C . trimCoefficients . toList

-- | 'trimCoefficients' takes a chain in list form and merges
-- duplicate entries and negative orientations into coefficients.  For
-- example, @'trim' [(0, "ba", 1)] == [(0, "ab", -1)]@. The operation
-- is quite expensive, roughly O(nlogn + n) in time, with @n@ the
-- length of the chain.  Implementation details: The function works by
-- first converting all negatively oriented simplices into positive
-- ones with opposite coefficient. Then a map is build for duplicate
-- merging. Finally, the map is converted back into a list, and any
-- null elements are removed.
trimCoefficients :: (Ord a, R.Ring b) => [(Nat.N, S.Simplex a, b)] -> [(Nat.N, S.Simplex a, b)]
trimCoefficients = filter (\ (_, s, c) -> c /= M.nil && (not . S.isNullSimplex) s) . 
                   (map unreshape) .
                   Map.toList . 
                   (Map.fromListWith (M.<+>)) . 
                   (map reshape) .
                   (map absorbParityIntoCoefficient) 
    where
      absorbParityIntoCoefficient (d, s, c)
          | S.parity s == Even = (d, s, c)
          | otherwise          = (d, S.reverse s, G.negative c)
      reshape (d, s, c) = ((d, s), c)
      unreshape ((d, s), c) = (d, s, c)
                                                  
instance (Ord a, R.Ring b) => Eq (GradedChain a b) where
    (C x) == (C y) = (trimCoefficients x) == (trimCoefficients y)

instance (Ord a, Show a, R.Ring b, Show b) => Show (GradedChain a b) where
    show x
        | x == M.nil = show (M.nil :: b)
        | otherwise  = ((drop 3) . (concatMap showTerm) . trimCoefficients . toList) x
        where
          showTerm (d, s, c) = " + " ++ (parenthesizedShow c) ++ "*" 
                                     ++ "deg{" ++ (show d) ++ "}"
                                     ++ (S.showSansSign s)

instance (Ord a, R.Ring b) => M.AdditiveMonoid (GradedChain a b) where
    (C x) <+> (C y) = C (x ++ y)
    nil = C []

instance (Ord a, R.Ring b) => G.AdditiveGroup (GradedChain a b) where
    negative (C x) = C (map (\ (d, s, c) -> (d, s, G.negative c)) x)

instance (Ord a, R.Ring b) => AbM.AbelianAdditiveMonoid (GradedChain a b)

-- | @'toHomogeneous' n c@ homogenizes the chain @c@ so that all terms
-- sit in degree @n@.
toHomogeneous :: Nat.N -> GradedChain a b -> GradedChain a b
toHomogeneous n (C x) = C $ map (\ (_, s, c) -> (n, s, c)) x

-- | Is the chain homogeneous? Warning: Involves trimming (see
-- 'trim'). FIXME: This function has never been tested.
homogeneous :: (Ord a, R.Ring b) => GradedChain a b -> Bool 
homogeneous = Misc.homogeneous . (map first3) . trimCoefficients . toList

-- | Degree of the first term in the chain. Useful to find the degree
-- of a homogeneous chain (see 'homogeneous').
firstTermDegree :: GradedChain a b -> Nat.N
firstTermDegree (C []) = Nat.zero
firstTermDegree (C (x:_)) = first3 x
                           
-- | A graded chain complex forms a module over the coefficient ring.
instance (Ord a, AbM.AbelianMultiplicativeMonoid b, R.Ring b) => Mod.Module b (GradedChain a b) where
    action r (C x) = (C . (map 
                           (\ (d, s, c) -> (d, s, r M.<*> c))
                          )) x

-- | A graded chain formed by placing a single simplex with unit
-- coefficient in the given degree.
fromSimplex :: (R.Ring b) => Nat.N -> S.Simplex a -> GradedChain a b
fromSimplex d s = C [(d, s, M.one)]

-- | @'coefficientOf' s c@ is the coefficient of the simplex @s@ in
-- the graded chain @c@. 
coefficientOf :: (Eq a, R.Ring b) => S.Simplex a -> GradedChain a b -> b
coefficientOf simplex (C terms) = case find (\ (_, s, _) -> s == simplex) terms of
                          Just (_, _, c) -> c
                          Nothing -> M.nil

simplices :: GradedChain a b -> [S.Simplex a]
simplices = (map middle3) . toList
                           
-- | The boundary of a simplex. The degree of the simplex must be
-- provided, as this will also be the degree of its boundary
-- components.
simplexBoundary :: (R.Ring b) => Nat.N -> S.Simplex a -> GradedChain a b
simplexBoundary d s = C $
                      map (\k -> (d, S.remove k s, M.one)) 
                      (takeWhile (<= S.dimension s) Nat.naturals)

-- | The boundary of a graded chain
boundary :: (Ord a, R.Ring b, AbM.AbelianMultiplicativeMonoid b) => GradedChain a b -> GradedChain a b
boundary = M.sum . 
           (map (\ (d, s, c) -> c Mod.*> (simplexBoundary d s) )) .
           toList

-- | Filter on terms.
filterTerms :: (Nat.N -> S.Simplex a -> b -> Bool) -> GradedChain a b -> GradedChain a b
filterTerms p (C ss) = C (filter (uncurry3 p) ss)


-- | Determine if a chain is the null-chain. Warning: Involves
-- trimming. See 'trim'.
isNull :: (Ord a, R.Ring b) => GradedChain a b -> Bool
isNull = isNull' . trim
    where
      isNull' (C (_:_)) = False
      isNull' (C []) = True

-- -- Testing

c10 :: IChainZ
c10 = fromSimplex (Nat.fromInt 4) (S.fromAscendingList Even [0..10]) 

c1000 :: IChainZ
c1000 = fromSimplex (Nat.fromInt 4) (S.fromAscendingList Even [0..1000]) 

c5000 :: IChainZ
c5000 = fromSimplex (Nat.fromInt 4) (S.fromAscendingList Even [0..5000]) 

-- benchmark :: IChainZt
-- benchmark = boundary c1000


