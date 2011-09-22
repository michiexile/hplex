{-# LANGUAGE ScopedTypeVariables #-} 
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
data GradedChain a b = C (Map.Map (Nat.N, S.Simplex a) b)

type CChainZ = GradedChain Char Z
type IChainZ = GradedChain Int Z

-- | Construct a chain from a list of triples @(d, s, c)@, where @d@
-- is the degree of the term with simplex @s@ having coefficient @c@.
fromList :: (Ord a, R.Ring b) => [(Nat.N, S.Simplex a, b)] -> GradedChain a b
fromList terms = C (Map.fromListWith (M.<+>) (map reshape terms))
    where
      reshape (d, s, c) 
          | S.parity s == Even = ((d, s), c)
          | otherwise          = ((d, S.reverse s), G.negative c)

-- | Extract the underlying list of triples, as described in
-- 'fromList'.
toList :: (Ord a) => GradedChain a b -> [(Nat.N, S.Simplex a, b)]
toList (C terms) = map reshape (Map.toList terms)
    where
      reshape ((x,y), z) = (x,y,z)

simplices :: (Ord a, R.Ring b) => GradedChain a b -> [S.Simplex a]
simplices (C x) = map snd (Map.keys (trimCoefficients x))

simplices' :: (Ord a, R.Ring b) => GradedChain a b -> [(Nat.N, S.Simplex a)]
simplices' (C x) = Map.keys (trimCoefficients x)

-- | A graded chain formed by placing a single simplex with unit
-- coefficient in the given degree.
fromSimplex :: (R.Ring b) => Nat.N -> S.Simplex a -> GradedChain a b
fromSimplex d s
    | S.parity s == Even = C (Map.singleton (d, s) M.one)
    | otherwise          = C (Map.singleton (d, S.reverse s) (G.negative M.one))

-- | Extract the underlying map.
coefficients :: GradedChain a b -> Map.Map (Nat.N, S.Simplex a) b
coefficients (C x) = x

-- | See 'trimCoefficients'.
trim :: (Ord a,  R.Ring b) => GradedChain a b -> GradedChain a b
trim = C . trimCoefficients . coefficients

-- | 'trimCoefficients' takes a chain in map form and removes zero entries. O(n).
trimCoefficients :: (Ord a, R.Ring b) => Map.Map (Nat.N, S.Simplex a) b -> Map.Map (Nat.N, S.Simplex a) b
trimCoefficients = Map.filterWithKey (\ (_, s) c -> not (S.isNullSimplex s || c == M.nil))

isNullChain :: (Ord a, R.Ring b) => GradedChain a b -> Bool
isNullChain c = null $ filter (\ (_, s, c) -> not (S.isNullSimplex s || c == M.nil)) (toList c)
                                               
instance (Ord a, R.Ring b) => Eq (GradedChain a b) where
    (C x) == (C y) = (trimCoefficients x) == (trimCoefficients y)

instance (Ord a, Show a, R.Ring b, Show b) => Show (GradedChain a b) where
    show x
        | x == M.nil = show (M.nil :: b)
        | otherwise  = ((drop 3) . (concatMap showTerm) . toList . trim) x
        where
          showTerm (d, s, c) = " + " ++ (parenthesizedShow c) ++ "*" 
                                     ++ "deg{" ++ (show d) ++ "}"
                                     ++ (S.showSansSign s)

instance (Ord a, R.Ring b) => M.AdditiveMonoid (GradedChain a b) where
    (C x) <+> (C y) = C (symmetricUnionWith (M.<+>) x y)
    nil = C Map.empty

instance (Ord a, R.Ring b) => G.AdditiveGroup (GradedChain a b) where
    negative (C x) = C (Map.map G.negative x)

instance (Ord a, R.Ring b) => AbM.AbelianAdditiveMonoid (GradedChain a b)

-- | A graded chain complex forms a module over the coefficient ring.
instance (Ord a, AbM.AbelianMultiplicativeMonoid b, R.Ring b) => Mod.Module b (GradedChain a b) where
    action r (C x) = C (Map.map (r M.<*>) x)

-- | The boundary of a simplex. The degree of the simplex must be
-- provided, as this will also be the degree of its boundary
-- components.
simplexBoundary :: (Ord a, R.Ring b) => Nat.N -> S.Simplex a -> GradedChain a b
simplexBoundary d s = fromList $
                      map (\k -> (d, S.remove k s, M.one)) 
                      (takeWhile (<= S.dimension s) Nat.naturals)

-- | The boundary of a graded chain
boundary :: (Ord a, R.Ring b, AbM.AbelianMultiplicativeMonoid b) => GradedChain a b -> GradedChain a b
boundary = M.sum . 
           (map (\ (d, s, c) -> c Mod.*> (simplexBoundary d s))) .
           toList

-- | @'coefficientOf' d s c@ is the coefficient of the simplex @s@ in
-- degree @d@ in the graded chain @c@.
coefficientOf :: (Ord a, R.Ring b) => Nat.N -> S.Simplex a -> GradedChain a b -> b
coefficientOf deg simplex (C terms) = Map.findWithDefault M.nil (deg, simplex) terms

-- | @'homogeneousCoefficientOf' s c@ is the coefficient of the simplex @s@ in
-- the homogeneous graded chain @c@. The condition that @c@ is homogeneous is not checked.
homogeneousCoefficientOf :: (Ord a, R.Ring b) => S.Simplex a -> GradedChain a b -> b
homogeneousCoefficientOf simplex c@(C terms) = Map.findWithDefault M.nil (homogeneousDegree c, simplex) terms

-- | Filter on terms.
filterTerms :: (Ord a) => (Nat.N -> S.Simplex a -> b -> Bool) -> GradedChain a b -> GradedChain a b
filterTerms p (C terms) = C (Map.filterWithKey p' terms)
    where
      p' (d, s) c = p d s c

-- | Is the chain homogeneous? For consistency, we define the empty
-- chain to be non-homogeneous.
homogeneous :: (Ord a, R.Ring b) => GradedChain a b -> Bool
homogeneous c
    | isNullChain c = False
    | otherwise = Misc.homogeneous (map first3 (toList c))

-- | If the chain is homogeneous, something that is not checked, this
-- is its degree. If the chain is not homogeneous, the result is
-- non-sensical.
homogeneousDegree :: (Ord a, R.Ring b) => GradedChain a b -> Nat.N
homogeneousDegree (C terms) = (fst . fst) (Map.elemAt 0 terms)

-- | 'homogenize d c' is a version of the chain 'c' with all terms in
-- degree 'd'. Linear in time.
homogenize :: (Ord a, R.Ring b) => Nat.N -> GradedChain a b -> GradedChain a b
homogenize d (C x) = C $ Map.mapKeysWith (M.<+>) (\ (_, s) -> (d, s) ) x

---- Testing

c10 :: IChainZ
c10 = fromSimplex (Nat.fromInt 5) (S.fromAscendingList Even [0..10]) 

c1000 :: IChainZ
c1000 = fromSimplex (Nat.fromInt 4) (S.fromAscendingList Even [0..1000]) 

c5000 :: IChainZ
c5000 = fromSimplex (Nat.fromInt 4) (S.fromAscendingList Even [0..5000]) 

-- benchmark :: IChainZt
-- benchmark = boundary c1000


