{-# LANGUAGE TypeSynonymInstances #-}
module Interleaved where

import qualified Data.Set as Set
import Data.Set ((\\))
import Data.List (nub)
import qualified Data.Map as Map
import Data.Map ((!))

type Edge = (Int, Int)
type Clique = Set.Set Int

clique :: [Int] -> Clique
clique = Set.fromList

dim :: Clique -> Int
dim c = (Set.size c) - 1

data State = S {
      vertices :: Set.Set Int,
      edges :: [Edge],
      cliques :: [Clique]
    } deriving (Eq, Show)

state :: [Int] -> State
state ints = S (Set.fromList ints) [] (map Set.singleton ints)

addEdge :: Edge -> State -> State
addEdge e@(s,t) st = st { edges = e : edges st, cliques = newC }
    where
      sCliques = [c | c <- cliques st, s `Set.member` c]
      tCliques = [c | c <- cliques st, t `Set.member` c]
      newCliques = nub [Set.insert s (Set.insert t (Set.intersection c d)) | c <- sCliques, d <- tCliques]
      candidates = newCliques ++ cliques st
      newC = [c | c <- candidates, 
              null (filter (\s -> c `Set.isSubsetOf` s && c /= s) (candidates))]

facets :: Clique -> [Clique]
facets c = filter (not . Set.null) $ map ((c\\) . Set.singleton) (Set.toList c)

newFacets :: [Clique] -> Clique -> [Clique]
newFacets old c = filter (not . (\f -> any (f `Set.isSubsetOf`) old)) (facets c)

allNewFacets :: [Clique] -> Clique -> [Clique]
allNewFacets old c = nub $ newF ++ (concatMap (allNewFacets old) newF)
    where
      newF = newFacets old c

newSimplices :: Edge -> State -> (State, [Clique])
newSimplices e oldS = (newS, simpl)
    where
      newS = addEdge e oldS
      newF = [c | c <- cliques newS, not (c `elem` (cliques oldS))]
      simpl = nub $ newF ++ (concatMap (allNewFacets (cliques oldS)) newF)

type Vector = [Double]

type GraphMap = Map.Map Double [(Int,Int)]

l2d2 :: Vector -> Vector -> Double
l2d2 v w = sum . map (^2) $ zipWith (-) v w

weightedGraph :: (Vector -> Vector -> Double) -> [Vector] -> [(Double,Edge)]
weightedGraph d vectors = concatMap (\(d,es) -> map (\e -> (d,e)) es) . Map.toAscList $ foldr (addEdgeGM d vectors) Map.empty [(i,j) | i <- [0..(length vectors)-1], j <- [0..(length vectors)-1], i < j]

addEdgeGM :: 
  (Vector -> Vector -> Double) -> [Vector] -> (Int, Int) -> GraphMap -> GraphMap
addEdgeGM d vs (i,j) gm = Map.insertWith (++) (d (vs !! i) (vs !! j)) [(i,j)] gm

type Chain = Map.Map Clique Double
instance Num Chain where
    c + d = Map.filter (/=0) $ Map.unionWith (+) c d
    negate = Map.map negate
    c * d = Map.filter (/=0) $ Map.fromListWith (+) $ [(Set.union s t, x * y) | (s,x) <- Map.toList c, (t,y) <- Map.toList d]
    abs = undefined
    signum = undefined
    fromInteger n = Map.fromList [(Set.empty,fromInteger n)]

constantC :: Double -> Chain
constantC x = Map.fromList [(Set.empty,x)]

chain :: Clique -> Chain
chain c = Map.fromList [(c,1)]

boundaryC :: Clique -> Chain
boundaryC s = sum $ zipWith 
             (*) 
             (map chain (facets s)) 
             (map (fromInteger . ((negate 1)^)) [0..])

boundary :: Chain -> Chain
boundary s = sum $ map d keyvals
  where
    keyvals = Map.toList s
    d = \(k,v) -> (constantC v) * (boundaryC k)

data Interval = I { 
      birth :: Double,
      death :: Maybe Double,
      dimension :: Int
    } deriving (Eq, Show)

data HomologyState = H {
      marked :: Map.Map (Double,Clique) Chain,
      graph :: State,
      edgeQ :: [(Edge, Double)],
      simplexQ :: [Clique],
      intervals :: [Interval],
      simplexAges :: Map.Map Clique Double
      } deriving (Eq, Show)

initPH :: State -> [(Edge, Double)] -> HomologyState
initPH st gr = H { marked = Map.empty, 
                   graph = st, 
                   edgeQ = gr, 
                   simplexQ = map Set.singleton (Set.toList $ vertices st),
                   intervals = [],
                   simplexAges = Map.fromList . map (\v -> (v,0.0::Double)) . map Set.singleton . Set.toList . vertices $ st
              }


projectChain :: Chain -> [Clique] -> Chain
projectChain s ts = Map.filterWithKey (\k _ -> k `elem` ts) s


removePivots :: 
    Chain -> Map.Map (Double,Clique) Chain -> Map.Map Clique Double -> Chain
removePivots s ms sa = d 
    where
      pcs = projectChain s (map snd (Map.keys ms))
      d = reduceAll ms sa pcs

reduceAll :: 
    Map.Map (Double,Clique) Chain -> Map.Map Clique Double -> Chain -> Chain
reduceAll ms sa ch = 
    case Map.null ch of
      True -> ch 
      False -> case Map.findMax (Map.filterWithKey (\k a -> k `elem` (Map.keys ch)) sa) of
                 (k,v) -> case Map.lookup (v,k) ms of
                            Nothing -> ch
                            Just b | Map.null b -> ch
                                   | otherwise -> reduceAll ms sa (reduce k ch b)

reduce :: Clique -> Chain -> Chain -> Chain
reduce s p q = p - (constantC (1/(q!s)) * q)

processNext :: HomologyState -> HomologyState
processNext hs | null (simplexQ hs) && null (edgeQ hs) = hs
               | null (simplexQ hs) = let
    (e,t):eq = edgeQ hs
    g = graph hs
    sa = simplexAges hs
    (g',sq') = newSimplices e g
    sa' = Map.union sa (Map.fromList . map (\v -> (v,t)) $ sq')
  in hs { 
    graph = g', 
    simplexQ = reverse sq', 
    simplexAges = sa',
    edgeQ = eq
  }
               | otherwise = let
    s:sq = simplexQ hs
    m = marked hs
    sa = simplexAges hs
    d = removePivots (boundaryC s) m sa
    result | Map.null d = hs { marked = Map.insert (sa!s,s) Map.empty m, 
                               simplexQ = sq }
           | otherwise = hs { marked = m', intervals = i', simplexQ = sq }
           where
               Just ((c,coeff),_) = Map.maxViewWithKey d
               i' = (I { birth=sa!c, death=Just (sa!s), dimension=dim c}) : (intervals hs)
               m' = Map.insert (sa!c,c) d m
  in result

processN :: Int -> HomologyState -> HomologyState
processN 0 hs = hs
processN n hs = processN (n-1) (processNext hs)

process :: HomologyState -> HomologyState
process hs | nhs == hs = hs
           | otherwise = process nhs
           where
             nhs = processNext hs

allIntervals :: HomologyState -> [Interval]
allIntervals hs = intervals hs ++ ii
    where
      emptyMarked = Map.filter Map.null (marked hs)
      hoInterval :: ((Double,Clique),Chain) -> Interval
      hoInterval ((t,c),ch) = I { birth=t, death=Nothing, dimension=dim c}
      ii = map hoInterval (Map.toList emptyMarked)
