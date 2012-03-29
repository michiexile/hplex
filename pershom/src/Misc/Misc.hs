-- | Stuff that doesn't belong anywhere in particular. Some of it is
-- very ugly.

module Misc.Misc where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Data.List
import Data.Maybe
import System.IO

-- | A version of 'zipWith' that continues until the biggest list is
-- exhausted. @'zipWithDefault' f x0 y0 x y@ is just like @'zipWith' f
-- x y@, except that @x0@ is used in place of elements from @x@ when
-- the latter is exhausted, and likewise for @y0@.
zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault f x0 _ [] ys = zipWith f (repeat x0) ys
zipWithDefault f _ y0 xs [] = zipWith f xs (repeat y0)
zipWithDefault f x0 y0 (x:xs) (y:ys) = (f x y) : (zipWithDefault f x0 y0 xs ys)

-- | @zipDefault = zipWithDefault '(,)'@.
zipDefault :: a -> b -> [a] -> [b] -> [(a,b)]
zipDefault = zipWithDefault (,)

-- | @'longerThan' m x@ checks whether the list @x@ has length strictly greater
-- than @m@ in a maximum of @m@ steps instead of @'length' x@ steps.
longerThan :: Int -> [a] -> Bool
longerThan m = (>m) . length . (take (m+1))

-- | @'reverseShow' = 'reverse' . 'show'@
reverseShow :: (Show a) => a -> String
reverseShow = reverse . show

-- | Integer-based 'length', we'll call it 'longth'. Hoho.
longth :: [a] -> Integer
longth = foldr (\_ m -> m + 1) 0

-- | Faster 'Data.Map.unionWith' if the function passed is symmetric.
symmetricUnionWith :: (Ord k) => (a -> a -> a) -> Map.Map k a -> Map.Map k a -> Map.Map k a
symmetricUnionWith f x y
    | Map.size x < Map.size y = Map.unionWith f y x
    | otherwise               = Map.unionWith f x y

-- | Sort a list of pairs by the first element in the pairs.
sortByFirst :: (Ord a) => [(a, b)] -> [(a, b)]
sortByFirst = sortBy (\ (x1,x2) (y1,y2) -> compare x1 y1)

-- | Cartesian product of two lists.
cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys = [(x,y) | x <- xs, y <- ys]

-- | Parenthesize a multi-word string. A string is considered
-- /multi-word/ if it contains at least one space (as determined by
-- 'isSpace').
parenthesizedShow :: (Show a) => a -> String
parenthesizedShow x = if ((longerThan 0) . (filter isSpace) . show) x
                      then "(" ++ (show x) ++ ")"
                      else show x

-- | @'tail\''@ is a @'tail'@ that works on empty lists. @'tail\'' [] = []@.
tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

-- | @'head\''@ is a @'head'@ that works on empty lists. @'head\'' [] =
-- Nothing@.
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x

-- | @'head\'\''@ is a @'head'@ that works on empty lists, using lists
-- to simulate 'Maybe'. Thus you can safely write @foo ++ ('head\'\''
-- bar)@.
head'' :: [a] -> [a]
head'' [] = []
head'' (x:_) = [x]

-- | Check whether two lists are equal in @O(shortest list)@ time.
sameLength :: [a] -> [a] -> Bool
sameLength [] [] = True
sameLength [] (_:_) = False
sameLength (_:_) [] = False
sameLength (_:xs) (_:ys) = sameLength xs ys

-- | @'lookupWithAlternative' k k' m@ looks up @k@ in the map @m@. If
-- it is found, then fine. If not then assume that @k'@ /is/ in the
-- map and get it unconditionally.
lookupWithAlternative :: (Ord a) => a -> a -> Map.Map a b -> b
lookupWithAlternative k k' m = case (Map.lookup k m) of
                                 Just x -> x
                                 Nothing -> m Map.! k'

-- | Power set.
powerSet :: (Ord a) => Set.Set a -> Set.Set (Set.Set a)
powerSet = Set.fromList . (map Set.fromList) . subsequences . Set.toList

-- | Strip whitespace from front.
stripStart :: String -> String
stripStart = dropWhile isSpace

-- | Pairs.
pairs :: [a] -> [(a,a)]
pairs l = cartesian l l

-- | Pairs, with @(x,y)@ considered equal to @(y,x)@.
symmetricPairs :: [a] -> [(a,a)]
symmetricPairs [] = []
symmetricPairs (x:xs) = ([(x,y) | y <- (x:xs)]) ++ (symmetricPairs xs)

-- | Like 'symmetricPairs', only excluding pairs of the form @(x,x)@.
symmetricPairs' :: [a] -> [(a,a)]
symmetricPairs' [] = []
symmetricPairs' (x:xs) = ([(x,y) | y <- xs]) ++ (symmetricPairs' xs)

-- | @'removeAt' n xs@ removes the @n@'th element from @xs@.
removeAt :: Int -> [a] -> [a]
removeAt n x = (take n x) ++ (drop (n+1) x)

-- | XOR.
infixr 2 |||
(|||) :: Bool -> Bool -> Bool
True ||| True = False
True ||| False = True
False ||| True = True
False ||| False = False

-- | A list is called homogeneous if it only contains a single element
-- (any number of times).
homogeneous :: (Eq a) => [a] -> Bool 
homogeneous [] = True
homogeneous (x:xs) = (null . (filter (/= x))) xs

-- | Same as NumPy's arange.
arange :: Double -> Double -> Double -> [Double]
arange start stop step = takeWhile (< stop) (map (start +) (map (step *) [0,1..]))

-- | Same as NumPy's linspace.
linspace :: Double -> Double -> Int -> [Double]
linspace start stop num = go num start
    where
      delta = (stop - start) / (fromIntegral num - 1)
      go 1 _ = [stop]
      go n s = s:(go (n-1) (s + delta))

-- | Same as 'unlines', except the final newline is not added.
unlines' :: [String] -> String
unlines' [] = []
unlines' [l] = l
unlines' (l:ls) = l ++ ('\n':(unlines' ls))

choose :: [a] -> Int -> [[a]]
_ `choose` 0 = [[]]
xs `choose` n = [ y:ys | y:xs' <- tails xs, ys <- xs' `choose` (n-1)]

-- | @mapWhile f p x = ('takeWhile' p) ('map' f x)@.
mapWhile :: (a -> b) -> (b -> Bool) -> [a] -> [b]
mapWhile f p = (takeWhile p) . (map f)

-- | @mapSpan f p x@ results in a pair whose first component is @map
-- f@ of those elements of @x@ for which @p (f x)@ is true, and whose
-- second component is the (unprocessed) remainder of @x@.
mapSpan :: (a -> b) -> (b -> Bool) -> [a] -> ([b], [a])
mapSpan f p = go [] []
    where
      go l r [] = (l, r)
      go l _ (x:xs)
          | p y = go (y:l) [] xs
          | otherwise = (reverse l, x:xs)
          where
            y = f x

-- | @compose [f1, f2, ..., fn] = f1 . f2 . ... fn@.
compose :: [(a -> a)] -> a -> a
compose = foldr (.) id

-- | A poor-man's zipper. @'zipper' f [1,2,3...n] = [f [] [1,2,...n],
-- f [1] [2,,...n], ..., f [1,2..n-1] [n]]@.
zipper :: ([a] -> [a] -> b) -> [a] -> [b]
zipper f xs = map (uncurry f) (zip (inits xs) ((init . tails) xs))

-- | An alternative to 'zipper' that does not respect the ordering of
-- of the elements in the "left" list.
zipper' :: ([a] -> [a] -> b) -> [a] -> [b]
zipper' _ [] = []
zipper' f xs = go [] xs
    where
      go _ [] = []
      go l (y:ys) = (f l (y:ys)):(go (y:l) ys)


-- | @'maximumByList ord xs'@ is the largest element in @xs@ as
-- defined by the strict ordering given by @ord@ (the first element of
-- @ord@ is defined to be strictly smaller than the next). Conditions:
-- @xs@ must be a non-empty subset of @ord@, and the latter must be
-- finite and contain no duplicates. 
maximumByList :: (Eq a) => [a] -> [a] -> a
maximumByList ordering elements = go ordering elements (head elements)
    where
      go [] _ largest = largest
      go _ [] largest = largest
      go order (x:xs) largest 
          | null r    = go l xs largest
          | otherwise = go r xs x
          where
            (l, r) = break (== x) order

quicksort :: (Ord a) => [a] -> [a]
quicksort = quicksortBy compare

quicksortBy :: (a -> a -> Ordering) -> [a] -> [a]
quicksortBy _ [] = []
quicksortBy comp (x:xs) = quicksortBy comp smaller ++ [x] ++ quicksortBy comp larger
    where
      smaller = [y | y <- xs,  y `comp` x == LT]
      larger = [y | y <- xs,  y `comp` x == GT || y `comp` x == EQ]

smallest :: (Ord a) => Int -> [a] -> [a]
smallest = smallestBy compare

smallest' :: (Ord a) => Int -> [a] -> a
smallest' n xs = last (smallest n xs)

smallestBy :: (a -> a -> Ordering) -> Int -> [a] -> [a]
smallestBy comp n xs = take n (sortBy comp xs)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

first3 :: (a, b, c) -> a
first3 (x, _, _) = x

middle3 :: (a, b, c) -> b
middle3 (_, y, _) = y

last3 :: (a, b, c) -> c
last3 (_, _, z) = z

first5 :: (a,b,c,d,e) -> a
first5 (x, _, _, _, _) = x

second5 :: (a,b,c,d,e) -> b
second5 (_, x, _, _, _) = x

third5 :: (a,b,c,d,e) -> c
third5 (_, _, x, _, _) = x

fourth5 :: (a,b,c,d,e) -> d
fourth5 (_, _, _, x, _) = x

fifth5 :: (a,b,c,d,e) -> e
fifth5 (_, _, _, _, x) = x


errStr :: String -> IO ()
errStr = hPutStr stderr

errStrLn :: String -> IO ()
errStrLn = hPutStrLn stderr