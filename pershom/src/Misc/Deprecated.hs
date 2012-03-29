-- | Find the column indices of the two smallest entries in a given
-- row.
minIndex2Row :: (Vect.Unbox a, Ord a) => Matrix a -> Int -> (Int, Int)
minIndex2Row x k
    | numColumns x < 2      = error "minIndex2Row called on a matrix of width < 2."
    | otherwise             = Vect.ifoldl' helper (initial1, initial2) (Vect.drop 2 r)
    where
      r = x `row` k
      (initial1, initial2) = if r Vect.! 0 <= r Vect.! 1
                             then (0,1)
                             else (1,0)
      helper (i1, i2) i x
          | x < r Vect.! i1 = (i + 2, i1)
          | x < r Vect.! i2 = (i1, i + 2)
          | otherwise       = (i1, i2)

-- | FIXME: Move this to VectorUtils once that module has been
-- converted to a generic interface. The number of elements to track
-- should also be a variable, and the return type should be a vector.
smallest2InVector :: (Vect.Unbox a, Ord a) => Vect.Vector a -> (a,a)
smallest2InVector x 
    | Vect.length x < 2 = error "FIXME"
    | otherwise         = Vect.foldl' helper (initial1, initial2) (Vect.drop 2 x)
    where
      (initial1, initial2) = if x Vect.! 0 <= x Vect.! 1
                             then (x Vect.! 0, x Vect.! 1)
                             else (x Vect.! 1, x Vect.! 0)
      helper (y1, y2) y
          | y < y1 = (y, y1)
          | y < y2 = (y1, y)
          | otherwise  = (y1, y2)

smallest2InRow :: (Vect.Unbox a, Ord a) => Matrix a -> Int -> (a,a)
smallest2InRow x k = smallest2InVector (x `row` k)

-- | Find the column indices of the smallest entry in a given row.
minIndexRow :: (Vect.Unbox a, Ord a) => Matrix a -> Int -> Int
minIndexRow x k = Vect.minIndex (x `row` k)


scaleSplit :: Double -> [[(S.Simplex Int, Double)]] -> ([[(S.Simplex Int, Double)]], [[(S.Simplex Int, Double)]])
scaleSplit scale xs = (reverse a, reverse r)
    where
      (a, r) = scaleSplit' [] xs
      scaleSplit' :: [[(S.Simplex Int, Double)]] ->
                     [[(S.Simplex Int, Double)]] -> 
                     ([[(S.Simplex Int, Double)]], [[(S.Simplex Int, Double)]])
      scaleSplit' accepted [] = (accepted, [])
      scaleSplit' accepted (cand:cands)
          | (not . null) reject = ((accept:accepted), (reject:cands))
          | otherwise = scaleSplit' (accept:accepted) cands
          where
            (accept, reject) = partition (\(_, w) -> w <= scale) cand