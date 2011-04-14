module Math.Plex (Polynomial, 
    Simplex, 
    Homology) where

import Data.Map (Map, (!))

type Polynomial k s = Map k s

class Simplex s where
  face :: Int -> s -> s

class Simplex s => Homology s where
  homology :: Int -> s -> Polynomial k s


