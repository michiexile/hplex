module Math.PersistentHomology.BarcodeCollection where

import qualified Math.PersistentHomology.Barcode as BC
import qualified Math.PersistentHomology.Bar as B
import qualified Data.Map as Map
import qualified Math.Misc.Nat as Nat
import qualified Data.Vector as Vect

data BarcodeCollection a = C (Map.Map Nat.N (BC.Barcode a))

dimension :: (Ord a) => BarcodeCollection a -> Nat.N -> BC.Barcode a
dimension (C x) d = Map.findWithDefault (BC.Barcode []) d x

dimension' :: (Ord a) => BarcodeCollection a -> Int -> BC.Barcode a
dimension' b d = dimension b (Nat.fromInt d)

fromMap :: (Ord a) => Map.Map Nat.N (BC.Barcode a) -> BarcodeCollection a
fromMap = C

timeIndex :: (Ord a) => Vect.Vector a -> BarcodeCollection Nat.N -> BarcodeCollection a
timeIndex ts (C x) = C (Map.map (BC.timeIndex ts) x)

-- | Untested.
fromList :: (Ord a) => [(Nat.N, B.Bar a)] -> BarcodeCollection a
fromList = C . (Map.fromListWith (BC.++)) . (map (\ (d, bs) -> (d, BC.Barcode [bs])))

sort :: (Ord a) => BarcodeCollection a -> BarcodeCollection a
sort (C x) = C $ Map.map BC.sort x

instance (Show a) => Show (BarcodeCollection a) where
    show (C x) = (init . unlines . (map showDimension) . Map.toAscList) x
        where
          showDimension (d, bs) = "Dimension " ++ show d ++ ":\n" ++ 
                                  show bs

javaPlexShow :: (Show a) => BarcodeCollection a -> String
javaPlexShow (C x) = (init . unlines . (map showDimension) . Map.toAscList) x
    where
      showDimension (d, bs) = "Dimension: " ++ (show d) ++ "\n" ++ 
                              BC.javaPlexShow bs

compactShow :: (Show a) => BarcodeCollection a -> String
compactShow (C x) = (init . unlines . (map showDimension) . Map.toAscList) x
        where
          showDimension (d, bs) = "Dim " ++ (show d) ++ "\n" ++ 
                                  BC.compactShow bs