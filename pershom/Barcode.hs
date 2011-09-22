module Barcode where

import qualified Nat as Nat
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Bar as B
import Misc

data Barcode = C (Map.Map Nat.N [B.Bar])

instance Show Barcode where
    show (C x) = (init . unlines . (map showDimension) . Map.toAscList) x
        where
          showDimension (d, bs) = "Dimension " ++ (show d) ++ ":\n" ++ 
                                  unlines' (map show bs)

javaPlexShow :: Barcode -> String
javaPlexShow (C x) = (init . unlines . (map showDimension) . Map.toAscList) x
    where
      showDimension (d, bs) = "Dimension: " ++ (show d) ++ "\n" ++ 
                              unlines' (map B.javaPlexShow bs)


-- | Untested.
fromList :: [(Nat.N, B.Bar)] -> Barcode
fromList = C . (Map.fromListWith (++)) . (map (\ (d, bs) -> (d, [bs])))

fromMap :: Map.Map Nat.N [B.Bar] -> Barcode
fromMap = C

sort :: Barcode -> Barcode
sort (C x) = C (Map.map L.sort x)

dimension :: Barcode -> Nat.N -> Barcode
dimension (C x) d = C $ case Map.lookup d x of
                          Just bs -> Map.singleton d bs
                          Nothing -> Map.empty

dimension' :: Barcode -> Int -> Barcode
dimension' b d = dimension b (Nat.fromInt d)

                              