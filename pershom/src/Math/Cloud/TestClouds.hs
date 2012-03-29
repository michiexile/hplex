module TestClouds where

import qualified Euclidean as Euc
import Cloud

house :: Cloud Euc.Euclidean
house = [Euc.fromList [-1,0], Euc.fromList [1,0],
         Euc.fromList [-1,2], Euc.fromList [1,2],
         Euc.fromList [0,3]]

subHouse1 :: Cloud Euc.Euclidean
subHouse1 = [Euc.fromList [-1,0], Euc.fromList [1,0], Euc.fromList [0,3]]