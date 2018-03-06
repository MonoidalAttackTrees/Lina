module Lina.Utils where

import qualified Data.Map as M
import qualified Data.Bimap as B
import Data.List

type ID = Integer

union :: (Ord a,Ord b) => B.Bimap a b -> B.Bimap a b -> B.Bimap a b
union m1 m2 = B.fromList.M.toList $ mm1 `M.union` mm2
 where
   mm1 = B.toMap m1
   mm2 = B.toMap m2  

minimums :: Ord a => [a] -> [a]
minimums [] = []
minimums l@(x:_) = snd $ foldl aux (x,[]) l
 where
   aux :: Ord a => (a,[a]) -> a -> (a,[a])
   aux (m,l) x | x < m  = (x,[x])
               | x == m = (m,x:l)
               | otherwise = (m,l)

maximums :: Ord a => [a] -> [a]
maximums [] = []
maximums l@(x:_) = snd $ foldl aux (x,[]) l
 where
   aux :: Ord a => (a,[a]) -> a -> (a,[a])
   aux (m,l) x | x > m  = (x,[x])
               | x == m = (m,x:l)
               | otherwise = (m,l)               

