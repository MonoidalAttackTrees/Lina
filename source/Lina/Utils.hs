module Lina.Utils where

import qualified Data.Map as M
import qualified Data.Bimap as B

type ID = Integer

union :: (Ord a,Ord b) => B.Bimap a b -> B.Bimap a b -> B.Bimap a b
union m1 m2 = B.fromList.M.toList $ mm1 `M.union` mm2
 where
   mm1 = B.toMap m1
   mm2 = B.toMap m2  

