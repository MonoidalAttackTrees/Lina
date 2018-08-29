{-# LANGUAGE FlexibleInstances #-}
module Lina.QuickCheck where

import Lina.Graphy
import Test.QuickCheck
import System.Random

data BinTree a = Leaf a
               | Node  a (BinTree a) (BinTree a)
               | NodeB a (BinTree a) (BinTree a)
  deriving(Show, Read, Eq)

showBT :: Gen (BinTree Int) -> String
showBT x = do
  undefined

showTree :: BinTree Int -> FilePath -> IO FilePath
showTree a f = mkImage Dot Jpeg 0.23 0.3 2 (createGraph a) (f) -- "BinTree.jpeg"

getData :: BinTree Int -> Int
getData (Leaf d) = d
getData (Node d _ _) = d
getData (NodeB d _ _) = d

createGraph :: BinTree Int -> [(Int,Int,Int)]
createGraph (Leaf d)      = []
createGraph (Node d l r)  =
  [(d,el,1),(d,er,1)] ++ lEdges ++ rEdges
 where
   el = getData l
   er = getData r
   lEdges = createGraph l
   rEdges = createGraph r
   
createGraph (NodeB d l r)  =
  [(d,el,1),(d,er,1)] ++ lEdges ++ rEdges
 where
   el = getData l
   er = getData r
   lEdges = createGraph l
   rEdges = createGraph r
  
createGraphGen :: Gen (BinTree Int) -> Gen [(Int,Int,Int)]
createGraphGen = fmap createGraph

instance Arbitrary (BinTree Int) where
  arbitrary = sized arbitrarySizedTree 

arbitrarySizedTree :: Int -> Gen (BinTree Int)
arbitrarySizedTree 1 = do
  d <- arbitrary :: Gen Int
  nodePick <- choose (0,1)  :: Gen Int
  if nodePick == 0 then return $ Leaf d
  else return $ Leaf d
  return $ Leaf d
arbitrarySizedTree m = ranNode Node m

ranNode :: (Int -> BinTree Int -> BinTree Int -> BinTree Int)
        -> Int
        -> Gen (BinTree Int)
ranNode op m = do 
  b <- arbitrary :: Gen Int
  left <- choose (0, m-1)
  leftBranch <- arbitrarySizedTree left
  rightBranch <- arbitrarySizedTree $ (m-1)-left
  return $ op b leftBranch rightBranch
