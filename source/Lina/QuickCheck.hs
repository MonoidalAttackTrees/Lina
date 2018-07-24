{-# LANGUAGE FlexibleInstances #-}
module Lina.QuickCheck where

import Test.QuickCheck
import System.Random

data BinTree a = EmptyTree | Node a (BinTree a) (BinTree a)
  deriving (Show, Read, Eq)

singleInsert :: a -> BinTree a
singleInsert x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> BinTree a -> BinTree a
treeInsert x EmptyTree = singleInsert x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

instance Arbitrary (BinTree Int) where
  arbitrary = sized arbitrarySizedTree 

arbitrarySizedTree :: Int -> Gen (BinTree Int)
arbitrarySizedTree 0 = return EmptyTree
arbitrarySizedTree 1 = do
  d <- arbitrary :: Gen Int
  return $ Node d EmptyTree EmptyTree
arbitrarySizedTree m = do 
  b <- arbitrary :: Gen Int
  left <- choose(0, m-1)
  right <- choose(0, m-1)
  leftBranch <- arbitrarySizedTree (left)
  rightBranch <- arbitrarySizedTree (right)
  return $ Node b leftBranch rightBranch
  

