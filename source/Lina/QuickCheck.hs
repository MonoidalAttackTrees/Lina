module Lina.QuickCheck where

import Test.QuickCheck

data BinTree b = EmptyTree | Node b (BinTree b) (BinTree b)
  deriving (Show, Read, Eq)

singleInsert :: a -> BinTree a
singleInsert x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> BinTree a -> BinTree a
treeInsert x EmptyTree = singleInsert x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

instance (Ord a, Bounded a, Random a, Num a, Arbitrary a) => Arbitrary (BinTree a) where
  arbitrary = gen 0 100 where
    gen :: (Ord a, Num a, Random a) => a -> a -> Gen (BinTree a)
    gen min max | (max - min) <= 3 = return EmptryTree
    gen min max = do
      
