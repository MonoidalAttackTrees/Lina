module Lina.QuickCheck where

import Test.QuickCheck

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

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (BinTree a)
arbitrarySizedTree m = do
  t <- arbitrary
  n <- choose (0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
  return (BinTree t ts)
  
  
--Testing Slack
