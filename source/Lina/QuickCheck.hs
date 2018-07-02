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
  arbitrary :: Gen(BinTree a)
  arbitrary = sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (BinTree a)
arbitrarySizedTree m = do
  size <- choose(0, m)
  case size of
    0 -> EmptyTree
    1 -> Node m (EmptyTree) (EmptyTree)
    n -> Node m (arbitrarySizedTree n-1)(arbitrarySizedTree n-2)
