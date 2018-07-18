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

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary =
    sized arbitrarySizedTree 

--It won't match type 'a' with 'Int'
--Expects type: Gen (BinTree a)
--Getting type: Gen (BinTree Int)
--I think it has something to do with the selection of 'b' in 'arbitrarySizedTree' but I'm not sure how to fix it

arbitrarySizedTree :: Arbitrary a => a -> Gen (BinTree a)
arbitrarySizedTree m = do
  d <- arbitrary
  b <- choose (0, m `div` 2)
  if b == 0 then return EmptyTree
  else if b == 1 then return (Node m (EmptyTree)(EmptyTree))
  else return (Node d (BinTree b)(BinTree b))
