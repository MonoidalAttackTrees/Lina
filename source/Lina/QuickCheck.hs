{-# LANGUAGE FlexibleInstances #-}
module Lina.QuickCheck where

--import graphy
import Test.QuickCheck
import System.Random

data BinTree a = EmptyTree
               | Node  a (BinTree a) (BinTree a)
               | NodeB a (BinTree a) (BinTree a)
  deriving (Show, Read, Eq)

showBT :: Gen (BinTree Int) -> String
showBT x = do
  undefined
  
createGraph :: Gen (BinTree Int) -> [(Int,Int)]
createGraph a = do
  root <- Node root _ _
  return root
  
  -- left <- Node _ y _
  -- right <- Node _ _ z
  -- if left == EmptyTree && right == EmptyTree then return
  -- else if left == EmptyTree && right != EmptyTree then edgesList ++ [(root,right)]
  --   createGraph right
  -- else if right == EmptyTree && left != EmptyTree then edgesList ++ [(root,left)]
  --   createGraph left
  -- else edgesList ++ [(root,left)]
  --   edgesList ++ [(root,right)]
  --   createGraph left
  --   createGraph right


--instance Show (BinTree a) where
  --show = showBT

instance Arbitrary (BinTree Int) where
  arbitrary = sized arbitrarySizedTree 

arbitrarySizedTree :: Int -> Gen (BinTree Int)
arbitrarySizedTree 0 = return EmptyTree
arbitrarySizedTree 1 = do
  d <- arbitrary :: Gen Int
  nodePick <- choose (0,1)  :: Gen Int
  if nodePick == 0 then return $ Node d EmptyTree EmptyTree
  else return $ NodeB d EmptyTree EmptyTree
  return $ Node d EmptyTree EmptyTree
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

