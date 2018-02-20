{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module AttackTree.AttackTree (
   AttackTree(..),
   PAttackTree(..),
   module AttackTree.Configuration,
   start,
   base,
   and,
   or,
   seq) where

import Prelude hiding (or, and, seq)
import Control.Monad.State

import AttackTree.Configuration
    
data ATOps c where
    OROp  :: Ord c => (c -> c -> c) -> ATOps c
    ANDOp :: Ord c => (c -> c -> c) -> ATOps c
    SEQOp :: Ord c => (c -> c -> c) -> ATOps c

type ID = Integer

data AttackTree cost label where
    Base :: ID
         -> cost
         -> label
         -> AttackTree cost label
            
    OR :: ATOps cost
       -> label
       -> AttackTree cost label
       -> AttackTree cost label
       -> AttackTree cost label

    AND :: ATOps cost
        -> label
        -> AttackTree cost label
        -> AttackTree cost label
        -> AttackTree cost label

    SEQ :: ATOps cost
        -> label
        -> AttackTree cost label
        -> AttackTree cost label
        -> AttackTree cost label

data Node = ORNode | ANDNode | SEQNode

foldrAT :: (Node -> ATOps cost -> label -> b -> b -> b)
        -> (ID -> cost -> label -> b)
        -> AttackTree cost label
        -> b
foldrAT f b (Base id c l) = b id c l
foldrAT f b (OR op l atl atr) = f ORNode op l (foldrAT f b atl) (foldrAT f b atr)
foldrAT f b (AND op l atl atr) = f ANDNode op l (foldrAT f b atl) (foldrAT f b atr)
foldrAT f b (SEQ op l atl atr) = f SEQNode op l (foldrAT f b atl) (foldrAT f b atr)

validAT :: AttackTree cost label -> Bool
validAT = foldrAT f (\_ -> \_ -> \_ -> True) 
 where
   f ORNode (OROp op) _ vl vr = vl && vr
   f ANDNode (ANDOp op) _ vl vr = vl && vr
   f SEQNode (SEQOp op) _ vl vr = vl && vr
   f _ _ _ _ _= False
                                     
showNode :: Show label => Int -> String -> label -> String -> String -> String
showNode i node l atl atr = node++"("++(show l)++")\n"
                         ++ tabs++"("++atl++")\n"
                         ++ tabs++"("++atr++")"
 where
   tabs = take i $ repeat '\t'

showAT :: (Show cost, Show label) => AttackTree cost label -> String
showAT = showAT' 1
         
showAT' :: (Show cost, Show label) => Int -> AttackTree cost label -> String
showAT' i (Base id c l)        = show (id,l,c)
showAT' i (OR _ l atl atr)  = showNode i "OR"  l (showAT' (i+1) atl) (showAT' (i+1) atr)
showAT' i (AND _ l atl atr) = showNode i "AND" l (showAT' (i+1) atl) (showAT' (i+1) atr)
showAT' i (SEQ _ l atl atr) = showNode i "SEQ" l (showAT' (i+1) atl) (showAT' (i+1) atr)

instance (Show cost, Show label) => Show (AttackTree cost label) where
    show t = showAT t
             
data PAttackTree cost label where
    IBase :: cost
          -> label
          -> PAttackTree cost label
            
    IOR :: label
        -> PAttackTree cost label
        -> PAttackTree cost label
        -> PAttackTree cost label

    IAND :: label
         -> PAttackTree cost label
         -> PAttackTree cost label
         -> PAttackTree cost label

    ISEQ :: label
         -> PAttackTree cost label
         -> PAttackTree cost label
         -> PAttackTree cost label
 deriving Show

foldrPAT :: (Node -> label -> b -> b -> b)
        -> (cost -> label -> b)
        -> PAttackTree cost label
        -> b
foldrPAT f b (IBase c l) = b c l
foldrPAT f b (IOR  l iatl iatr) = f ORNode l (foldrPAT f b iatl) (foldrPAT f b iatr)
foldrPAT f b (IAND l iatl iatr) = f ANDNode l (foldrPAT f b iatl) (foldrPAT f b iatr)
foldrPAT f b (ISEQ l iatl iatr) = f SEQNode l (foldrPAT f b iatl) (foldrPAT f b iatr)

projConf :: Conf cost -> Node -> ATOps cost
projConf (Conf orOp _ _)  ORNode  = OROp orOp
projConf (Conf _ andOp _) ANDNode = ANDOp andOp
projConf (Conf _ _ seqOp) SEQNode = SEQOp seqOp

start' :: Conf cost -> PAttackTree cost l -> State ID (AttackTree cost l)
start' _ (IBase c l) = do
  id <- get
  put $ id + 1
  return $ Base id c l
start' conf (IOR  l iatl iatr) = do
  atl <- start' conf iatl
  atr <- start' conf iatr
  return $ OR (projConf conf ORNode) l atl atr
start' conf (IAND l iatl iatr) = do
  atl <- start' conf iatl
  atr <- start' conf iatr
  return $ AND (projConf conf ANDNode) l atl atr
start' conf (ISEQ l iatl iatr) = do
  atl <- start' conf iatl
  atr <- start' conf iatr
  return $ SEQ (projConf conf SEQNode) l atl atr
                             
start :: Conf cost -> PAttackTree cost l -> AttackTree cost l
start conf ia = evalState (start' conf ia) 0

base :: label -> cost -> PAttackTree label cost
base = IBase
                                          
and :: label -> PAttackTree cost label -> PAttackTree cost label -> PAttackTree cost label
and l iatl iatr = IAND l iatl iatr

or :: label -> PAttackTree cost label -> PAttackTree cost label -> PAttackTree cost label
or l iatl iatr = IOR l iatl iatr                  

seq :: label -> PAttackTree cost label -> PAttackTree cost label -> PAttackTree cost label
seq l iatl iatr = ISEQ l iatl iatr
