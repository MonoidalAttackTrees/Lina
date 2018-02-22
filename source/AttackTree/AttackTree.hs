{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module AttackTree.AttackTree (
   PAttackTree(..),
   AttackTree(..),
   module AttackTree.Configuration,
   ID,
   start_PAT,
   start_AT,
   base,
   and_node,
   or_node,
   seq_node) where

import Prelude hiding (or, and, seq)
import Control.Monad.State

import AttackTree.Configuration
    
data ATOps c where
    OROp  :: Ord c => (c -> c -> c) -> ATOps c
    ANDOp :: Ord c => (c -> c -> c) -> ATOps c
    SEQOp :: Ord c => (c -> c -> c) -> ATOps c

type ID = Integer

data PAttackTree cost label where
    Base :: ID
         -> cost
         -> label
         -> PAttackTree cost label
            
    OR :: ID
       -> label
       -> PAttackTree cost label
       -> PAttackTree cost label
       -> PAttackTree cost label

    AND :: ID
        -> label
        -> PAttackTree cost label
        -> PAttackTree cost label
        -> PAttackTree cost label

    SEQ :: ID
        -> label
        -> PAttackTree cost label
        -> PAttackTree cost label
        -> PAttackTree cost label

data AttackTree cost label = AttackTree {      
      conf   :: Conf cost,
      pATree :: PAttackTree cost label
}
           
data Node = ORNode | ANDNode | SEQNode

foldrPAT :: (ID -> Node -> label -> b -> b -> b)
        -> (ID -> cost -> label -> b)
        -> PAttackTree cost label
        -> b
foldrPAT f b (Base id c l) = b id c l
foldrPAT f b (OR id l atl atr) = f id ORNode l (foldrPAT f b atl) (foldrPAT f b atr)
foldrPAT f b (AND id l atl atr) = f id ANDNode l (foldrPAT f b atl) (foldrPAT f b atr)
foldrPAT f b (SEQ id l atl atr) = f id SEQNode l (foldrPAT f b atl) (foldrPAT f b atr)
                                     
showNode :: Show label => Int -> ID -> String -> label -> String -> String -> String
showNode i id node l atl atr = node++"("++(show id)++","++(show l)++")\n"
                            ++ tabs++"("++atl++")\n"
                            ++ tabs++"("++atr++")"
 where
   tabs = take i $ repeat '\t'

showPAT :: (Show cost, Show label) => PAttackTree cost label -> String
showPAT = showPAT' 1
         
showPAT' :: (Show cost, Show label) => Int -> PAttackTree cost label -> String
showPAT' i (Base id c l)      = show (id,l,c)
showPAT' i (OR  id l atl atr) = showNode i id "OR"  l (showPAT' (i+1) atl) (showPAT' (i+1) atr)
showPAT' i (AND id l atl atr) = showNode i id "AND" l (showPAT' (i+1) atl) (showPAT' (i+1) atr)
showPAT' i (SEQ id l atl atr) = showNode i id "SEQ" l (showPAT' (i+1) atl) (showPAT' (i+1) atr)

instance (Show cost, Show label) => Show (PAttackTree cost label) where
    show = showPAT

instance (Show cost, Show label) => Show (AttackTree cost label) where
    show at = showPAT $ pATree at
             
projConf :: Conf cost -> Node -> ATOps cost
projConf (Conf orOp _ _)  ORNode  = OROp orOp
projConf (Conf _ andOp _) ANDNode = ANDOp andOp
projConf (Conf _ _ seqOp) SEQNode = SEQOp seqOp

-- State(Base-ID,OR-ID,AND-ID,SEQ-ID)
type PATState cost label = State ID (PAttackTree cost label)
initialState :: ID
initialState = 0

incID :: State ID ID
incID = do
  id <- get
  put $ id + 1
  return id

constructNode ::
    (ID    ->
     label ->
     PAttackTree cost label ->
     PAttackTree cost label ->
     PAttackTree cost label)
 -> label
 -> PATState cost label
 -> PATState cost label
 -> PATState cost label
constructNode node l atl atr = do
  id <- incID
  patl <- atl
  patr <- atr
  return $ node id l patl patr


start' :: PAttackTree cost label -> PATState cost label
start' (Base _ label cost) = do
   id <- incID
   return $ Base id label cost
start' (OR  _ label atl atr) = constructNode OR  label (start' atl) (start' atr) 
start' (AND _ label atl atr) = constructNode AND label (start' atl) (start' atr) 
start' (SEQ _ label atl atr) = constructNode SEQ label (start' atl) (start' atr) 

start_PAT :: PAttackTree cost label -> PAttackTree cost label
start_PAT at = fst $ runState (start' at) 0

start_AT :: Conf cost -> PAttackTree cost l -> AttackTree cost l
start_AT conf at = let (pat,_) = runState (start' at) 0
                   in AttackTree conf pat   
        
base :: label -> cost -> PAttackTree label cost
base label cost = Base 0 label cost
         
and_node :: label
         -> PAttackTree cost label
         -> PAttackTree cost label
         -> PAttackTree cost label
and_node l atl atr = AND 0 l atl atr

or_node :: label
        -> PAttackTree cost label
        -> PAttackTree cost label
        -> PAttackTree cost label
or_node l atl atr = OR 0 l atl atr

seq_node :: label
         -> PAttackTree cost label
         -> PAttackTree cost label
         -> PAttackTree cost label
seq_node l atl atr = SEQ 0 l atl atr
