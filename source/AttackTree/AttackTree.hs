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
   seq_node,
   RAttackTree(..),
   eval,
   eval_PT,
   Attack(..),
   get_attacks,
   min_attack) where

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

data RAttackTree cost label where
    RBase :: ID
          -> cost
          -> label
          -> RAttackTree cost label
            
    ROR :: ID
        -> cost
        -> label
        -> RAttackTree cost label
        -> RAttackTree cost label
        -> RAttackTree cost label

    RAND :: ID
         -> cost
         -> label
         -> RAttackTree cost label
         -> RAttackTree cost label
         -> RAttackTree cost label

    RSEQ :: ID
         -> cost
         -> label
         -> RAttackTree cost label
         -> RAttackTree cost label
         -> RAttackTree cost label
         
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

showNodeRT :: (Show cost, Show label) => Int -> String -> cost -> label -> String -> String -> String
showNodeRT i node c l atl atr = node++"("++(show l)++","++(show c)++")\n"
                             ++ tabs++"("++atl++")\n"
                             ++ tabs++"("++atr++")"
 where
   tabs = take i $ repeat ' '

showRT :: (Show cost, Show label) => RAttackTree cost label -> String
showRT = showRT' 1
         
showRT' :: (Show cost, Show label) => Int -> RAttackTree cost label -> String
showRT' i (RBase id c l)       = show (id,l,c)
showRT' i (ROR  _ c l atl atr) = showNodeRT i "OR"  c l (showRT' (i+3) atl) (showRT' (i+3) atr)
showRT' i (RAND _ c l atl atr) = showNodeRT i "AND" c l (showRT' (i+3) atl) (showRT' (i+3) atr)
showRT' i (RSEQ _ c l atl atr) = showNodeRT i "SEQ" c l (showRT' (i+3) atl) (showRT' (i+3) atr)

instance (Show cost, Show label) => Show (RAttackTree cost label) where
    show = showRT
                                 
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

get_cost :: RAttackTree cost label -> cost
get_cost (RBase _ c _) = c
get_cost (ROR _ c _ _ _) = c
get_cost (RAND _ c _ _ _) = c
get_cost (RSEQ _ c _ _ _) = c

eval :: AttackTree cost label -> RAttackTree cost label
eval (AttackTree conf at) = eval_PT conf at

eval_PT :: Conf cost -> PAttackTree cost label -> RAttackTree cost label
eval_PT _ (Base id c l) = RBase id c l
eval_PT conf@(Conf orOp _ _) (OR id l atl atr) = ROR id cost l ratl ratr
 where
   ratl = eval_PT conf atl
   ratr = eval_PT conf atr
   cost = (get_cost ratl) `orOp` (get_cost ratr)   
eval_PT conf@(Conf _ andOp _) (AND id l atl atr) = RAND id cost l ratl ratr
 where
   ratl = eval_PT conf atl
   ratr = eval_PT conf atr
   cost = (get_cost ratl) `andOp` (get_cost ratr)
eval_PT conf@(Conf _ _ seqOp) (SEQ id l atl atr) = RSEQ id cost l ratl ratr
 where
   ratl = eval_PT conf atl
   ratr = eval_PT conf atr
   cost = (get_cost ratl) `seqOp` (get_cost ratr)

data Attack cost label where
    PBase :: ID
          -> cost
          -> label
          -> Attack cost label
            
    POR :: ID
        -> cost
        -> label
        -> Attack cost label
        -> Attack cost label

    PAND :: ID
         -> cost
         -> label
         -> Attack cost label
         -> Attack cost label
         -> Attack cost label

    PSEQ :: ID
         -> cost
         -> label
         -> Attack cost label
         -> Attack cost label
         -> Attack cost label

showNodePOR :: (Show cost, Show label) => Int -> String -> cost -> label -> String -> String
showNodePOR i node c l at = node++"("++(show l)++","++(show c)++")\n" ++ tabs++"("++at++")"
 where
   tabs = take i $ repeat ' '

showAttack :: (Show cost, Show label) => Attack cost label -> String
showAttack = showAttack' 1
         
showAttack' :: (Show cost, Show label) => Int -> Attack cost label -> String
showAttack' i (PBase id c l)       = show (id,l,c)
showAttack' i (POR  _ c l at)      = showNodePOR i "OR"  c l (showAttack' (i+3) at)
showAttack' i (PAND _ c l atl atr) = showNodeRT i "AND" c l (showAttack' (i+3) atl) (showAttack' (i+3) atr)
showAttack' i (PSEQ _ c l atl atr) = showNodeRT i "SEQ" c l (showAttack' (i+3) atl) (showAttack' (i+3) atr)

instance (Show cost, Show label) => Show (Attack cost label) where
    show = showAttack

get_pcost :: Attack cost label -> cost
get_pcost (PBase _ c _) = c
get_pcost (POR _ c _ _) = c
get_pcost (PAND _ c _ _ _) = c
get_pcost (PSEQ _ c _ _ _) = c

get_attacks :: Conf cost -> RAttackTree cost label -> [Attack cost label]
get_attacks conf (RBase id c l)  = [PBase id c l]
get_attacks conf (ROR id c l lt rt) = (map compute_POR $ get_attacks conf lt) ++ (map compute_POR $ get_attacks conf rt)
 where
   compute_POR p = (POR id (get_pcost p) l p)
get_attacks conf@(Conf _ andOp _) (RAND id c l lt rt) = [compute_PAND p1 p2 | p1 <- get_attacks conf lt,p2 <- get_attacks conf rt]
 where
   compute_PAND p1 p2 = (PAND id ((get_pcost p1) `andOp` (get_pcost p2)) l p1 p2)
get_attacks conf@(Conf _ _ seqOp) (RSEQ id c l lt rt) = [compute_PSEQ p1 p2 | p1 <- get_attacks conf lt,p2 <- get_attacks conf rt]
 where
   compute_PSEQ p1 p2 = (PSEQ id ((get_pcost p1) `seqOp` (get_pcost p2)) l p1 p2)

min_attack :: Ord cost => [Attack cost label] -> [Attack cost label]
min_attack [] = []
min_attack ps@(p:_) = min_attack' (get_pcost p) ps
 where   
   min_attack' :: Ord cost => cost -> [Attack cost label] -> [Attack cost label]
   min_attack' c ps = [p | p <- ps,get_pcost p <= c]
