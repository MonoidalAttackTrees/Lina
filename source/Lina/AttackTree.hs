{-# LANGUAGE GADTs                #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Lina.AttackTree -- (  
   -- PAttackTree(..),
   -- ProcAT,
   -- AttackTree(..),
   -- module AttackTree.Configuration,
   -- ID,
   -- start_PAT,
   -- start_AT,
   -- base,
   -- atr_base,
   -- and_node,
   -- or_node,
   -- seq_node,
   -- RAttackTree(..),
   -- eval,
   -- eval_PT,
   -- Attack(..),
   -- get_attacks,
   -- min_attack)
where

import qualified Data.Map as M
import qualified Data.Bimap as B
import Control.Monad.State

import Lina.Utils
import Lina.Error
import Lina.AttackTree.Configuration

-- Internal Attack Tree
data IAT where
    Base :: ID -> IAT            
    OR   :: ID -> IAT -> IAT -> IAT
    AND  :: ID -> IAT -> IAT -> IAT
    SEQ  :: ID -> IAT -> IAT -> IAT

-- Process Attack Tree: Attack Tree without Attributes
data PAttackTree label = PAttackTree {
  internal_tree :: IAT,
  labels :: B.Bimap label ID
}

-- Attributed Process Attack Tree
data APAttackTree attribute label = APAttackTree {
  process_tree :: PAttackTree label,
  attributes :: M.Map ID attribute
}

-- Full Attack Tree
data AttackTree attribute label = AttackTree {            
      ap_tree :: APAttackTree attribute label,
      configuration :: Conf attribute
}

showNode :: (Ord label,Show label) => Int -> ID -> String -> B.Bimap label ID -> String -> String -> String
showNode i id node labels lt rt = 
  case B.lookupR id labels of
    Just l -> node++"("++(show id)++","++(show l)++")\n"
                            ++ tabs++"("++lt++")\n"
                            ++ tabs++"("++rt++")"
    Nothing -> error.show $ LabelNotFound "showNode" id
 where
   tabs = take i $ repeat '\t'

showIAT' :: (Ord label,Show label) => Int -> B.Bimap label ID -> IAT -> String
showIAT' i labels (Base id)      = case B.lookupR id labels of
                                     Just l -> (show id)++","++(show l)
                                     Nothing -> error.show $ LabelNotFound "showIAT'" id
showIAT' i labels (OR id lt rt)  = showNode i id "OR"  labels (showIAT' (i+1) labels lt) (showIAT' (i+1) labels rt)
showIAT' i labels (AND id lt rt) = showNode i id "AND" labels (showIAT' (i+1) labels lt) (showIAT' (i+1) labels rt)
showIAT' i labels (SEQ id lt rt) = showNode i id "SEQ" labels (showIAT' (i+1) labels lt) (showIAT' (i+1) labels rt)

showPAT :: (Ord label, Show label) => PAttackTree label -> String
showPAT (PAttackTree tree labels) = showIAT' 1 labels tree
         
instance (Ord label, Show label) => Show (PAttackTree label) where
    show = showPAT

showANode :: (Ord label,Ord attribute,Show label,Show attribute) => Int -> ID -> String -> B.Bimap label ID -> M.Map ID attribute -> String -> String -> String
showANode i id node labels attributes lt rt = 
  case (B.lookupR id labels,M.lookup id attributes) of
    (Just l, Just a) -> node++"("++(show id)++","++(show a)++","++(show l)++")\n"
                            ++ tabs++"("++lt++")\n"
                            ++ tabs++"("++rt++")"
    (Nothing,Nothing) -> (show $ LabelNotFound "showANode" id) ++ "\n" ++ (show $ AttrNotFound "showANode" id)
    (Nothing,_) -> show $ LabelNotFound "showANode" id
    (_,Nothing) -> show $ AttrNotFound "showANode" id
 where
   tabs = take i $ repeat '\t'

showAPAT' :: (Ord label,Ord attribute,Show label,Show attribute) => Int -> B.Bimap label ID -> M.Map ID attribute -> IAT -> String
showAPAT' i labels attributes (Base id)      = show id
showAPAT' i labels attributes (OR id lt rt)  = showANode i id "OR"  labels attributes (showAPAT' (i+1) labels attributes lt) (showAPAT' (i+1) labels attributes rt)
showAPAT' i labels attributes (AND id lt rt) = showANode i id "AND" labels attributes (showAPAT' (i+1) labels attributes lt) (showAPAT' (i+1) labels attributes rt)
showAPAT' i labels attributes (SEQ id lt rt) = showANode i id "SEQ" labels attributes (showAPAT' (i+1) labels attributes lt) (showAPAT' (i+1) labels attributes rt)

showAPAT :: (Ord label,Ord attribute,Show label,Show attribute) => APAttackTree attribute label -> String
showAPAT (APAttackTree (PAttackTree tree labels) attributes) = showAPAT' 1 labels attributes tree

instance (Ord label,Ord attribute,Show label,Show attribute) => Show (APAttackTree attribute label) where
    show = showAPAT

instance (Ord label,Ord attribute,Show label,Show attribute) => Show (AttackTree attribute label) where
    show (AttackTree apt _) = showAPAT apt

type PATState label = State (ID,B.Bimap label ID) (PAttackTree label) 

build_node :: Ord label => (ID -> IAT -> IAT -> IAT) -> label -> PATState label -> PATState label -> PATState label
build_node node label l r = do
  (PAttackTree lt labelsl) <- l
  (PAttackTree rt labelsr) <- r
  (cid,labels) <- get
  let id = cid
  let labels' = labelsl `union` labelsr
  let labels'' = B.insert label id $ labels `union` labels'
  put (cid+1,labels'')
  return $ PAttackTree (node id lt rt) labels''

base :: Ord label => label -> PATState label
base label = do
  (cid,labels) <- get
  case B.lookup label labels of
    Just id -> return $ PAttackTree (Base id) labels
    Nothing -> do
      let id = cid
      let labels' = B.insert label id labels
      put (cid+1,labels')
      return $ PAttackTree (Base id) labels'

or_node :: Ord label => label -> PATState label -> PATState label -> PATState label
or_node label l r = build_node OR label l r

and_node :: Ord label => label -> PATState label -> PATState label -> PATState label
and_node label l r = build_node AND label l r

seq_node :: Ord label => label -> PATState label -> PATState label -> PATState label
seq_node label l r = build_node SEQ label l r

start_PAT :: PATState label -> PAttackTree label
start_PAT at = evalState at (0,B.empty)

type APATState attribute label = State (ID,M.Map ID attribute,B.Bimap label ID) (APAttackTree attribute label) 

build_anode :: Ord label => (ID -> IAT -> IAT -> IAT) -> label -> APATState attribute label -> APATState attribute label -> APATState attribute label
build_anode node label l r = do
  APAttackTree (PAttackTree lt labelsl) attributesl <- l
  APAttackTree (PAttackTree rt labelsr) attributesr <- r
  (cid,attributes,labels) <- get
  let id = cid
  let attributes' = attributesl `M.union` attributesr
  let labels' = labelsl `union` labelsr
  let labels'' = B.insert label id $ labels `union` labels'
  put (cid+1,attributes',labels'')
  return $ APAttackTree (PAttackTree (node id lt rt) labels'') attributes'

atr_base :: Ord label => attribute -> label -> APATState attribute label
atr_base attribute label = do
  (cid,attributes,labels) <- get
  case B.lookup label labels of
    Just id -> case M.lookup id attributes of
                 Just attr -> return $ APAttackTree (PAttackTree (Base id) labels) attributes
                 Nothing -> do
                   let attributes' = M.insert id attribute attributes
                   put (cid,attributes',labels)
                   return $ APAttackTree (PAttackTree (Base id) labels) attributes'
    Nothing -> do
      let id = cid
      let labels' = B.insert label id labels
      let attributes' = M.insert id attribute attributes
      put (cid+1,attributes',labels')
      return $ APAttackTree (PAttackTree (Base id) labels') attributes'

or_anode :: Ord label => attribute -> label -> APATState attribute label -> APATState attribute label -> APATState attribute label
or_anode attribute label l r = build_anode OR label l r

and_anode :: Ord label => attribute -> label -> APATState attribute label -> APATState attribute label -> APATState attribute label
and_anode attribute label l r = build_anode AND label l r

seq_anode :: Ord label => attribute -> label -> APATState attribute label -> APATState attribute label -> APATState attribute label
seq_anode attribute label l r = build_anode SEQ label l r

start_APAT :: APATState attribute label -> APAttackTree attribute label
start_APAT at = evalState at (0,M.empty,B.empty)

-- get_attacks :: Conf attribute -> RAttackTree attribute label -> [Attack attribute label]
-- get_attacks conf (RBase id c l)  = [PBase id c l]
-- get_attacks conf (ROR id c l lt rt) = (map compute_POR $ get_attacks conf lt) ++ (map compute_POR $ get_attacks conf rt)
--  where
--    compute_POR p = (POR id (get_pattribute p) l p)
-- get_attacks conf@(Conf _ andOp _) (RAND id c l lt rt) = [compute_PAND p1 p2 | p1 <- get_attacks conf lt,p2 <- get_attacks conf rt]
--  where
--    compute_PAND p1 p2 = (PAND id ((get_pattribute p1) `andOp` (get_pattribute p2)) l p1 p2)
-- get_attacks conf@(Conf _ _ seqOp) (RSEQ id c l lt rt) = [compute_PSEQ p1 p2 | p1 <- get_attacks conf lt,p2 <- get_attacks conf rt]
--  where
--    compute_PSEQ p1 p2 = (PSEQ id ((get_pattribute p1) `seqOp` (get_pattribute p2)) l p1 p2)

-- min_attack :: Ord attribute => [Attack attribute label] -> [Attack attribute label]
-- min_attack [] = []
-- min_attack ps@(p:_) = min_attack' (get_pattribute p) ps
--  where   
--    min_attack' :: Ord attribute => attribute -> [Attack attribute label] -> [Attack attribute label]
--    min_attack' c ps = [p | p <- ps,get_pattribute p <= c]
