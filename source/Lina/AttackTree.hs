{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Lina.AttackTree (
   IAT(..),
   PAttackTree,
   APAttackTree(..),
   AttackTree(..),
   module Lina.AttackTree.Configuration,   
   start_PAT,
   start_AT,
   insert,
   base,
   no_attr,
   attr,
   base_wa,
   base_na,
   and_node,
   or_node,
   seq_node,
   get_attacks,
   min_attacks,
   get_attack_attribute)
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

map_IAT :: (ID -> ID) -> IAT -> IAT
map_IAT f (Base id) = Base $ f id
map_IAT f (OR id l r) = OR (f id) (map_IAT f l) (map_IAT f r)
map_IAT f (AND id l r) = AND (f id) (map_IAT f l) (map_IAT f r)
map_IAT f (SEQ id l r) = SEQ (f id) (map_IAT f l) (map_IAT f r)

-- Attributed Process Attack Tree
data APAttackTree attribute label = APAttackTree {
  process_tree :: IAT,
  labels :: B.Bimap label ID,
  attributes :: M.Map ID attribute
}

-- Process Attack Tree: Attack Tree without Attributes
type PAttackTree label = APAttackTree () label

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
showPAT (APAttackTree tree labels _) = showIAT' 1 labels tree
         
instance {-# OVERLAPS #-} (Ord label, Show label) => Show (PAttackTree label) where
    show = showPAT

buildShowString :: Show label => Int -> String -> ID -> label -> String -> String -> String
buildShowString i node id l lt rt = node++"("++(show id)++","++(show l)++")\n"
                                        ++ tabs++"("++lt++")\n"
                                        ++ tabs++"("++rt++")"
 where
   tabs = take i $ repeat '\t'

get_apat_attribute :: String -> ID -> M.Map ID attribute -> attribute
get_apat_attribute w id attributes =
  case M.lookup id attributes of
    Just a -> a
    Nothing -> error.show $ AttrNotFound w id

get_apat_label :: Ord label => String -> ID -> B.Bimap label ID -> label
get_apat_label w id labels =
  case B.lookupR id labels of
    Just l -> l
    Nothing -> error.show $ LabelNotFound w id    

showANode :: (Ord label,Ord attribute,Show label,Show attribute) => Int -> ID -> String -> B.Bimap label ID -> M.Map ID attribute -> String -> String -> String
showANode i id node labels attributes lt rt = buildShowString i node id label lt rt
 where
   label = get_apat_label "showANode" id labels

showAPAT' :: (Ord label,Ord attribute,Show label,Show attribute) => Int -> B.Bimap label ID -> M.Map ID attribute -> IAT -> String
showAPAT' i labels attributes (Base id)      = case (B.lookupR id labels,M.lookup id attributes) of
                                                 (Just l,Just a) -> (show id)++","++(show l)++","++(show a)
                                                 (Nothing,Nothing) -> error $ (show $ LabelNotFound "showIAT'" id) ++ "\n" ++ (show $ AttrNotFound "shoqIAT'" id)
                                                 (Nothing,_) -> error $ (show $ LabelNotFound "showIAT'" id)
                                                 (_,Nothing) -> error $ (show $ AttrNotFound "shoqIAT'" id)                                                 
showAPAT' i labels attributes (OR id lt rt)  = showANode i id "OR"  labels attributes (showAPAT' (i+1) labels attributes lt) (showAPAT' (i+1) labels attributes rt)
showAPAT' i labels attributes (AND id lt rt) = showANode i id "AND" labels attributes (showAPAT' (i+1) labels attributes lt) (showAPAT' (i+1) labels attributes rt)
showAPAT' i labels attributes (SEQ id lt rt) = showANode i id "SEQ" labels attributes (showAPAT' (i+1) labels attributes lt) (showAPAT' (i+1) labels attributes rt)

showAPAT :: (Ord label,Ord attribute,Show label,Show attribute) => APAttackTree attribute label -> String
showAPAT (APAttackTree tree labels attributes) = showAPAT' 1 labels attributes tree

instance (Ord label,Ord attribute,Show label,Show attribute) => Show (APAttackTree attribute label) where
    show = showAPAT

instance (Ord label,Ord attribute,Show label,Show attribute) => Show (AttackTree attribute label) where
    show (AttackTree apt _) = showAPAT apt

type PATState label = State (ID,B.Bimap label ID) (PAttackTree label) 

type ALState attribute label a = State (M.Map ID attribute,B.Bimap label ID) a
type APATState attribute label = State (ID,M.Map ID attribute,B.Bimap label ID) (APAttackTree attribute label) 

build_node :: Ord label => (ID -> IAT -> IAT -> IAT) -> label -> APATState attribute label -> APATState attribute label -> APATState attribute label
build_node node label l r = do
  APAttackTree lt labelsl attributesl <- l
  APAttackTree rt labelsr attributesr <- r
  (cid,attributes,labels) <- get
  let id = cid
  let attributes' = attributesl `M.union` attributesr
  let labels' = labelsl `union` labelsr
  let labels'' = B.insert label id $ labels `union` labels'
  put (cid+1,attributes',labels'')
  return $ APAttackTree (node id lt rt) labels'' attributes'

no_attr :: Maybe attribute
no_attr = Nothing

attr :: attribute -> Maybe attribute
attr a = Just a

base :: Ord label => Maybe attribute -> label -> APATState attribute label
base Nothing label = do
  (cid,attributes,labels) <- get
  case B.lookup label labels of
    Just id -> do put (cid,attributes,labels)
                  return $ APAttackTree (Base id) labels attributes
    Nothing -> do
      let id = cid
      let labels' = B.insert label id labels
      put (cid+1,attributes,labels')
      return $ APAttackTree (Base id) labels' attributes
base (Just attribute) label = do
  (cid,attributes,labels) <- get
  case B.lookup label labels of
    Just id -> case M.lookup id attributes of
                 Just attr -> return $ APAttackTree (Base id) labels attributes
                 Nothing -> do
                   let attributes' = M.insert id attribute attributes
                   put (cid,attributes',labels)
                   return $ APAttackTree (Base id) labels attributes'
    Nothing -> do
      let id = cid
      let labels' = B.insert label id labels
      let attributes' = M.insert id attribute attributes
      put (cid+1,attributes',labels')
      return $ APAttackTree (Base id) labels' attributes'

base_wa :: Ord label => attribute -> label -> APATState attribute label
base_wa at = base (Just at)

base_na :: Ord label => label -> APATState attribute label
base_na label = base Nothing label

or_node :: Ord label => label -> APATState attribute label -> APATState attribute label -> APATState attribute label
or_node label l r = build_node OR label l r

and_node :: Ord label => label -> APATState attribute label -> APATState attribute label -> APATState attribute label
and_node label l r = build_node AND label l r

seq_node :: Ord label => label -> APATState attribute label -> APATState attribute label -> APATState attribute label
seq_node label l r = build_node SEQ label l r

start_PAT :: APATState attribute label -> APAttackTree attribute label
start_PAT at = evalState at (0,M.empty,B.empty)

start_AT :: Conf attribute -> APATState attribute label -> AttackTree attribute label
start_AT conf sAT = AttackTree (evalState sAT (0,M.empty,B.empty))  conf

insert :: Ord label => APAttackTree attribute label -> APATState attribute label
insert p@(APAttackTree tree labels attributes) = do
  (cid,cattibutes,clabels) <- get
  let (max,_) = B.findMaxR labels
  let tree' = map_IAT (+cid) tree
  let labels' = clabels `union` (B.mapR (+cid) labels)
  let attributes' = cattibutes `M.union` M.mapKeys (+cid) attributes
  put (cid+max+1, attributes', labels)
  return $ APAttackTree tree' labels' attributes'

data IAttack where
  BaseA :: ID -> IAttack
  ORA   :: ID -> IAttack -> IAttack
  ANDA  :: ID -> IAttack -> IAttack -> IAttack
  SEQA  :: ID -> IAttack -> IAttack -> IAttack
 deriving Eq

data Attack attribute label = Attack {
  attack_itree :: IAttack,
  attack_labels :: B.Bimap label ID,
  attack_attributes :: M.Map ID attribute
}

showORANode :: (Ord label,Ord attribute,Show label,Show attribute)
            => Int
            -> ID
            -> B.Bimap label ID
            -> M.Map ID attribute
            -> String
            -> String
showORANode i id labels attributes t = 
  case (B.lookupR id labels,M.lookup id attributes) of
    (Just l,Just att) -> "OR("++(show id)++","++(show l)++","++(show att)++")\n"
                   ++ tabs++"("++t++")"
    (Nothing,_) -> show $ LabelNotFound "showANode" id
    (_,Nothing) -> show $ AttrNotFound "showANode" id    
 where
   tabs = take i $ repeat '\t'

build_attack_node_show_string :: (Show label, Show attribute) => Int -> String -> ID -> label -> attribute -> String -> String -> String
build_attack_node_show_string i node id l a lt rt = node++"("++(show id)++","++(show l)++","++(show a)++")\n"
                                                        ++ tabs++"("++lt++")\n"
                                                        ++ tabs++"("++rt++")"
 where
   tabs = take i $ repeat '\t'
   
show_attack_node :: (Ord label,Ord attribute,Show label,Show attribute) => Int -> ID -> String -> B.Bimap label ID -> M.Map ID attribute -> String -> String -> String
show_attack_node i id node labels attributes lt rt = build_attack_node_show_string i node id label attribute lt rt
 where
   label = get_apat_label "showANode" id labels
   attribute = get_apat_attribute "showANode" id attributes

showIAttack' :: (Ord label,Ord attribute,Show label,Show attribute)
             => Int
             -> B.Bimap label ID
             -> M.Map ID attribute
             -> IAttack
             -> String
showIAttack' i labels attributes (BaseA id) =
  case (B.lookupR id labels,M.lookup id attributes) of
    (Just l,Just a) -> (show id)++","++(show l)++","++(show a)
    (Nothing,Nothing) -> error $ (show $ LabelNotFound "showIAttack'" id) ++ "\n" ++ (show $ AttrNotFound "showIAttack'" id)
    (Nothing,_) -> error $ (show $ LabelNotFound "showIAttack'" id)
    (_,Nothing) -> error $ (show $ AttrNotFound "showIAttack'" id)                                                 
showIAttack' i labels attributes (ORA id t)      = showORANode i id labels attributes (showIAttack' (i+1) labels attributes t)
showIAttack' i labels attributes (ANDA id lt rt) = show_attack_node i id "AND" labels attributes (showIAttack' (i+1) labels attributes lt) (showIAttack' (i+1) labels attributes rt)
showIAttack' i labels attributes (SEQA id lt rt) = show_attack_node i id "SEQ" labels attributes (showIAttack' (i+1) labels attributes lt) (showIAttack' (i+1) labels attributes rt)

showAttack :: (Ord label,Ord attribute,Show label,Show attribute) => Attack attribute label -> String
showAttack (Attack tree labels attributes) = showIAttack' 1 labels attributes tree

instance (Ord label,Ord attribute,Show label,Show attribute) => Show (Attack attribute label) where
    show = showAttack

get_id :: IAttack -> ID
get_id (BaseA id) = id
get_id (ORA id _) = id
get_id (ANDA id _ _) = id
get_id (SEQA id _ _) = id

compute_POR :: ID
            -> Attack attribute label
            -> Attack attribute label
compute_POR id (Attack attack labels attack_attributes) = 
  case M.lookup a_id attack_attributes of
    Just attr -> Attack (ORA id attack) labels (M.insert id attr attack_attributes)
    Nothing -> error $ "Failed to find the attribute for id: "++(show id)++" in compute_POR"
 where
   a_id = get_id attack
   
compute_OPNode :: M.Map ID attribute
               -> B.Bimap label ID
               -> (ID -> IAttack -> IAttack -> IAttack)
               -> (attribute -> attribute -> attribute)
               -> ID
               -> Attack attribute label
               -> Attack attribute label
               -> Attack attribute label
compute_OPNode attributes labels node op id (Attack l _ attributes_l) (Attack r _ attributes_r) =  
  case (mattr_l,mattr_r) of
    (Just attr_l,Just attr_r) ->  
      Attack (node id l r) labels (M.insert id (attr_l `op` attr_r) attributes_ats)
    (_,_) -> error $ "Failed to find an attribute for id: "++(show id_l)++
                     " or id: "++(show id_r)++" in compute_OpNode"
 where
  id_l = get_id l
  id_r = get_id r
  mattr_l = M.lookup id_l attributes_l
  mattr_r = M.lookup id_r attributes_r
  attributes_ats = attributes_l `M.union` attributes_r

get_attacks_IAT :: Conf attribute
                -> B.Bimap label ID
                -> IAT
                -> State (M.Map ID attribute) [Attack attribute label]
get_attacks_IAT conf labels (Base id) = do
  attributes <- get
  return [Attack (BaseA id) labels attributes]
get_attacks_IAT conf labels (OR id lt rt) = do
  attributes <- get  
  lats <- get_attacks_IAT conf labels lt
  rats <- get_attacks_IAT conf labels rt
  let comp_POR = compute_POR id
  return $ (map comp_POR lats) ++ (map comp_POR rats)
get_attacks_IAT conf@(Conf _ andOp _) labels (AND id lt rt) = do
  attributes <- get
  lats <- get_attacks_IAT conf labels lt
  rats <- get_attacks_IAT conf labels rt
  return [(compute_OPNode attributes labels ANDA (andOp) id l r) | l <- lats,r <- rats]  
get_attacks_IAT conf@(Conf _ _ seqOp) labels (SEQ id lt rt) = do
  attributes <- get
  lats <- get_attacks_IAT conf labels lt
  rats <- get_attacks_IAT conf labels rt
  return [(compute_OPNode attributes labels SEQA (seqOp) id l r) | l <- lats,r <- rats]  

get_attacks :: AttackTree attribute label -> [Attack attribute label]
get_attacks (AttackTree (APAttackTree tree labels attributes) conf) =
  evalState (get_attacks_IAT conf labels tree) attributes

get_attack_attribute :: Attack attribute label -> Either Error attribute
get_attack_attribute (Attack tree labels attributes) =
  case M.lookup id attributes of
    Just att -> return att
    Nothing -> throwError $ AttrNotFound "get_attack_attribute" id
 where
   id = get_id tree

instance (Eq label,Eq attribute) => Eq (Attack attribute label) where
  (Attack tree1 labels1 attributes1) == (Attack tree2 labels2 attributes2) =
    (tree1 == tree2) && (attributes1 == attributes2) && (labels1 == labels2)

compare_attacks :: Ord attribute
                => M.Map ID attribute
                -> ID
                -> M.Map ID attribute
                -> ID
                -> Ordering
compare_attacks attributes1 id1 attributes2 id2 | att1 < att2 = LT
                                                | att1 > att2 = GT
                                                | otherwise   = EQ
 where
  get_att attributes id = case M.lookup id attributes of
    Just att -> att
    Nothing -> error.show $ AttrNotFound "get_attack_attribute" id

  att1 = get_att attributes1 id1
  att2 = get_att attributes2 id2

instance (Ord label,Ord attribute) => Ord (Attack attribute label) where
  compare (Attack tree1 _ attributes1) (Attack tree2 _ attributes2) =
    compare_attacks attributes1 (get_id tree1) attributes2 (get_id tree2)

min_attacks :: (Ord label,Ord attribute)
            => [Attack attribute label]
            -> [Attack attribute label]
min_attacks attacks = [a | a <- attacks,a <= min_attack]
 where
   min_attack = minimum attacks
