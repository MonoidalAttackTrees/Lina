{-# LANGUAGE OverloadedStrings #-}
module Lina.Maude.SAND-- (SANDForm(..),
                 --  eq,
                 --  normalize,
                 --  eq_AT,
                 --  eq_PT,
                 --  eq_APT,
                 --  eq_PAT)
where

import qualified Data.Text as T
import qualified Control.Monad.State as ST
import Control.Monad.Except
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Data.Bimap as B

import Lina.Utils
import Lina.Error
import Lina.AttackTree
import Language.Maude.Syntax
import Language.Maude.Exec


parenForm :: IAT -> String -> String
parenForm (Base _) str = str
parenForm _ str = "("++str++")"

toSAND' :: IAT -> String
toSAND' (Base id) = show id    
toSAND' (OR _ p q) = (parenForm p p_str)++" || "++(parenForm q q_str)
 where
   p_str = toSAND' p
   q_str = toSAND' q
toSAND' (AND _ p q) = (parenForm p p_str)++" . "++(parenForm q q_str)
 where
   p_str = toSAND' p
   q_str = toSAND' q
toSAND' (SEQ _ p q) = (parenForm p p_str)++" ; "++(parenForm q q_str)
 where
   p_str = toSAND' p
   q_str = toSAND' q   

toSAND_PAT :: PAttackTree label -> String
toSAND_PAT (PAttackTree at _) = toSAND' at

sandFile :: FilePath
sandFile = "/Users/heades/attack-trees/Lina/source/Lina/Maude/maude-modules/SAND.maude"

-- TODO: Factor out finding the max.
reID_node :: Ord label => (ID -> IAT -> IAT -> IAT) -> ID -> IAT -> IAT -> ST.State (B.Bimap label ID,B.Bimap label ID) (Either Error IAT)
reID_node node id l r = do
  (labels1,labels2) <- ST.get
  let (max_id,_) = B.findMaxR labels1
  l' <- reID' l
  r' <- reID' r
  case (l',r') of
    (Right l'',Right r'') -> return.return $ node (id+max_id+1) l'' r''
    (Left e,_) -> return $ throwError e
    (_,Left e) -> return $ throwError e
  
reID' :: Ord label => IAT -> ST.State (B.Bimap label ID,B.Bimap label ID) (Either Error IAT)
reID' (Base id) = do
  (labels1,labels2) <- ST.get
  case B.lookupR id labels2 of
    Just label -> do
      case B.lookup label labels1 of
        Just id' -> return.return $ Base id'
        Nothing -> return.throwError $ IDNotFound "reID'" 
    Nothing -> return.throwError $ LabelNotFound "reID'" id
reID' (OR  id l r) = reID_node OR id l r
reID' (AND id l r) = reID_node AND id l r
reID' (SEQ id l r) = reID_node SEQ id l r

reID :: Ord label => PAttackTree label -> PAttackTree label -> Either Error IAT
reID (PAttackTree _ labels1) (PAttackTree t2 labels2) = ST.evalState (reID' t2) (labels1,labels2) 

eq :: Ord label => PAttackTree label -> PAttackTree label -> IO (Either Error Bool)
eq p q = do
  let p_sand = toSAND_PAT p
  case reID p q of
    Right q' -> do
      let q_sand = toSAND' q'
      eq_maude p_sand q_sand
    Left e -> return $ throwError e

eq_PAT :: Ord label => PAttackTree label -> PAttackTree label -> IO ()
eq_PAT p q = do
  m <- p `eq` q
  case m of
    Right True -> putStrLn "True"
    Right False -> putStrLn "False"
    Left e -> putStrLn.show $ e

maude_bool :: String -> Either Error Bool
maude_bool "true" = return True
maude_bool "false" = return False
maude_bool s = throwError $ MaudeNotBoolError "maude_bool" s

eq_maude :: String -> String -> IO (Either Error Bool)
eq_maude p q = do
  (RewriteResult (Term _ result _) _) <- rewrite [sandFile] (T.pack cms)
  return $ maude_bool result
 where
   cms = "EQ(("++p++"), ("++q++"))"

