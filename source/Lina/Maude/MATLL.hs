{-# LANGUAGE OverloadedStrings #-}
module Lina.Maude.MATLL where

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
import Lina.Maude.Module
import Language.Maude.Syntax
import Language.Maude.Exec

matllFile :: IO FilePath
matllFile = get_maude_mod_absolute_path "MATLL.maude"

parenForm :: IAT -> String -> String
parenForm (Base _) str = str
parenForm _ str = "("++str++")"

toMATLL' :: IAT -> String
toMATLL' (Base id) = show id    
toMATLL' (OR _ p q) = (parenForm p p_str)++" || "++(parenForm q q_str)
 where
   p_str = toMATLL' p
   q_str = toMATLL' q
toMATLL' (AND _ p q) = (parenForm p p_str)++" . "++(parenForm q q_str)
 where
   p_str = toMATLL' p
   q_str = toMATLL' q
toMATLL' (SEQ _ p q) = (parenForm p p_str)++" ; "++(parenForm q q_str)
 where
   p_str = toMATLL' p
   q_str = toMATLL' q   

toMATLL_PAT :: PAttackTree label -> String
toMATLL_PAT (APAttackTree at _ _) = toMATLL' at

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
reID (APAttackTree _ labels1 _) (APAttackTree t2 labels2 _) = ST.evalState (reID' t2) (labels1,labels2) 

rw_maude :: String -> String -> IO Bool
rw_maude p q = do
  file <- matllFile  
  (SearchResult out xmlogs) <- search [file] (T.pack p) (T.pack cms)  
  return.not.null $ xmlogs
 where
   cms = "=>+ "++q

rw :: Ord label => PAttackTree label -> PAttackTree label -> IO (Either Error Bool)
rw p q = do  
  case reID p q of
    Right q' -> do
      let p_matll = toMATLL_PAT p
      let q_matll = toMATLL' q'
      o <- rw_maude p_matll q_matll
      return.return $ o
    Left e -> return $ throwError e

rw_PAT :: Ord label
       => PAttackTree label
       -> PAttackTree label
       -> IO ()
rw_PAT p q = do
  m <- p `rw` q
  putStrLn $ case m of
    Right b -> show b
    Left e -> show e

eq_PAT :: Ord label => PAttackTree label -> PAttackTree label -> IO ()
eq_PAT p q = do
  m1 <- p `rw` q
  m2 <- q `rw` p
  case (m1,m2) of
    (Right True,Right True) -> putStrLn "True"
    (Right _,Right _) -> putStrLn "False"
    (Left e,_) -> putStrLn.show $ e
    (_,Left e) -> putStrLn.show $ e

