{-# LANGUAGE OverloadedStrings #-}
module Maude.SAND(SANDForm(..),
                  eq,
                  normalize,
                  eq_AT,
                  eq_PT,
                  eq_APT,
                  eq_PAT) where

import qualified Data.Text as T
import qualified Control.Monad.State as ST
import Control.Monad.Except
import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Text.Parsec.Expr

import AttackTree.AttackTree
import Language.Maude.Syntax
import Language.Maude.Exec


data SANDForm label = SBase ID label
             | SOR  (SANDForm label) (SANDForm label)
             | SAND (SANDForm label) (SANDForm label)
             | SSEQ (SANDForm label) (SANDForm label)
             deriving Eq

parenForm :: SANDForm label -> String -> String
parenForm (SBase _ _) str = str
parenForm _ str = "("++str++")"

showSANDForm :: SANDForm label -> ST.State ([(ID,ID)],ID) String
showSANDForm (SBase id _) = do
  (t,n) <- ST.get 
  case lookup id t of
    Nothing  -> do
      let id' = n
      ST.put ((id,id'):t,n+1)
      return $ show id'
    Just id' -> return $ show id'    
showSANDForm (SOR p q) = do
  p_str <- showSANDForm p
  q_str <- showSANDForm q
  return $ (parenForm p p_str)++" || "++(parenForm q q_str)
showSANDForm (SAND p q) = do
  p_str <- showSANDForm p
  q_str <- showSANDForm q
  return $ (parenForm p p_str)++" . "++(parenForm q q_str)
showSANDForm (SSEQ p q) =  do
  p_str <- showSANDForm p
  q_str <- showSANDForm q
  return $ (parenForm p p_str)++" ; "++(parenForm q q_str)

instance Show (SANDForm label) where
    show = \f -> ST.evalState (showSANDForm f) ([],1)

toSANDForm :: PAttackTree cost label -> SANDForm label
toSANDForm (Base id _ l)     = SBase id l
toSANDForm (OR _ _ atl atr)  = SOR (toSANDForm atl) (toSANDForm atr)
toSANDForm (AND _ _ atl atr) = SAND (toSANDForm atl) (toSANDForm atr)
toSANDForm (SEQ _ _ atl atr) = SSEQ (toSANDForm atl) (toSANDForm atr)

lexer = haskellStyle {
  Token.reservedOpNames = ["||",".",";"]
}
tokenizer = Token.makeTokenParser lexer

ident      = Token.identifier tokenizer
reserved   = Token.reserved tokenizer
reservedOp = Token.reservedOp tokenizer
parens     = Token.parens tokenizer
ws         = Token.whiteSpace tokenizer
symbol     = Token.symbol tokenizer
natural    = Token.natural tokenizer

expr    = buildExpressionParser table term        

sbase_parser = do
  id <- natural
  return $ SBase id ()

term    =  parens expr <|> sbase_parser

table   = [ [binary "." SAND AssocLeft, binary ";" SSEQ AssocLeft ]
          , [binary "||" SOR AssocLeft]
          ]

binary name fun assoc = Infix (do{ reservedOp name; return fun }) assoc

parse_sform = parse expr ""
                               
-- -- How should this work?
-- -- fromSANDForm :: SANDForm -> Conf a -> PAttackTree cost label -> AttackTree cost label
-- -- fromSANDForm = undefined             

sandFile :: FilePath
sandFile = "/Users/heades/attack-trees/Lina/source/Maude/maude-modules/SAND.maude"

eq_AT :: AttackTree cost label -> AttackTree cost label -> IO (Either String Bool)
eq_AT (AttackTree _ at1) (AttackTree _ at2) = eq_PT at1 at2

eq_APT :: AttackTree cost label -> PAttackTree cost label -> IO (Either String Bool)
eq_APT at1 at2 = eq_PAT at2 at1

eq_PAT :: PAttackTree cost label -> AttackTree cost label -> IO (Either String Bool)
eq_PAT at1 (AttackTree _ at2) = eq_PT at1 at2

eq_PT :: PAttackTree cost label -> PAttackTree cost label -> IO (Either String Bool)
eq_PT at1 at2 = eq (toSANDForm at1) (toSANDForm at2)

eq :: SANDForm label -> SANDForm label -> IO (Either String Bool)
eq p q = do
  putStrLn.show $ p
  putStrLn.show $ q  
  p_norm <- normalize p
  q_norm <- normalize q
  putStrLn.show $ p_norm
  putStrLn.show $ q_norm
  case (p_norm,q_norm) of
    (Right n, Right m) -> return $ Right $ n == m
    (Left msg, Right _) -> return $ throwError msg
    (Right _, Left msg) -> return $ throwError msg
    (Left msg1, Left msg2) -> return $ Left $ msg1 ++ "\n" ++ msg2

normalize :: SANDForm label -> IO (Either String (SANDForm ()))
normalize p = do
  (SearchResult out _) <- search [sandFile] (T.pack (show p)) (T.pack cmd)
  case (normalize_parser out) of
    Right p -> return $ return p
    Left msg -> return $ throwError $ show msg
 where   
   cmd = "=>! P:Formula"

normalize_parser :: T.Text -> Either String (SANDForm ())
normalize_parser out = case (parse_sform (T.unpack s_exp)) of
                         Right p -> Right p
                         Left msg -> Left $ show msg
 where
   s_exp = T.drop 6 $ head $ drop 3 $ T.lines out
         
lookup_label :: ID -> SANDForm label -> Maybe label
lookup_label id (SBase id' l) | id == id' = Just l
                              | otherwise = Nothing
lookup_label id (SOR lt rt) =
  case (lookup_label id lt) of
    Just id' -> Just id'
    Nothing -> lookup_label id rt

lookup_label id (SAND lt rt) =
  case (lookup_label id lt) of
    Just id' -> Just id'
    Nothing -> lookup_label id rt

lookup_label id (SSEQ lt rt) =
  case (lookup_label id lt) of
    Just id' -> Just id'
    Nothing -> lookup_label id rt               

ex_form1 :: SANDForm String
ex_form1 = (SBase 0 "") `SAND` ((SBase 1 "") `SOR` (SBase 2 ""))

ex_form2 :: SANDForm String
ex_form2 = ((SBase 0 "") `SAND` (SBase 1 "")) `SOR` ((SBase 0 "") `SAND` (SBase 1 ""))
