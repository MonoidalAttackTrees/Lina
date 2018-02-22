{-# LANGUAGE OverloadedStrings #-}
module Maude.SAND where

import qualified Data.Text as T
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

parenForm :: SANDForm label -> String -> String
parenForm (SBase _ _) str = str
parenForm _ str = "("++str++")"

showSANDForm :: SANDForm label -> String
showSANDForm (SBase id _) = (show id)
showSANDForm (SOR p q)    = (parenForm p (showSANDForm p))++" || "++(parenForm q (showSANDForm q))
showSANDForm (SAND p q)   = (parenForm p (showSANDForm p))++" . "++(parenForm q (showSANDForm q))
showSANDForm (SSEQ p q)   = (parenForm p (showSANDForm p))++" ; "++(parenForm q (showSANDForm q))

instance Show (SANDForm label) where
    show = showSANDForm

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
