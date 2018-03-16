module Lina.Error (module Control.Monad.Except,Error(..),showError) where

import Control.Monad.Except

import Lina.Utils

data Error = IDNotFound String | LabelNotFound String ID | AttrNotFound String ID | MaudeNotBoolError String String | Errors [Error]

showError :: Error -> String
showError (LabelNotFound w id) = "Unable to find a label for id: "++(show id)++" in "++w
showError (IDNotFound w) = "Unable to find a id for label: TODO in "++w
showError (AttrNotFound w id)  = "Unable to find an attribute for id: "++(show id)++" in "++w
showError (MaudeNotBoolError w s) = "Muade returned a non-bool result: "++s++" in "++w
showError (Errors es) = foldr (\e s -> s++(show e)++"\n") "" es

instance Show Error where
  show = showError
