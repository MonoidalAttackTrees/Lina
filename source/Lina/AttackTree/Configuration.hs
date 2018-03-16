{-# LANGUAGE GADTs #-}
module Lina.AttackTree.Configuration where

import Data.Semiring 

data Conf attribute = (Ord attribute) => Conf {
  orOp  :: attribute -> attribute -> attribute,
  andOp :: attribute -> attribute -> attribute,
  seqOp :: attribute -> attribute -> attribute
}

maxAddMulConf :: (Ord a,Semiring a) => Conf a
maxAddMulConf = Conf max (.+.) (.*.)

minMaxMaxConf :: (Ord a,Semiring a) => Conf a
minMaxMaxConf = Conf min max max

minMaxAddConf :: (Ord a,Semiring a) => Conf a
minMaxAddConf = Conf min max (.+.)
