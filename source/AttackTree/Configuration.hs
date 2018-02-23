{-# LANGUAGE GADTs                #-}
module AttackTree.Configuration where

import Data.Semiring 

data Conf cost = Ord cost => Conf {
      orOp :: cost -> cost -> cost,
      andOp :: cost -> cost -> cost,
      seqOp :: cost -> cost -> cost
}

maxAddMulConf :: (Ord a,Semiring a) => Conf a
maxAddMulConf = Conf max (.+.) (.*.)
