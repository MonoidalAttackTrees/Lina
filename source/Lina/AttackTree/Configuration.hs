{-# LANGUAGE GADTs                #-}
module Lina.AttackTree.Configuration where

import Data.Semiring 

data Conf cost = Ord cost => Conf {
      andOp :: cost -> cost -> cost,
      seqOp :: cost -> cost -> cost
}

addMulConf :: (Ord a,Semiring a) => Conf a
addMulConf = Conf (.+.) (.*.)
