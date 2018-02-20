{-# LANGUAGE GADTs                #-}
module AttackTree.Configuration where

data Conf cost = Ord cost => Conf {
      orOp :: cost -> cost -> cost,
      andOp :: cost -> cost -> cost,
      seqOp :: cost -> cost -> cost
}

maxAddMulConf :: Conf Int
maxAddMulConf = Conf max (+) (*)