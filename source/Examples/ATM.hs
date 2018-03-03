module ATM where

import AttackTree.AttackTree
import Maude.SAND

atm1 :: ProcAT String
atm1 = start_PAT $
  seq_node "ATM atack"
    (and_node "get credentials"
       (base "steal card")
       (or_node "get PIN"
          (base "social engineer")
          (base "find a post-it")))
    (base "withdraw money")

atm2 :: ProcAT String
atm2 = start_PAT $
  or_node "ATM attack"
    (seq_node "attack vector 1"
       (and_node "get credentials 1"
          (base "social engineer")
          (base "steal card"))
       (base "withdraw money"))
    (seq_node "attack vector 2"
       (and_node "get credentials 2"
          (base "steal card")
          (base "find a post-it"))
       (base "withdraw money"))
