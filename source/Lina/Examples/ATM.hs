module Lina.Examples.ATM where

import Lina.AttackTree
import Lina.Maude.SAND

atm1 :: PAttackTree String
atm1 = start_PAT $
  seq_node "ATM attack"
    (and_node "get credentials"
       (base_na "steal card")
       (or_node "get PIN"
          (base_na "social engineer")
          (base_na "find a post-it")))
    (base_na "withdraw money")

atm2 :: PAttackTree String
atm2 = start_PAT $
  or_node "ATM attack"
    (seq_node "attack vector 1"
       (and_node "get credentials 1"
          (base_na "social engineer")
          (base_na "steal card"))
       (base_na "withdraw money"))
    (seq_node "attack vector 2"
       (and_node "get credentials 2"
          (base_na "steal card")
          (base_na "find a post-it"))
       (base_na "withdraw money"))
