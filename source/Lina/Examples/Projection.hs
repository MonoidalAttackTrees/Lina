module Lina.Examples.Projection where

import Lina.AttackTree

pat1 :: APAttackTree Integer String
pat1 = start_PAT $
  or_node "or 1"
    (and_node "and 1"
       (or_node "or 2"
          (base_wa 1 "base 1")
          (base_wa 2 "base 2"))
       (base_wa 3 "base 3"))
    (seq_node "seq 1"
       (base_wa 6 "base 4")
       (or_node "or 3"
          (base_wa 5 "base 5")
          (base_wa 6 "base 6")))

at1 :: Conf Integer -> AttackTree Integer String
at1 conf = start_AT conf (insert pat1)

pat2 :: APAttackTree Integer String
pat2 = start_PAT $
  or_node "or 1"
    (and_node "and 1"
       (or_node "or 2"
          (base_wa 1 "base 1")
          (base_wa 3 "base 2"))
       (base_wa 3 "base 3"))
    (seq_node "seq 1"
       (base_wa 2 "base 4")
       (or_node "or 3"
          (base_wa 3 "base 5")
          (base_wa 2 "base 6")))

at2 :: Conf Integer -> AttackTree Integer String
at2 conf = start_AT conf (insert pat2)
