module Lina.Examples.Projection where

import Lina.AttackTree

pat1 :: PAttackTree Integer String
pat1 = start_PAT $
  or_node "or 1"
    (and_node "and 1"
       (or_node "or 2"
          (base 1 "base 1")
          (base 2 "base 2"))
       (base 3 "base 3"))
    (seq_node "seq 1"
       (base 6 "base 4")
       (or_node "or 3"
          (base 5 "base 5")
          (base 6 "base 6")))

at1 :: Conf Integer -> AttackTree Integer String
at1 conf = start_AT conf pat1

rat1 :: Conf Integer -> RAttackTree Integer String
rat1 conf = eval $ at1 conf

attacks1 :: Conf Integer -> [Attack Integer String]
attacks1 conf = get_attacks conf $ rat1 conf

pat2 :: PAttackTree Integer String
pat2 = start_PAT $
  or_node "or 1"
    (and_node "and 1"
       (or_node "or 2"
          (base 1 "base 1")
          (base 3 "base 2"))
       (base 3 "base 3"))
    (seq_node "seq 1"
       (base 2 "base 4")
       (or_node "or 3"
          (base 3 "base 5")
          (base 2 "base 6")))

at2 :: Conf Integer -> AttackTree Integer String
at2 conf = start_AT conf pat2

rat2 :: Conf Integer -> RAttackTree Integer String
rat2 conf = eval $ at2 conf

attacks2 :: Conf Integer -> [Attack Integer String]
attacks2 conf = get_attacks conf $ rat2 conf
