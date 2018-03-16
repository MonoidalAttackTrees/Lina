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
          (base_wa 1 "base 2"))
       (base_wa 3 "base 3"))
    (seq_node "seq 1"
       (base_wa 2 "base 4")
       (or_node "or 3"
          (base_wa 3 "base 5")
          (base_wa 2 "base 6")))

at2 :: Conf Integer -> AttackTree Integer String
at2 conf = start_AT conf (insert pat2)

ex1 :: AttackTree Integer String
ex1 = start_AT maxAddMulConf $
  or_node "or node"
    (base_wa 1 "base 1")
    (base_wa 2 "base 2")

ex2 :: AttackTree Integer String
ex2 = start_AT maxAddMulConf $
  or_node "or node"
    (base_wa 1 "base 3")
    (base_wa 2 "base 4")

    
-- Autonomous Vehicle Attack
vehicle_attack :: APAttackTree Double String
vehicle_attack = start_PAT $
  or_node "Autonomous Vehicle Attack"
    (seq_node "external sensor attack"
       (base_wa 0.2 "modify street signs to cause wreck")
       (and_node "social engineering attack"
          (base_wa 0.6 "pose as mechanic")
          (base_wa 0.1 "install malware")))
    (seq_node "over night attack"
       (base_wa 0.05 "Find address where car is stored")
       (seq_node "compromise vehicle"
          (or_node "break in"
             (base_wa 0.8 "break window")
             (base_wa 0.5 "disable door alarm/locks"))
          (base_wa 0.1 "install malware")))

vehicle_AT :: AttackTree Double String
vehicle_AT = AttackTree vehicle_attack minMaxMaxConf

-- Autonomous Vehicle Attack: Composed
se_attack :: APAttackTree Double String
se_attack = start_PAT $
  and_node "social engineering attack"
     (base_wa 0.6 "pose as mechanic")
     (base_wa 0.1 "install malware")

bi_attack :: APAttackTree Double String
bi_attack = start_PAT $
  or_node "break in"
     (base_wa 0.8 "break window")
     (base_wa 0.5 "disable door alarm/locks")

cv_attack :: APAttackTree Double String
cv_attack = start_PAT $
  seq_node "compromise vehicle"
    (insert bi_attack)
    (base_wa 0.1 "install malware")

es_attack :: APAttackTree Double String
es_attack = start_PAT $
  seq_node "external sensor attack"
       (base_wa 0.2 "modify street signs to cause wreck")
       (insert se_attack)

on_attack :: APAttackTree Double String
on_attack = start_PAT $
  seq_node "over night attack"
     (base_wa 0.05 "Find address where car is stored")
     (insert cv_attack)

vehicle_attack' :: APAttackTree Double String
vehicle_attack' = start_PAT $
  or_node "Autonomous Vehicle Attack"
    (insert es_attack)
    (insert on_attack)

vehicle_AT' :: AttackTree Double String
vehicle_AT' = AttackTree vehicle_attack minMaxMaxConf
