module Lina.Examples.Specializations where

import Lina.AttackTree
import Lina.Maude.MATLL

-- Figure 1: Horne et al.
enc_data1 :: PAttackTree String
enc_data1 = start_PAT $
  and_node "obtain secret"
    (or_node "obtain encryped file"
       (base_na "bribe sysadmin")
       (base_na "steal backup"))
    (seq_node "obtain password"
       (base_na "break into system")
       (base_na "install keylogger"))

-- Figure 2: Horne et al.
enc_data2 :: PAttackTree String
enc_data2 = start_PAT $
  or_node "obtain secret"
    (and_node "obtain secret via sysadmin"
       (base_na "bribe sysadmin")
          (seq_node "obtain password"
             (base_na "break into system")
             (base_na "install keylogger")))
    (seq_node "break in, then obtain secrect"
       (base_na "break into system")
       (and_node "obtain secret from inside"
         (base_na "install keylogger")
         (base_na "steal backup")))

-- Figure 3(b): Horne et al.
enc_data3 :: PAttackTree String
enc_data3 = start_PAT $
  seq_node "break in, then obtain secret"
    (base_na "break into system")
    (and_node "obtain secret from inside"
      (base_na "install keylogger")
      (base_na "steal backup"))
