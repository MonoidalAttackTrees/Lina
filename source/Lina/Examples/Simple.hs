module Lina.Examples.Simple where

import Lina.AttackTree
import Lina.Maude.Causal

example' :: APAttackTree Int String
example' = start_PAT $
       or_node "break in and steal password"
          (and_node "bribe and steal"
               (base_wa 20 "bribe")
               (base_wa 10 "steal"))
           (seq_node "break in then install"
                (base_wa 50 "break in")
                (base_wa 10 "install"))

example'' :: Conf Int -> AttackTree Int String
example'' conf = start_AT conf $
                 and_node "LABEL"
                     (insert example')
                     (base_wa 14 "base attack")
