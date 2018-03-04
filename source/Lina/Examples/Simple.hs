module Lina.Examples.Simple where

import Lina.AttackTree
import Lina.Maude.SAND

example' :: PAttackTree Int String
example' = start_PAT $
       or_node "break in and steal password"
          (and_node "bribe and steal"
               (base 20 "bribe")
               (base 10 "steal"))
           (seq_node "break in then install"
                (base 50 "break in")
                (base 10 "install"))

example'' :: Conf Int -> AttackTree Int String
example'' conf = start_AT conf $
                 and_node "LABEL"
                     example'
                     (base 14 "base attack")
