module Examples.Simple where

import Prelude hiding (or,and,seq)

import Lina

example' :: PAttackTree Int String
example' = 
       or "break in and steal password"
          (and "bribe and steal"
               (base 20 "bribe")
               (base 10 "steal"))
           (seq "break in then install"
                (base 50 "break in")
                (base 10 "install"))

example'' :: Conf Int -> AttackTree Int String
example'' conf = start conf $
                 and "LABEL"
                     example'
                     (base 14 "base attack")

