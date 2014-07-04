module Numeric.Limp.Canon.Constraint where
import Numeric.Limp.Rep
import Numeric.Limp.Canon.Linear

data Constraint z r c
 = Constraint [Constraint1 z r c]

data Constraint1 z r c
 = CSLe (Linear z r c) (Linear z r c)
 | CSEq (Linear z r c) (Linear z r c)

check :: Rep c => Assignment z r c -> Constraint z r c -> Bool
check a (Constraint cs) = all go cs
 where
  ev l = evalR a l

  go (CSLe l r)
   = ev l <= ev r
  go (CSEq l r)
   = ev l == ev r

