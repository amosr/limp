module Numeric.Limp.Canon.Constraint where
import Numeric.Limp.Rep
import Numeric.Limp.Canon.Linear

data Constraint z r c
 = Constraint [Constraint1 z r c]

data ConstraintType
 = CTGe0
 | CTEq0

data Constraint1 z r c
 = C1 (Linear z r c) ConstraintType

check :: Rep c => Assignment z r c -> Constraint z r c -> Bool
check a (Constraint cs) = all go cs
 where
  ev l = evalR a l

  go (C1 l CTGe0)
   = ev l >= 0
  go (C1 l CTEq0)
   = ev l == 0

