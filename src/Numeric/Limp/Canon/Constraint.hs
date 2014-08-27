module Numeric.Limp.Canon.Constraint where
import Numeric.Limp.Rep
import Numeric.Limp.Canon.Linear

import qualified Data.Set as S

data Constraint z r c
 = Constraint [Constraint1 z r c]

data Constraint1 z r c
 = C1 (Maybe (R c)) (Linear z r c) (Maybe (R c))

check :: (Rep c, Ord z, Ord r) => Assignment z r c -> Constraint z r c -> Bool
check a (Constraint cs) = all go cs
 where
  ev l = evalR a l

  go (C1 lower lin upper)
   = let lin' = ev lin
     in  maybe True (<= lin') lower
     &&  maybe True (lin' <=) upper


varsOfConstraint :: (Ord z, Ord r) => Constraint z r c -> S.Set (Either z r)
varsOfConstraint (Constraint cs)
 = S.unions
 $ map get cs
 where
  get (C1 _ lin _)
   = varsOfLinear lin

