-- | Representation of linear constraints
module Numeric.Limp.Canon.Constraint where
import Numeric.Limp.Rep
import Numeric.Limp.Canon.Linear

import qualified Data.Set as S

-- | Conjunction of simple constraints
data Constraint z r c
 = Constraint [Constraint1 z r c]

-- | A simple constraint
data Constraint1 z r c
 -- | Maybe a lower bound, a linear function, and maybe an upper bound.
 --
 -- In order to be meaningful, at least one of lower or upper bound should be @Just@.
 = C1 (Maybe (R c)) (Linear z r c) (Maybe (R c))

-- | Check whether an assignment satisfies the constraint
check :: (Rep c, Ord z, Ord r) => Assignment z r c -> Constraint z r c -> Bool
check a (Constraint cs) = all go cs
 where
  ev l = evalR a l

  go (C1 lower lin upper)
   = let lin' = ev lin
     in  maybe True (<= lin') lower
     &&  maybe True (lin' <=) upper


-- | Get set of variables in constraint
varsOfConstraint :: (Ord z, Ord r) => Constraint z r c -> S.Set (Either z r)
varsOfConstraint (Constraint cs)
 = S.unions
 $ map get cs
 where
  get (C1 _ lin _)
   = varsOfLinear lin

