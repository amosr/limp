-- | Crunch together all constraints with same linear function
module Numeric.Limp.Canon.Simplify.Crunch where
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep

import Data.List
import Data.Function
import Data.Maybe

-- | Crunch the constraints in some program
crunchProgram :: (Ord z, Ord r, Rep c) => Program z r c -> Program z r c
crunchProgram p
 = p { _constraints = crunchConstraint $ _constraints p }

-- | Crunch some constraints.
-- Constraints with the same function, for example
--
-- >              2x + y    < 5
-- > &&   0 <     2x + y
-- > &&           2x + y    < 10
--
-- becomes
--
-- >      0 <     2x + y    < 5
--
-- This should satisfy:
--
-- > forall a c. check a c == check a (crunchConstraint c)
-- > forall a.   length (checkConstraint c) <= length c
--
crunchConstraint :: (Ord z, Ord r, Rep c) => Constraint z r c -> Constraint z r c
crunchConstraint (Constraint cs)
 = Constraint
 $ map crunchC
 $ groupBy ((==) `on` getLin) cs
 where
  getLin (C1 _   lin _  ) = lin
  getLow (C1 low _   _  ) = low
  getUpp (C1 _   _   upp) = upp

  crunchC grp@(c:_)
   = let low = compareMaybes maximum $ map getLow grp
         upp = compareMaybes minimum $ map getUpp grp
     in  C1 low (getLin c) upp

  crunchC []
   = error "Impossible - groupBy should not produce empty lists"

  compareMaybes f ms
   = case catMaybes ms of
      ms'@(_:_) -> Just $ f ms'
      []        -> Nothing

