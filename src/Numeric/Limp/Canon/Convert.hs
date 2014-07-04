module Numeric.Limp.Canon.Convert where

import Numeric.Limp.Rep
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import qualified Numeric.Limp.Program.Constraint as P
import qualified Numeric.Limp.Program.Linear as P

linear :: Rep c => P.Linear z r c k -> Linear z r c
linear (P.LZ ls co)
 = Linear (map conv ls) (fromZ co)
 where
  conv (z,c) = (Left z, fromZ c)
linear (P.LR ls co)
 = Linear ls co

constraint :: Rep c => P.Constraint z r c -> Constraint z r c
constraint z
 = Constraint $ go z
 where
  constr l r = CSLe (linear l) (linear r)

  go (l P.:== r)
   = [CSEq (linear l) (linear r)]
  go (l P.:<= r)
   = [constr l r]
  go (l P.:>= r)
   = [constr r l]

  -- We know from the type of :< that both sides are int.
  -- That means we can safely convert (a < b) to (a + 1 <= b)
  go (l P.:<  r)
   = [constr (l P..+. P.c1) r]
  go (l P.:>  r)
   = [constr (r P..+. P.c1) l]

  go (P.Between a b c)
   = [constr a b, constr b c]
  go (a P.:&& b)
   = go a ++ go b
  go (_ P.:! a)
   = go a

  go  P.CTrue
   = []

-- lemma: check a (constraint c) == P.check a c


