module Numeric.Limp.Canon.Convert where

import Numeric.Limp.Rep
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import qualified Numeric.Limp.Program.Constraint as P
import qualified Numeric.Limp.Program.Linear as P

linear :: (Rep c, Ord z, Ord r) => P.Linear z r c k -> Linear z r c
linear (P.LZ ls co)
 = mkLinear (map conv ls) (fromZ co)
 where
  conv (z,c) = (Left z, fromZ c)
linear (P.LR ls co)
 = mkLinear ls co

constraint :: (Rep c, Ord z, Ord r) => P.Constraint z r c -> Constraint z r c
constraint z
 = Constraint $ go z
 where
  -- a <= b <==> b - a >= 0
  cle l r = C1 (linear (r P..-. l)) CTGe0
  -- a == b <==> a - b == 0
  ceq l r = C1 (linear (l P..-. r)) CTEq0

  go (l P.:== r)
   = [ceq l r]
  go (l P.:<= r)
   = [cle l r]
  go (l P.:>= r)
   = [cle r l]

  -- We know from the type of :< that both sides are int.
  -- That means we can safely convert (a < b) to (a + 1 <= b)
  go (l P.:<  r)
   = [cle (l P..+. P.c1) r]
  go (l P.:>  r)
   = [cle (r P..+. P.c1) l]

  go (P.Between a b c)
   = [cle a b, cle b c]
  go (a P.:&& b)
   = go a ++ go b
  go (_ P.:! a)
   = go a

  go  P.CTrue
   = []

-- lemma: check a (constraint c) == P.check a c


