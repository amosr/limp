-- | Functions for evaluating linear functions and checking constraints.
module Numeric.Limp.Program.Eval where
import Numeric.Limp.Rep
import Numeric.Limp.Program.Constraint
import Numeric.Limp.Program.Linear
import Numeric.Limp.Program.ResultKind

-- | Evaluate a linear function with given assignment.
-- If the linear function is purely integral, a @Z@ will be returned; otherwise, @R@.
eval :: (Rep c, Ord z, Ord r) => Assignment z r c -> Linear z r c k -> KRep k c
eval a (LZ ls c)
 = sum (map get ls) + c
 where
  get (l, co) = zOf a l * co

eval a (LR ls c)
 = sum (map get ls) + c
 where
  get (l, co) = zrOf a l * co


-- | Evaluate a linear function with given assignment, returning real value.
evalR :: (Rep c, Ord z, Ord r) => Assignment z r c -> Linear z r c k -> R c
evalR a l@(LZ{}) = fromZ (eval a l)
evalR a l@(LR{}) =        eval a l


-- | Check whether assignment satisfies constraint.
check :: (Rep c, Ord z, Ord r) => Assignment z r c -> Constraint z r c -> Bool
check ass = go
 where
  go (a :== b)
   = evalR ass a == evalR ass b
  go (a :<= b)
   = evalR ass a <= evalR ass b
  go (a :>= b)
   = evalR ass a >= evalR ass b

  -- They are both ints, so no conversion to R is necessary
  go (a :<  b)
   = eval  ass a <  eval  ass b
  go (a :>  b)
   = eval  ass a >  eval  ass b

  go (Between a b c)
   = evalR ass a <= evalR ass b && evalR ass b <= evalR ass c
  go (a :&& b)
   = go a && go b
  go (_ :! a)
   = go a

  go CTrue
   = True
