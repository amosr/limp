module Numeric.Limp.Program.Constraint where
import Numeric.Limp.Rep
import Numeric.Limp.Program.Linear

import Data.Monoid

data Constraint z r c where
 (:==)   :: Linear z r c k1  -> Linear z r c k2  -> Constraint z r c
 (:<=)   :: Linear z r c k1  -> Linear z r c k2  -> Constraint z r c
 (:<)    :: Linear z r c KZ  -> Linear z r c KZ  -> Constraint z r c
 (:>=)   :: Linear z r c k1  -> Linear z r c k2  -> Constraint z r c
 (:>)    :: Linear z r c KZ  -> Linear z r c KZ  -> Constraint z r c
 Between :: Linear z r c k1  -> Linear z r c k2  -> Linear z r c k3   -> Constraint z r c
 (:&&)   :: Constraint z r c -> Constraint z r c -> Constraint z r c
 (:!)    :: String           -> Constraint z r c -> Constraint z r c
 CTrue   ::                                         Constraint z r c
-- These are not all necessary, but I have a hunch that keeping some structure may be helpful in the future.
-- Also for pretty printing.
--
-- Less than is interesting: we can only construct a < b if both are integral.

infix  5 :==
infix  5 :<=
infix  5 :<
infix  5 :>=
infix  5 :>
infix  4 :!
infixr 3 :&&

check :: (Rep c, Ord z, Ord r) => Assignment z r c -> Constraint z r c -> Bool
check ass = go
 where
  -- ev :: Linear z r c k -> R c
  -- ev l = evalR ass l

  -- TODO should there be tolerance here?
  -- that's probably something that should go in Rep class
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

instance Monoid (Constraint z r c) where
 mempty  = CTrue
 mappend = (:&&)

