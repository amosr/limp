-- | Convert from "Numeric.Limp.Program" representation to simpler, so-called canonical representation.
module Numeric.Limp.Canon.Convert where

import Numeric.Limp.Rep

import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import Numeric.Limp.Canon.Program

import qualified Numeric.Limp.Program.Bounds     as P
import qualified Numeric.Limp.Program.Constraint as P
import qualified Numeric.Limp.Program.Linear     as P
import qualified Numeric.Limp.Program.Program    as P

import Control.Applicative
import qualified Data.Map as M


-- | Convert a Frontend 'P.Linear' into a Canon 'Linear'.
-- Returns the constant summand as well, as Canon Linear do not have these.
--
-- Should satisfy that
-- @forall a l. P.evalR a l == evalR a (fst $ linear l) + (snd $ linear l)@
linear :: (Rep c, Ord z, Ord r) => P.Linear z r c k -> (Linear z r c, R c)
linear (P.LZ ls co)
 = (mkLinear $ map conv ls, fromZ co)
 where
  conv (z,c) = (Left z, fromZ c)
linear (P.LR ls co)
 = (mkLinear ls, co)

-- | Convert a Frontend 'P.Constraint' into a Canon 'Constraint'.
--
-- Should satisfy that
-- @forall a c. P.check a c == check a (constraint c)@
constraint :: (Rep c, Ord z, Ord r) => P.Constraint z r c -> Constraint z r c
constraint z
 = Constraint $ go z
 where
  -- a <= b <==> b - a >= 0
  -- x + 1 <= y     ==> 1 <= y - x
  -- x + c <= y + d ==> -(d - c) <= y - x
  --
  -- x + c <= y + d
  --     c <= y + d - x
  -- c - d <= y - x
  -- -(d-c)<= y - x
  --
  cle l r
   = let (lin, co) = linear (r P..-. l)
     in  C1 (Just (-co)) lin Nothing

  -- a == b <==> a - b == 0
  ceq l r
   = let (lin, co) = linear (r P..-. l)
     in  C1 (Just (-co)) lin (Just (-co))

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



-- | Convert a Frontend 'P.Program' into a Canon 'Program'.
--
-- If we had a solve function that worked on either, it would ideally satisfy
-- @forall p. P.solve p == solve (program p)@
--
-- However, due to potential non-determinism in solving functions, it could be possible to get a different, but still optimal, solution:
--
-- > forall p. let aP = P.solve p
-- >                p' = program p
-- >                a  =   solve p'
-- >            in P.eval aP (P._objective p) == eval a (_objective p')
-- >            &&  check a (P._constraints p) && check ...
--
program :: (Rep c, Ord z, Ord r) => P.Program z r c -> Program z r c
program p
 = Program obj constr bnds
 where

  obj
   = case P._direction p of
        P.Minimise -> fst $ linear $       obj_orig
        P.Maximise -> fst $ linear $ P.neg obj_orig
  obj_orig
   = P._objective p

  constr
   = constraint $ P._constraints p

  bnds
   = M.fromListWith merge
   $ map extract
   $ P._bounds p

  merge (l1,u1) (l2,u2)
   = ( mmaybe max l1 l2
     , mmaybe min u1 u2 )

  mmaybe f a b
   = case (a,b) of
     (Nothing, Nothing)
      -> Nothing
     (Nothing, Just b')
      -> Just $ b'
     (Just a', Nothing)
      -> Just $ a'
     (Just a', Just b')
      -> Just $ f a' b'

  extract :: Rep c => P.Bounds z r c -> (Either z r, (Maybe (R c), Maybe (R c)))
  extract (P.BoundZ (l,k,u))
   = (Left k, (fromZ <$> l, fromZ <$> u))
  extract (P.BoundR (l,k,u))
   = (Right k, (l,u))

