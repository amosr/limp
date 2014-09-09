-- | Convert linear constraints that only mention one variable to bounds
module Numeric.Limp.Canon.Simplify.Bounder where
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep

import Data.Either
import qualified Data.Map as M

type Bound z r c = (Either z r, (Maybe (R c), Maybe (R c)))


-- | Convert a single constraint into a bound, if possible.
--
-- > bounder $ Constraint (5 <= y <= 10)
-- > == Bound (Just 5) y (Just 10)
--
-- > bounder $ Constraint (5 <= 2y <= 10)
-- > == Bound (Just 2.5) y (Just 5)
--
bounderConstraint1 :: (Ord z, Ord r, Rep c) => Constraint1 z r c -> Maybe (Bound z r c)
bounderConstraint1 (C1 low (Linear mf) upp)
 | M.size mf == 1
 , [(k,c)]   <- M.toList mf
 , c /= 0
 = let fixup = (/ c)
       low'  = fmap fixup low
       upp'  = fmap fixup upp
       bounds
        | c >= 0
        = (low',upp')
        | otherwise
        = (upp',low')
   in  Just (k, bounds)

 | otherwise
 = Nothing
   

bounderConstraint :: (Ord z, Ord r, Rep c) => Constraint z r c -> (Constraint z r c, [Bound z r c])
bounderConstraint (Constraint cs)
 = let (cs', bs) = partitionEithers $ map bounderC cs
   in  (Constraint cs', bs)
 where
  bounderC c
   = case bounderConstraint1 c of
     Nothing -> Left c
     Just b  -> Right b
   

-- 
bounderProgram :: (Ord z, Ord r, Rep c) => Program z r c -> Program z r c
bounderProgram p
 = let (c',bs) = bounderConstraint $ _constraints p
   in p
    { _constraints = c'
    , _bounds      = foldl merge (_bounds p) bs }

 where
  merge m (k,v)
   = case M.lookup k m of
     Just v'
      -> M.insert k (mergeBounds v' v) m
     Nothing
      -> M.insert k                 v  m

