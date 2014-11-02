-- | Convert linear constraints that only mention one variable to bounds
module Numeric.Limp.Canon.Simplify.Bounder where
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep
import Numeric.Limp.Error

import Control.Applicative
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
-- > bounder $ Constraint (10 <= 2y <= 5)
-- > == Left InfeasibleBoundEmpty
--
bounderConstraint1 :: (Ord z, Ord r, Rep c) => Constraint1 z r c -> Either Infeasible (Maybe (Bound z r c))
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

       valid
        | (Just lo, Just hi) <- bounds
        = lo <= hi
        | otherwise
        = True

   in  if  valid
       then Right $ Just (k, bounds)
       else Left InfeasibleNotIntegral

 | otherwise
 = Right Nothing
   

bounderConstraint :: (Ord z, Ord r, Rep c) => Constraint z r c -> Either Infeasible (Constraint z r c, [Bound z r c])
bounderConstraint (Constraint cs)
 = do   (cs', bs) <- partitionEithers <$> mapM bounderC cs
        return      (Constraint cs', bs)
 where
  bounderC c
   = do c' <- bounderConstraint1 c
        return $ case c' of
            Nothing -> Left c
            Just b  -> Right b
   

-- 
bounderProgram :: (Ord z, Ord r, Rep c) => Program z r c -> Either Infeasible (Program z r c)
bounderProgram p
 = do   (c',bs) <- bounderConstraint $ _constraints p
        return $ p
            { _constraints = c'
            , _bounds      = foldl merge (_bounds p) bs }
 where
  merge m (k,v)
   = case M.lookup k m of
     Just v'
      -> M.insert k (mergeBounds v' v) m
     Nothing
      -> M.insert k                 v  m

