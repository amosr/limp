{-# LANGUAGE CPP #-}
-- | The simplest, stupidest possible branch and bound algorithm.
--
--
module Numeric.Limp.Solve.Branch.Simple
    (branch, makeIntegral)
    where
import Numeric.Limp.Canon.Program
import Numeric.Limp.Canon.Simplify
import Numeric.Limp.Rep

import Control.Monad
import qualified Data.Map as M

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

branch
    :: (Ord z, Ord r, Rep c)
    => (Program z r c -> Maybe (Assignment () (Either z r) c, R c))
    -> Program z r c
    -> Maybe (Assignment z r c, R c)
branch solver start_prog
 = go mempty start_prog
 where
  go ass p
   -- TODO:
   -- simp can actually change the objective function
   -- because Canon doesn't store a constant summand on the objective.
   -- we really need to return the modified summand and take that into account when
   -- choosing between two integer assignments.
   | Right (ass', p') <- simplify' ass p
   = do  (assRelax,co) <- solver p'
         case makeIntegral assRelax of
          Left (var, val)
           -> branchon p' ass' (Left var) val
          Right r
           -> Just (ass' <> r, co)
   | otherwise
   = Nothing

  branchon p ass var val
   = let lo = addBound p var (Just (fromZ $ truncate val + 1), Nothing)
         up = addBound p var (Nothing, Just (fromZ $ truncate val))
         loB     = go ass lo
         upB     = go ass up
     in case (loB, upB) of
        (Just (a1, o1), Just (a2, o2))
         | o1 > o2
         -> Just (a1, o1)
         | otherwise
         -> Just (a2, o2)
        (Just r, Nothing)
         -> Just r
        (Nothing, Just r)
         -> Just r
        (Nothing, Nothing)
         -> Nothing
     

  addBound p v b
   = let bs = _bounds p
         b' = maybe (Nothing,Nothing) id
            $ M.lookup v bs
     in  p { _bounds = M.insert v (mergeBounds b b') bs }

makeIntegral
    :: (Ord z, Ord r, Rep c)
    => Assignment () (Either z r) c
    -> Either (z, R c)
              (Assignment z r c)
makeIntegral (Assignment _ vs)
 =   uncurry Assignment
 <$> foldM go (M.empty, M.empty) (M.toList vs)
 where
  go (zs,rs) (var, val)
   = case var of
      Right r
       -> return (zs, M.insert r val rs)
      Left z
       | val' <- truncate val
       , val == fromZ val'
       -> return (M.insert z val' zs, rs)
       | otherwise
       -> Left (z, val)

