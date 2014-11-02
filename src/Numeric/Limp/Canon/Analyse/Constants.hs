-- | Analyse a program to find all constants
module Numeric.Limp.Canon.Analyse.Constants where
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep
import Numeric.Limp.Error

import qualified Data.Map as M


-- | Find the constants in a program, only by looking at the bounds with lo==up.
-- (See "Numeric.Limp.Canon.Simplify.Stride" to convert constraints to bounds)
constantsProgram :: (Ord z, Ord r, Rep c) => Program z r c -> Either Infeasible (Assignment z r c)
constantsProgram p
 = mkAss $ concatMap eq $ M.toList $ _bounds p
 where

  eq (var, (Just lo, Just up))
   | lo == up
   = [(var, lo)]

  eq _
   = []

  mkAss ms
   = do zs  <- mapM tkLeft  ms
        rs  <- mapM tkRight ms
        return $ Assignment (M.fromList $ concat zs)
                            (M.fromList $ concat rs)

  tkLeft (Left z, v)

   -- Wow! What if the bounds aren't integral?
   -- Well, I guess the ILP solver will eventually figure out it's infeasible.
   -- Maybe it would be nice to trigger that error here.
   | v /= (fromZ $ truncate v)
   = Left InfeasibleNotIntegral

   | otherwise
   = return [(z, truncate v)]

  tkLeft _
   = return []

  tkRight (Right r, v)
   = return [(r, v)]
  tkRight _
   = return []

