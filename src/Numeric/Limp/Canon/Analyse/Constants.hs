-- | Analyse a program to find all constants
module Numeric.Limp.Canon.Analyse.Constants where
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep

import qualified Data.Map as M


-- | Find the constants in a program, only by looking at the bounds with lo==up.
-- (See "Numeric.Limp.Canon.Simplify.Stride" to convert constraints to bounds)
constantsProgram :: (Ord z, Ord r, Rep c) => Program z r c -> Assignment z r c
constantsProgram p
 = mkAss $ concatMap eq $ M.toList $ _bounds p
 where

  eq (var, (Just lo, Just up))
   | lo == up
   = [(var, lo)]

  eq _
   = []

  mkAss ms
   = Assignment
      (M.fromList $ concatMap tkLeft ms)
      (M.fromList $ concatMap tkRight ms)

  tkLeft (Left z, v)
   -- Wow! What if the bounds aren't integral?
   -- Well, I guess the ILP solver will eventually figure out it's infeasible.
   -- Maybe it would be nice to trigger that error here.
   | v == (fromZ $ truncate v)
   = [(z, truncate v)]
  tkLeft _
   = []

  tkRight (Right r, v)
   = [(r, v)]
  tkRight _
   = []

