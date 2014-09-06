-- | Substitute an assignment into functions, constraints and programs
module Numeric.Limp.Canon.Simplify.Subst where
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep

import qualified Data.Map as M


-- | Substitute assignment into linear function.
-- However, 'Linear' isn't quite a linear function! That is, it doesn't have a constant summand.
-- So we must return the constant summand we lose.
--
-- Satisfies:
--
-- > forall a b f.
-- > let (f', c') = substLinear a f
-- > in  eval (a <> b) f == eval b f' + c'
--
substLinear :: (Ord z, Ord r, Rep c) => Assignment z r c -> Linear z r c -> (Linear z r c, R c)
substLinear (Assignment mz mr) (Linear mf)
 = ( Linear $ M.fromList $ concatMap update mf'
   ,          sum        $ map       getC   mf' )
 where
  mf' = M.toList mf

  get (v,co)
   | Left  z <- v
   , Just zv <- M.lookup z mz
   = Just $ fromZ zv * co
   | Right r <- v
   , Just rv <- M.lookup r mr
   = Just $       rv * co

   | otherwise
   = Nothing

  update vc
   | Just _ <- get vc
   = []
   | otherwise
   = [vc]

  getC vc
   | Just n <- get vc
   = n
   | otherwise
   = 0


-- | Substitute assignment into a single linear constraint.
-- See 'substConstraint'.
--
-- > 5 <= 2x + y <= 10
-- > subst (y := 3)
-- > 2 <= 2x     <= 7
--
substConstraint1 :: (Ord z, Ord r, Rep c) => Assignment z r c -> Constraint1 z r c -> Constraint1 z r c
substConstraint1 ass (C1 low lin upp)
 = let (lin', const') = substLinear ass lin
       fixup bound    = bound - const'
   in C1 (fmap fixup low) lin' (fmap fixup upp)


-- | Substitute assignment into a set of linear constraints.
-- Satisfies:
--
-- > forall a b f.
-- > let c' = substConstraint a c
-- > in  check (a <> b) c == check b c'
--
substConstraint :: (Ord z, Ord r, Rep c) => Assignment z r c -> Constraint z r c -> Constraint z r c
substConstraint ass (Constraint cs)
 = Constraint
 $ map (substConstraint1 ass) cs


-- | Substitute assignment into a program.
-- What does this satisfy? Hm.
substProgram :: (Ord z, Ord r, Rep c) => Assignment z r c -> Program z r c -> Program z r c
substProgram ass@(Assignment mz mr) p
 = p
 { _objective   = fst $ substLinear     ass $ _objective   p
 , _constraints =       substConstraint ass $ _constraints p
 , _bounds      =       cullBounds          $ _bounds      p
 }
 where
  cullBounds
   = M.mapMaybeWithKey cullB

  cullB k v

   | Left  z <- k
   , Just  _ <- M.lookup z mz
   = Nothing
   | Right r <- k
   , Just  _ <- M.lookup r mr
   = Nothing

   | otherwise
   = Just v


