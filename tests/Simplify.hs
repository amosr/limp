module Simplify where

import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C
import Numeric.Limp.Canon.Simplify as CS
import Numeric.Limp.Canon.Simplify.Subst as CS
import Numeric.Limp.Canon.Simplify.Bounder as CS
import Numeric.Limp.Canon.Simplify.Crunch as CS

import Numeric.Limp.Canon.Pretty

import Arbitrary.Assignment     as Arb
import Arbitrary.Var            as Arb
import Arbitrary.Program        as Arb
import Data.Monoid

import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Debug.Trace

tests = $(testGroupGenerator)

prop_bounder :: ProgramAss -> Property
prop_bounder (ProgramAss p a)
 = let cp     = C.program p
       cp'    = CS.bounderProgram cp
       valcp  = C.checkProgram a cp
       valcp' = C.checkProgram a cp'
   in  counterexample
       (unlines
         [ "CP: " ++ show cp
         , "CP':" ++ show cp'
         , "Val: " ++ show (valcp, valcp')])
       $ valcp == valcp'


prop_crunch :: ProgramAss -> Property
prop_crunch (ProgramAss p a)
 = let cp     = C.program p
       cp'    = CS.crunchProgram cp
       valcp  = C.checkProgram a cp
       valcp' = C.checkProgram a cp'
   in  counterexample
       (unlines
         [ "CP: " ++ show cp
         , "CP':" ++ show cp'
         , "Val: " ++ show (valcp, valcp')])
       $ valcp == valcp'


-- | I don't think this property is very interesting.
-- The real property should be something like:
--
-- > solve cp == solve (simplify cp)
--
prop_simplify :: Program' -> Property
prop_simplify p
 = let cp = C.program p
       (a', cp') = CS.simplify cp
       valcp  = C.checkProgram a' cp
       valcp' = C.checkProgram a' cp'
   in  counterexample
       (unlines
         [ "CP: " ++ show cp
         , "CP':" ++ show cp'
         , "Ass:" ++ show a'
         , "Val: " ++ show (valcp, valcp')])
       $ if valcp then valcp' else True


prop_subst_linear :: Vars -> Property
prop_subst_linear vs
 = forAll (Arb.linearR    vs) $ \f ->
   forAll (Arb.assignment vs) $ \a ->
   forAll (Arb.assignment vs) $ \b ->
     let (fc, _)   = C.linear f
         (fc', c') = substLinear a fc
     in  C.evalR (a <> b) fc == C.evalR b fc' + c'


-- subst can actually make a failing program pass.
-- so this test needs to be implication, not equivalence.
prop_subst_program :: Vars -> Property
prop_subst_program vs
 = forAll (Arb.program    vs) $ \f ->
   forAll (Arb.assignment vs) $ \a ->
   forAll (Arb.assignment vs) $ \b ->
     let fc    = C.program f
         fc'   = substProgram a fc
         both  = a <> b
         valcp = C.checkProgram both fc 
         valcp'= C.checkProgram b fc'
     in counterexample 
         (unlines
         [ "CP: " ++ show fc
         , "CP':" ++ show fc'
         , "Ass:" ++ show both
         , "Val: " ++ show (valcp, valcp')])
       $ if valcp then valcp' else True
     
     
