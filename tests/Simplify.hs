module Simplify where

import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C
import Numeric.Limp.Canon.Simplify as CS
import Numeric.Limp.Canon.Simplify.Subst as CS
import Numeric.Limp.Canon.Simplify.Bounder as CS
import Numeric.Limp.Canon.Simplify.Crunch as CS

import Numeric.Limp.Canon.Pretty

import Arbitrary.Program
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
       $ valcp == valcp'

