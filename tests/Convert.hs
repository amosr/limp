module Convert where

import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C

import Arbitrary.Program
import Data.Monoid

import Test.Tasty.QuickCheck
import Test.Tasty.TH


tests = $(testGroupGenerator)

prop_constraints_converted :: ProgramAss -> Bool
prop_constraints_converted (ProgramAss p a)
 =  P.checkProgram a  p
 == C.checkProgram a (C.program p)

