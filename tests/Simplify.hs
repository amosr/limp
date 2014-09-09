module Simplify where

import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C
import Numeric.Limp.Canon.Simplify as CS
import Numeric.Limp.Canon.Simplify.Subst as CS

import Numeric.Limp.Canon.Pretty

import Arbitrary.Program
import Data.Monoid

import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Debug.Trace

tests = $(testGroupGenerator)

-- | I don't think this property is very interesting.
-- The real property should be something like:
--
-- > solve cp == solve (simplify cp)
--
prop_simplify :: Program' -> Bool
prop_simplify p
 = let cp = C.program p
       (a', cp') = CS.simplify cp
       valcp  = C.checkProgram a' cp
       valcp' = C.checkProgram a' cp'
   in  valcp == valcp'
       --'if   valcp
       -- then valcp'
       -- else True

