-- | Perform some simple optimisations on program
module Numeric.Limp.Canon.Simplify where
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep

import Numeric.Limp.Canon.Analyse.Constants

import Numeric.Limp.Canon.Simplify.Bounder
import Numeric.Limp.Canon.Simplify.Crunch
import Numeric.Limp.Canon.Simplify.Subst

simplify :: (Ord z, Ord r, Rep c) => Program z r c -> Program z r c
simplify p
 = let p'   = crunchProgram    p
       p''  = bounderProgram   p'
       sub  = constantsProgram p''
   in  if   assSize sub == 0
       then                             p''
       else simplify $ substProgram sub p''

