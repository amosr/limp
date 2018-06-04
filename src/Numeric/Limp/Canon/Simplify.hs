{-# LANGUAGE CPP #-}
-- | Perform some simple optimisations on program
module Numeric.Limp.Canon.Simplify where
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep
import Numeric.Limp.Error

import Numeric.Limp.Canon.Analyse.Constants

import Numeric.Limp.Canon.Simplify.Bounder
import Numeric.Limp.Canon.Simplify.Crunch
import Numeric.Limp.Canon.Simplify.Subst

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

simplify :: (Ord z, Ord r, Rep c) => Program z r c -> Either Infeasible (Assignment z r c, Program z r c)
simplify p
 = simplify' mempty p

simplify' :: (Ord z, Ord r, Rep c) => Assignment z r c -> Program z r c -> Either Infeasible (Assignment z r c, Program z r c)
simplify' sub1 p
 = do  let p'   = crunchProgram    p
       p''  <- bounderProgram   p'
       sub2 <- constantsProgram p''
       if   assSize sub2 == 0
       then return (sub1, p'')
       else simplify' (sub1 <> sub2) (substProgram sub2 p'')


