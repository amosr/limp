module Arbitrary.Program where

import qualified Numeric.Limp.Program as P
import Numeric.Limp.Rep

import Arbitrary.Var
import Arbitrary.Assignment

import Test.QuickCheck
import Control.Applicative

type Program' = P.Program ZVar RVar IntDouble

data ProgramAss = ProgramAss Program' Assignment'
 deriving Show

instance Arbitrary ProgramAss where
 arbitrary
  = do  a <- arbitrary
        ProgramAss <$> program a <*> assignment a

instance Arbitrary Program' where
 arbitrary = arbitrary >>= program


program :: Vars -> Gen Program'
program vs
 = do   dir  <- elements [P.Minimise, P.Maximise]
        
        obj  <- linearR        vs
        cons <- constraints    vs
        bnds <- listOf (bounds vs)

        return $ P.program dir obj cons bnds


linearR :: Vars -> Gen (P.Linear ZVar RVar IntDouble P.KR)
linearR (Vars zs rs)
 = do   let vs = map Left zs ++ map Right rs
        vs' <- listOf1 (elements vs)
        cs' <- infiniteListOf arbitrary
        summand <- arbitrary
        return $ P.LR (vs' `zip` cs') summand

linearZ :: Vars -> Gen (P.Linear ZVar RVar IntDouble P.KZ)
linearZ (Vars zs _rs)
 = do   vs' <- listOf1 (elements zs)
        cs' <- infiniteListOf arbitrary
        summand <- arbitrary
        return $ P.LZ (vs' `zip` cs') summand


constraints :: Vars -> Gen (P.Constraint ZVar RVar IntDouble)
constraints vs
 = oneof
 [ (P.:==)   <$> lR <*> lR
 , (P.:<=)   <$> lR <*> lR
 , (P.:<)    <$> lZ <*> lZ
 , (P.:>=)   <$> lR <*> lR
 , (P.:>)    <$> lZ <*> lZ
 , P.Between <$> lR <*> lR <*> lR
 , (P.:&&)   <$> constraints vs <*> constraints vs
 , return P.CTrue ]
 where
  lR = linearR vs
  lZ = linearZ vs


bounds :: Vars -> Gen (P.Bounds ZVar RVar IntDouble)
bounds (Vars zs rs)
 = oneof [bZ, bR]
 where
  bZ = do   v <- elements zs
            a <- arbitrary
            b <- arbitrary
            return $ P.BoundZ (a,v,b)

  bR = do   v <- elements rs
            a <- arbitrary
            b <- arbitrary
            return $ P.BoundR (a,v,b)

