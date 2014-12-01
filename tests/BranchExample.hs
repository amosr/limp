module BranchExample where

import Numeric.Limp.Rep.Rep     as R
import Numeric.Limp.Rep.Arbitrary     as R
import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C
import Numeric.Limp.Solve.Simplex.Maps   as SM
import Numeric.Limp.Solve.Simplex.StandardForm   as ST
import Numeric.Limp.Solve.Branch.Simple  as B

import Numeric.Limp.Canon.Pretty
import Debug.Trace

import Control.Applicative

-- Dead simple ones -------------------------
-- x = 2
prog1 :: P.Program String String R.Arbitrary
prog1
 = P.maximise
    -- objective
        (z "x" 1)
    -- subject to
     (   z "x"  2 :<= con 5
     :&& z "x"  4 :>= con 7)
    []

-- x = 1, y = 2
prog2 :: P.Program String String R.Arbitrary
prog2
 = P.minimise
    -- objective
        (z "x" 1 .+. z "y" 1)
    -- subject to
     (   z "x"  2 :<= con 5 -- z "y" 1 .+. con 1
     :&& z "x"  1 :>= con 1 
     :&& z "y"  1 :<= con 4
     :&& z "y"  1 :>= con 1)
    [ lowerZ 0 "x" 
    , lowerZ 0 "y" ]


xkcd :: Direction -> P.Program String String R.Arbitrary
xkcd dir = P.program dir
           ( z1 mf .+.
             z1 ff .+.
             z1 ss .+.
             z1 hw .+.
             z1 ms .+.
             z1 sp )
           ( z mf mfp .+.
             z ff ffp .+.
             z ss ssp .+.
             z hw hwp .+.
             z ms msp .+.
             z sp spp :== con 1505 )
           [ lowerZ 0 mf
           , lowerZ 0 ff
           , lowerZ 0 ss
           , lowerZ 0 hw
           , lowerZ 0 ms
           , lowerZ 0 sp
            ]
  where
    (mf, mfp) = ("mixed-fruit",       215)
    (ff, ffp) = ("french-fries",      275)
    (ss, ssp) = ("side-salad",        335)
    (hw, hwp) = ("hot-wings",         355)
    (ms, msp) = ("mozzarella-sticks", 420)
    (sp, spp) = ("sampler-plate",     580)

test :: (Show z, Show r, Ord z, Ord r)
     => P.Program z r R.Arbitrary -> IO ()
test prog
 = let prog' = C.program prog
       
       simpl p = SM.simplex $ ST.standard p

       solver p
        | st <- ST.standard p
        -- , trace (ppr show show p) True
        , Just s' <- SM.simplex st
        -- , trace ("SAT") True
        , ass <- SM.assignment s'
         = Just ass
        | otherwise
        -- , trace ("unsat") True
        = Nothing
       bb    = B.branch solver
   in  do   
            -- putStrLn (show (simpl prog'))
            -- putStrLn (show (solver prog'))
            putStrLn (show (bb prog'))

