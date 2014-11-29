module BranchExample where

import Numeric.Limp.Rep     as R
import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C
import Numeric.Limp.Solve.Simplex.Maps   as SM
import Numeric.Limp.Solve.Simplex.StandardForm   as ST
import Numeric.Limp.Solve.Branch.Simple  as B

import Control.Applicative


xkcd :: Direction -> P.Program String String R.IntDouble
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
           []
  where
    (mf, mfp) = ("mixed-fruit",       215)
    (ff, ffp) = ("french-fries",      275)
    (ss, ssp) = ("side-salad",        335)
    (hw, hwp) = ("hot-wings",         355)
    (ms, msp) = ("mozzarella-sticks", 420)
    (sp, spp) = ("sampler-plate",     580)

test :: IO ()
test
 = let prog' = C.program $ xkcd Maximise
       
       simpl p = SM.simplex $ ST.standard p

       solver p = SM.assignment <$> simpl p
       bb    = B.branch solver
   in  do   
            putStrLn (show (simpl prog'))
            putStrLn (show (bb prog'))
            putStrLn (show (solver prog'))

