module SimplexExample where

import Numeric.Limp.Rep     as R
import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C
import Numeric.Limp.Solve.Simplex.Maps      as SM
import Numeric.Limp.Solve.Simplex.StandardForm  as ST

import Control.Monad
import qualified Data.Map as M


data Xs = X1 | X2 | X3
 deriving (Eq, Ord, Show)

prog :: P.Program () Xs R.IntDouble
prog
 = P.maximise
    -- objective
        (r X1 60 .+. r X2  30 .+. r X3  20)
    -- subject to
     (   r X1  8 .+. r X2   6 .+. r X3   1 :<= con 48
     :&& r X1  2 .+. r X2 1.5 .+. r X3 0.5 :<= con  8
     :&& r X1  4 .+. r X2   2 .+. r X3 1.5 :<= con 20
     :&&             r X2   1              :<= con  5)
    -- bounds ommitted for now
    [ lowerR 0 X1 , lowerR 0 X2 , lowerR 0 X3 ]
    -- []

test :: IO Bool
test
 = case SM.simplex $ ST.standard $ C.program prog of
   Nothing
    -> do   putStrLn "Error: simplex returned Nothing"
            putStrLn (show $ ST.standard $ C.program prog)
            putStrLn (show $ SM.simplex1 $ ST.standard $ C.program prog)
            return False

   Just s
    -> do   let (Assignment _ vars,obj) = SM.assignment s
            let vars'      = M.toList vars
            let e_vars = [(Right X1, 2.0), (Right X3, 8.0)] :: [(Either () Xs, R IntDouble)]
            let e_obj  = 280
            putStrLn "Vars:"
            putStrLn (show vars')
            putStrLn "Obj:"
            putStrLn (show obj)

            when (obj /= e_obj) $
                putStrLn ("Bad objective: should be " ++ show e_obj)
            when (vars' /= e_vars) $
                putStrLn ("Bad vars: should be "      ++ show e_vars)

            return (obj == e_obj && vars' == e_vars)

