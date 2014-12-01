module Simplexs where

import Numeric.Limp.Rep     as R
import Numeric.Limp.Program as P
import Numeric.Limp.Canon   as C
import Numeric.Limp.Solve.Simplex.Maps      as SM
import Numeric.Limp.Solve.Simplex.StandardForm  as ST

import qualified Data.Map as M


data Xs = X1 | X2 | X3
 deriving (Eq, Ord, Show)

-- Dead simple ones -------------------------
-- x1 = 10
prog1 :: P.Program () Xs R.IntDouble
prog1
 = P.maximise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 10)
    -- bounds omitted for now
    []

-- x1 = 10
prog2 :: P.Program () Xs R.IntDouble
prog2
 = P.maximise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 10)
    [ lowerR 0 X1 ]

-- x1 = 0
prog3 :: P.Program () Xs R.IntDouble
prog3
 = P.minimise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 10)
    [ lowerR 0 X1 ]

-- Unbounded!
prog4 :: P.Program () Xs R.IntDouble
prog4
 = P.minimise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 10)
    []


-- Two constraints! --------------

-- x = 10
prog5 :: P.Program () Xs R.IntDouble
prog5
 = P.maximise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 10
     :&& r X1  1 :>= con (-10))
    []

-- x = -10
prog6 :: P.Program () Xs R.IntDouble
prog6
 = P.minimise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 10
     :&& r X1  1 :>= con (-10))
    []


-- Now two variables -------------
-- x1 = 20, x2 = 10
prog7 :: P.Program () Xs R.IntDouble
prog7
 = P.maximise
    -- objective
        (r X1 1 .+. r X2 1)
    -- subject to
     (   r X1  1 :<= r X2 2
     :&& r X2  1 :<= con 10)
    [lowerR 0 X1, lowerR 0 X2]

-- x1 = 20, x2 = 10
prog8 :: P.Program () Xs R.IntDouble
prog8
 = P.maximise
    -- objective
        (r X1 1 .+. r X2 1)
    -- subject to
     (   r X1  1 :<= r X2 2
     :&& r X2  1 :<= con 10)
    [] -- [lowerR 0 X1, lowerR 0 X2]

-- Something where vars=0 isn't sat ------
-- x1 = 8
prog9 :: P.Program () Xs R.IntDouble
prog9
 = P.minimise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :>= con 8 
     :&& r X1  1 :<= con 10)
    [lowerR 0 X1]

-- x1 = 10
prog10 :: P.Program () Xs R.IntDouble
prog10
 = P.maximise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :>= con 8 
     :&& r X1  1 :<= con 10)
    [lowerR 0 X1]



-- An equality constraint ------------
-- x1 = 10
prog11 :: P.Program () Xs R.IntDouble
prog11
 = P.maximise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :== con 10 )
    [lowerR 0 X1]

-- x1 = 10
prog12 :: P.Program () Xs R.IntDouble
prog12
 = P.minimise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :== con 10 )
    [lowerR 0 X1]


-- From wikipedia ----------------
-- x1 = 2.142..., x3 = 3.571...
prog13 :: P.Program () Xs R.IntDouble
prog13
 = P.minimise
    -- objective
        (r X1 (-2) .+. r X2 (-3) .+. r X3 (-4))
    -- subject to
     (   r X1  3   .+. r X2 2    .+. r X3 1 :== con 10
     :&& r X1  2   .+. r X2 5    .+. r X3 3 :== con 15)
    [lowerR 0 X1
    ,lowerR 0 X2
    ,lowerR 0 X3]

-- x1 = 1.818..., x2 = 2.272...
prog14 :: P.Program () Xs R.IntDouble
prog14
 = P.maximise
    -- objective
        (r X1 (-2) .+. r X2 (-3) .+. r X3 (-4))
    -- subject to
     (   r X1  3   .+. r X2 2    .+. r X3 1 :== con 10
     :&& r X1  2   .+. r X2 5    .+. r X3 3 :== con 15)
    [lowerR 0 X1
    ,lowerR 0 X2
    ,lowerR 0 X3]

-- An equality constraint on unconstrained (+-) ------------
-- x1 = 10
prog15 :: P.Program () Xs R.IntDouble
prog15
 = P.maximise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :== con 10 )
    []

-- A lower bound greater than zero ------------
-- x1 = 5
prog16 :: P.Program () Xs R.IntDouble
prog16
 = P.minimise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 30 )
    [lowerR 5 X1]

-- Lower and upper bounds -------
-- x1 = 5
prog17 :: P.Program () Xs R.IntDouble
prog17
 = P.minimise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 30 )
    [lowerUpperR 5 X1 10]
-- x1 = 5
prog18 :: P.Program () Xs R.IntDouble
prog18
 = P.maximise
    -- objective
        (r X1 1)
    -- subject to
     (   r X1  1 :<= con 30 )
    [lowerUpperR 5 X1 10]




std :: (Ord z, Ord r, Rep c) => P.Program z r c -> Standard z r c
std = ST.standard . C.program




test :: P.Program () Xs IntDouble -> IO Bool
test p
 = case SM.simplex $ ST.standard $ C.program p of
   Nothing
    -> do   putStrLn "Error: simplex returned Nothing"
            putStrLn (show $ ST.standard $ C.program p)
            putStrLn (show $ SM.simplex1 $ ST.standard $ C.program p)
            return False

   Just s
    -> do   let (Assignment _ vars,obj) = SM.assignment s
            let vars'      = M.toList vars

            putStrLn (show $ ST.standard $ C.program p)
            putStrLn (show $ SM.simplex1 $ ST.standard $ C.program p)

            putStrLn "Vars:"
            putStrLn (show vars')
            putStrLn "Obj:"
            putStrLn (show obj)

            return True

