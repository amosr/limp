module Numeric.Limp.Canon.Pretty where
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Either

ppr :: (Show (Z c), Show (R c), Rep c, Show z, Show r, Ord z, Ord r) => (z -> String) -> (r -> String) -> Program z r c -> String
ppr pZ pR p
 = unlines
 [ "Minimize"
 , indent $ pprL $ _objective p
 , "Subject to"
 , pprCs $ _constraints p
 , "Bounds"
 , pprBs $ _bounds p
 , "Generals"
 , pprGs $ varsOfProgram p ]

 where
  indent = ("\t"++)

  pprV v
   = filter (/=' ') $ either pZ pR v

  pprL (Linear m)
   = pprLf
   $ M.toList m

  pprLf xs@((_,c): _)
   | c < 0
   = "-" ++ pprLfs xs
  pprLf xs
   = pprLfs xs

  pprLfs []
   = ""
  pprLfs [x]
   = pprL1 x
  pprLfs (x : rs@((_,c):_) )
   =  pprL1 x
   ++ (if c >= 0 then " + " else " - ")
   ++ pprLfs rs

  pprL1 (v,c) = show (abs c) ++ " " ++ pprV v

  pprCs (Constraint cs)
   = unlines $ map indent $ concatMap pprC cs

  pprC (C1 lo f up)
   =  case lo of
       Nothing  -> []
       Just lo' -> [pprL f ++ " >= " ++ show lo']
   ++ case up of
       Nothing  -> []
       Just up' -> [pprL f ++ " <= " ++ show up']

  pprLo (Just l)
   = show l ++ " <= "
  pprLo Nothing
   = ""

  pprUp (Just l)
   = " <= " ++ show l
  pprUp Nothing
   = ""

  pprBs m
   = unlines $ map (indent.pprB) $ M.toList m

  pprB (v, (lo,up))
   = pprLo lo ++ pprV v ++ pprUp up

  pprGs fvs
   = unlines $ map pprV
   $ filter isLeft
   $ S.toList fvs

