module Numeric.Limp.Canon.Linear where
import Numeric.Limp.Rep

import qualified Data.Map as M

data Linear z r c
 = Linear (M.Map (Either z r) (R c)) (R c)

mkLinear :: (Ord z, Ord r)
         => [(Either z r, R c)]
         -> (R c)
         -> Linear z r c
mkLinear zrs c
 = Linear (M.fromList zrs) c

evalR :: Rep c => Assignment z r c -> Linear z r c -> R c
evalR a (Linear ls c)
 = sum (map get $ M.toList ls) + c
 where
  get (l, co) = zrOf a l * co

