module Numeric.Limp.Canon.Linear where
import Numeric.Limp.Rep

data Linear z r c
 = Linear [(Either z r, R c)] (R c)

evalR :: Rep c => Assignment z r c -> Linear z r c -> R c
evalR a (Linear ls c)
 = sum (map get ls) + c
 where
  get (l, co) = zrOf a l * co

