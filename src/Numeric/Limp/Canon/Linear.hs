-- | Representation of subset of linear functions: only variables and coefficients, no constant summand
module Numeric.Limp.Canon.Linear where
import Numeric.Limp.Rep

import qualified Data.Map as M
import qualified Data.Set as S


-- | Linear function is represented as a map from either a integral variable or an real variable, to a real coefficient.
data Linear z r c
 = Linear (M.Map (Either z r) (R c))

deriving instance (Ord z, Ord r, Rep c) => Eq (Linear z r c)
deriving instance (Ord z, Ord r, Rep c) => Ord (Linear z r c)

-- | Create linear function from list of variables and coefficients
mkLinear :: (Ord z, Ord r, Rep c)
         => [(Either z r, R c)]
         -> Linear z r c
mkLinear zrs
 = Linear (M.fromListWith (+) zrs)


-- | Evaluate linear function with given assignment
evalR :: (Rep c, Ord z, Ord r) => Assignment z r c -> Linear z r c -> R c
evalR a (Linear ls)
 = sum (map get $ M.toList ls)
 where
  get (l, co) = zrOf a l * co


-- | Find set of all variables mentioned in function
varsOfLinear :: (Ord z, Ord r) => Linear z r c -> S.Set (Either z r)
varsOfLinear (Linear m)
 = M.keysSet m

