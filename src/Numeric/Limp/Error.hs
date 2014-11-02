-- | Reasons an analysis, simplification or solution could fail
module Numeric.Limp.Error where

-- | Give reason for being infeasible, if possible
data Infeasible
 = InfeasibleNotIntegral
 -- ^ An integer variable is constrained to be equal to a non-int
 | InfeasibleBoundEmpty
 -- ^ The bound on a variable or constraint is empty - lower bound is above upper.
 deriving (Eq,Show)
