-- | Front-end representation of programs.
-- See 'Numeric.Limp.Program.Program.Program' for the entire program;
-- 'Numeric.Limp.Program.Constraint.Constraint' for constraints such as less than or equal, greater than, etc;
-- and 'Numeric.Limp.Program.Linear.Linear' for linear functions.
module Numeric.Limp.Program
    ( -- | Each variable can have a lower or upper bound.
      module Numeric.Limp.Program.Bounds
      -- | Constraints such as less than or equal, greater than or equal, between,...
    , module Numeric.Limp.Program.Constraint
      -- | Functions for evaluating linear functions constraints for a given assignment of variables.
    , module Numeric.Limp.Program.Eval
      -- | Linear functions with constant coefficients on variables, and a constant addition.
    , module Numeric.Limp.Program.Linear
      -- | An entire program.
    , module Numeric.Limp.Program.Program
    ) where

import Numeric.Limp.Program.Bounds
import Numeric.Limp.Program.Constraint
import Numeric.Limp.Program.Eval
import Numeric.Limp.Program.Linear
import Numeric.Limp.Program.Program

