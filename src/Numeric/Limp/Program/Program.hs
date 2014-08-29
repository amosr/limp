-- | Definition of a whole program
module Numeric.Limp.Program.Program where

import Numeric.Limp.Program.Bounds
import Numeric.Limp.Program.Constraint
import Numeric.Limp.Program.Linear
import Numeric.Limp.Program.ResultKind

-- | Direction to optimise program in: minimise or maximise.
data Direction
 = Minimise
 | Maximise

-- | Whole program, parameterised by:
--
--   [@z@] type of integer variables
--   [@r@] type of real variables
--   [@c@] representation of integers and reals (see 'Numeric.Limp.Rep.Rep')
--
data Program z r c
 = Program {
   -- | The objective function
     _objective     :: Linear z r c KR
   -- | Optimisation direction
   , _direction     :: Direction
   -- | All constraints bundled up with @:&&@.
   , _constraints   :: Constraint z r c
   -- | Upper and lower bounds of variables.
   -- Not all variables need to be mentioned, and if variables are mentioned multiple times, the intersection is used.
   , _bounds        :: [Bounds z r c]
   }

