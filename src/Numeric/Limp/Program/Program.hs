-- | Definition of a whole program
module Numeric.Limp.Program.Program where

import Numeric.Limp.Program.Bounds
import Numeric.Limp.Program.Constraint
import Numeric.Limp.Program.Linear
import Numeric.Limp.Program.ResultKind
import Numeric.Limp.Rep

-- | Direction to optimise program in: minimise or maximise.
data Direction
 = Minimise
 | Maximise
   deriving Show

-- | Whole program, parameterised by:
--
--   [@z@] type of integer variables
--   [@r@] type of real variables
--   [@c@] representation of integers and reals (see 'Numeric.Limp.Rep.Rep')
--
data Program z r c
 = Program {
   -- | Optimisation direction
     _direction     :: Direction
   -- | The objective function
   , _objective     :: Linear z r c 'KR
   -- | All constraints bundled up with @:&&@.
   , _constraints   :: Constraint z r c
   -- | Upper and lower bounds of variables.
   -- Not all variables need to be mentioned, and if variables are mentioned multiple times, the intersection is used.
   , _bounds        :: [Bounds z r c]
   }

deriving instance (Show z, Show r, Show (Z c), Show (R c)) => (Show (Program z r c))

program :: Rep c => Direction -> Linear z r c k -> Constraint z r c -> [Bounds z r c] -> Program z r c
program dir obj constr bounds
 = Program dir (toR obj) constr bounds

minimise :: Rep c => Linear z r c k -> Constraint z r c -> [Bounds z r c] -> Program z r c
minimise
 = program Minimise
 

maximise :: Rep c => Linear z r c k -> Constraint z r c -> [Bounds z r c] -> Program z r c
maximise
 = program Maximise

