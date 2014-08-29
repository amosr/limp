-- | A simpler representation of programs.
-- The frontend representation ("Numeric.Limp.Program") has many different kinds of constraints
-- (@<=@, @<@, @==@, @between@), as well as constant additions on each linear function
-- (eg. @x + 2y + 5@).
-- The so-called canonical representation removes the constant addition from each linear constraint,
-- and converts each constraint (@Lin Op Lin@) to (@Num <= Lin <= Num@).
--
-- The most interesting function here is 'Numeric.Limp.Canon.Convert.program' for converting
-- from Program representation to Canon.
module Numeric.Limp.Canon
    ( module Numeric.Limp.Canon.Linear
    , module Numeric.Limp.Canon.Constraint
    , module Numeric.Limp.Canon.Program
    , module Numeric.Limp.Canon.Convert
    ) where

import Numeric.Limp.Canon.Linear
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Program
import Numeric.Limp.Canon.Convert

