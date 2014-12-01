-- | Representation of integers (Z) and reals (R) of similar precision.
-- Programs are abstracted over this, so that ideally in the future we could have a
-- solver that produces Integers and Rationals, instead of just Ints and Doubles.
--
-- We bundle Z and R up into a single representation instead of abstracting over both,
-- because we must be able to convert from Z to R without loss.
--
module Numeric.Limp.Rep
    ( module Numeric.Limp.Rep.Rep
    , module Numeric.Limp.Rep.IntDouble )
    where

import Numeric.Limp.Rep.Rep
import Numeric.Limp.Rep.IntDouble

