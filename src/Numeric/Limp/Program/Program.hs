{-# LANGUAGE TemplateHaskell #-}
module Numeric.Limp.Program.Program where

import Numeric.Limp.Program.Linear
import Numeric.Limp.Program.Constraint
import Numeric.Limp.Program.Bounds

data Direction
 = Minimise
 | Maximise

data Program z r c
 = Program
   { _objective     :: Linear z r c KR
   , _direction     :: Direction
   , _constraints   :: Constraint z r c
   , _bounds        :: [Bounds z r c]
   }


-- relax :: Program z r -> Program Void (Either z r)
-- relax = undefined

