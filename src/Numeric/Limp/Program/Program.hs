{-# LANGUAGE TemplateHaskell #-}
module Numeric.Limp.Program.Program where

import Numeric.Limp.Program.Linear
import Numeric.Limp.Program.Constraint

import Control.Lens

data Program z r c
 = Program
   { _objective     :: Linear z r c KR
   , _constraints   :: Constraint z r c
   }
makeLenses ''Program



-- relax :: Program z r -> Program Void (Either z r)
-- relax = undefined

