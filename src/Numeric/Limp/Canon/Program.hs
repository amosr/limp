-- | Canon representation of linear program
module Numeric.Limp.Canon.Program where

import Numeric.Limp.Canon.Linear
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Rep

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

-- | A program represented by objective, constraints and bounds.
-- There is no need for an optimisation direction; the objective is just negated.
data Program z r c
 = Program
   { _objective     :: Linear z r c
   , _constraints   :: Constraint z r c
   , _bounds        :: Map (Either z r) (Maybe (R c), Maybe (R c))
   }


-- | Find set of all variables mentioned in program
varsOfProgram :: (Ord z, Ord r) => Program z r c -> Set (Either z r)
varsOfProgram p
 = S.unions
 [ varsOfLinear     $ _objective   p
 , varsOfConstraint $ _constraints p
 , M.keysSet        $ _bounds      p ]


-- | Merge some lower and upper bounds
mergeBounds :: Rep c => (Maybe (R c), Maybe (R c)) -> (Maybe (R c), Maybe (R c)) -> (Maybe (R c), Maybe (R c))
mergeBounds (l1,u1) (l2,u2)
 = ( mmaybe max l1 l2
   , mmaybe min u1 u2 )
 where
  mmaybe f a b
   = case (a,b) of
    (Nothing, Nothing)
     -> Nothing
    (Nothing, Just b')
     -> Just $ b'
    (Just a', Nothing)
     -> Just $ a'
    (Just a', Just b')
     -> Just $ f a' b'

