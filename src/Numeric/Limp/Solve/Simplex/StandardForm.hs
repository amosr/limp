-- | Standard form for programs:
-- only equalities, all variables >= 0
module Numeric.Limp.Solve.Simplex.StandardForm
    where
import Numeric.Limp.Rep
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import qualified Numeric.Limp.Canon.Program as C

import qualified Data.Map as M
import qualified Data.Set as S


type StandardRow z r c
    = (StandardLinear z r c, R c)

data Standard z r c
    = Standard
    { _objective   :: StandardRow z r c
    , _constraints :: M.Map (StandardVar z r) (StandardRow z r c)
    , _substs      :: StandardSubst z r c
    }
deriving instance (Show z, Show r, Show (R c)) => Show (Standard z r c)

type StandardSubst  z r c
    = M.Map (Either z r) (StandardRow z r c)

type StandardLinear z r c
    = M.Map (StandardVar z r) (R c)

data StandardVar z r
    -- | A normal variable
    = SV (Either z r)

    -- | A slack variable, introduced to make less-eq constraints into equalities
    | SVS Int

    -- | When a variable has a lower bound other than 0, we replace all occurences with
    -- with a new version minus the lower bound.
    -- x >= 5
    -- ==>
    -- Lx - 5 >= 5
    -- ==>
    -- Lx >= 0
    | SVLower (Either z r)

    -- | When unconstrained variables are encountered, they are replaced with
    -- x = SVPos x - SVNeg x
    -- so both parts can be constrained to >= 0.
    | SVPos (Either z r)
    | SVNeg (Either z r)
    deriving (Eq, Ord, Show)


addLinears
    :: (Ord z, Ord r, Rep c)
    => [(StandardLinear z r c, R c)] -> (StandardLinear z r c, R c)
addLinears []
 = (M.empty, 0)
addLinears ((lin,co):rs)
 = let (lin',co') = addLinears rs
   in  (M.unionWith (+) lin lin', co + co')


substLinear
    :: (Ord z, Ord r, Rep c)
    => StandardSubst z r c -> (StandardLinear z r c, R c) -> (StandardLinear z r c, R c)
substLinear sub (lin, co)
 = let (lin', co') = addLinears 
                   $ map subby 
                   $ M.toList lin
   in (lin', co + co')
 where
  subby (var, coeff)
   = case var of
      SV s
       | Just (vs,cnst) <- M.lookup s sub
       -> (M.map (*coeff) vs, -cnst)
      _
       -> (M.fromList [(var, coeff)], 0)


standard :: (Ord z, Ord r, Rep c)
        => C.Program z r c
        -> Standard z r c
standard p
 = Standard
 { _objective   = objective
 , _constraints = constraints
 , _substs      = substs }
 where
  fv = C.varsOfProgram p
  bs = C._bounds p

  objective
   = substLinear substs
    (standardOfLinear $ C._objective p, 0)

  constraints
   = M.fromList
   $ zipWith (\c s -> (s, substLinear substs $ c s))
   ( constrs ++ bounds )
   ( map SVS [1..] )

  substs
   = M.fromList
   $ concatMap substOf
   $ S.toList fv

  negPos v
   = [(v, (M.fromList [(SVPos v, 1), (SVNeg v, -1)], 0))]

  substOf v
   = case M.lookup v bs of
     Nothing
      -> negPos v
     Just (Nothing, Nothing)
      -> negPos v
     Just (Just 0, _)
      -> []
     Just (Just n, _)
      -> [(v, (M.fromList [(SVLower v, 1)], n)) ]
     _
      -> []

  bounds
   = concatMap linearOfBound
   $ M.toList
   $ C._bounds p

  linearOfBound (v,binds)
   = case binds of
     (_, Just n)
      -> [\s -> (M.fromList [(SV v, 1), (s, 1)], n)]
     _
      -> []

  Constraint cs = C._constraints p
  constrs
   = concatMap linearOfConstraint cs
  linearOfConstraint (C1 lo lin up)
   = let lin' = standardOfLinear lin
   in case (lo,up) of
      (Nothing,Nothing)
       -> []
      (Just lo', Nothing)
       -> [ lt lo' lin' ]
      (Nothing,  Just up')
       -> [ gt up' lin' ]
      (Just lo', Just up')
       -> [ lt lo' lin'
          , gt up' lin' ]


  lt lo lin s
   = ( M.union lin (M.fromList [(s,-1)])
     , lo )
  gt up lin s
   = ( M.union lin (M.fromList [(s, 1)])
     , up )

  standardOfLinear (Linear lin)
   = M.mapKeysMonotonic SV lin


--- 5 <= x1 <= 40
-- ==>
-- x1 subst Lx1+5
-- Lx1 + 5 <= 40
-- ==>
-- Lx1 <= 35

-- assignmentOfMap :: Standard z r c -> M.Map (StandardVar z r) (R c) -> Assignment z r c


