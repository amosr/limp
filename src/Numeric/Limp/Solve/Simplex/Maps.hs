-- | The simplest, stupidest possible simplex algorithm.
-- The idea here is to be slow, but "obviously correct" so other algorithms
-- can be verified against it.
--
-- That's the plan, at least. For now this is just a first cut of trying to implement simplex.
--
module Numeric.Limp.Solve.Simplex.Maps
    where
import Numeric.Limp.Rep
import Numeric.Limp.Canon.Constraint
import Numeric.Limp.Canon.Linear
import Numeric.Limp.Canon.Program

import Control.Arrow
import qualified Data.Map as M
import Data.Function (on)
import Data.List (minimumBy)

-- | The type of variables in a simplex row.
-- A simplex row is an equality constraint, whereas our Canon constraints
-- are LOW <= lin <= UPP, so we introduce a new
-- "slack variable" which is >= 0, such that lin + slack = (UPP - LOW).
data SimplexVar z r
    -- | A  Z-value
    = SVZ z
    -- | An R-value
    | SVR r
    -- | A slack variable, introduced to make less-eq constraints into equalities
    | SVS Int
    deriving (Eq, Ord, Show)


type SimplexRow z r c = (M.Map (SimplexVar z r) (R c), R c)
type SlackCounter     = Int


data  SimplexMap z r c
    = SimplexMap
    { _rows :: [(SimplexVar z r, SimplexRow z r c)]
    , _obj  :: SimplexRow z r c
    -- TODO: if "z" or "r" are not >= 0,
    -- we need to move them by some amount?
    }

deriving instance (Show z, Show r, Show (R c)) => Show (SimplexMap z r c)


-- | Transform a canonical program into a sparse simplex tableau.
-- The tableau is represented by Maps for simplicity.
relax   :: (Ord z, Ord r, Rep c)
        => Program z r c -> SimplexMap z r c
relax p
 = SimplexMap
 { _rows = rows
 , _obj  = (mapOfLinear $ _objective p, 0) }
 where
    rows
     = zipWith (\slack row -> (SVS slack, row (SVS slack)))
        [1..]
        (  doConstraints (_constraints p)
        ++ doBounds      (_bounds      p))

    doConstraints (Constraint cs)
     = concatMap rowOfConstraint cs

    doBounds bounds
     = concatMap (uncurry rowOfBound)
     $ M.toList bounds


varOfEither :: Either z r -> SimplexVar z r
varOfEither (Left  z) = SVZ z
varOfEither (Right r) = SVR r


mapOfLinear :: Linear z r c -> M.Map (SimplexVar z r) (R c)
mapOfLinear (Linear m)
 = M.mapKeysMonotonic varOfEither m


rowOfBound  :: (Ord z, Ord r, Rep c)
            => Either z r -> (Maybe (R c), Maybe (R c)) -> [SimplexVar z r -> SimplexRow z r c]
rowOfBound v (lo,up)
 = case range lo up of
    Lower l
     -> [\s ->
        ( M.fromList [(varOfEither v, -1), (s, 1)]
        , l ) ]
    Upper u
     -> [\s ->
        ( M.fromList [(varOfEither v, 1), (s, 1)]
        , u ) ]
    Both _ _
     -> rowOfBound v (lo, Nothing)
     ++ rowOfBound v (Nothing, up)
    Unconstrained
     -> []


rowOfConstraint :: (Ord z, Ord r, Rep c)
                => Constraint1 z r c
                -> [SimplexVar z r -> SimplexRow z r c]
rowOfConstraint (C1 lo lin up)
 = case range lo up of
    Lower l
     -> [\s ->
        ( M.union (M.map (negate) $ mapOfLinear lin)
                  (M.fromList [(s, 1)])
        , negate l )]
    Upper u
     -> [\s ->
        ( M.union (mapOfLinear lin)
                  (M.fromList [(s, 1)])
        , negate u )]
    Both _ _
     -> rowOfConstraint (C1 lo lin Nothing)
     ++ rowOfConstraint (C1 Nothing lin up)
    Unconstrained
     -> []


data RangeResult c
    = Lower (R c)
    | Upper (R c)
    | Both  (R c) (R c)
    | Unconstrained

range :: Rep c => Maybe (R c) -> Maybe (R c) -> RangeResult c
range lo up
 = case (lo,up) of
        -- TODO:
        --  here we should note that any values we get will have to be shifted back?
        (Just l, Just u)   -> Both l u
        (Nothing, Just u)  -> Upper u
        (Just l, Nothing)  -> Lower  l
        -- Unconstrained
        (Nothing, Nothing) -> Unconstrained

lookupRow :: (Ord z, Ord r, Rep c)
    => SimplexRow z r c
    -> SimplexVar z r
    -> R c
lookupRow (r,_) v
 = case M.lookup v r of
    Nothing -> 0
    Just vv -> vv

objOfRow
    :: SimplexRow z r c
    -> R c
objOfRow = snd

-- If you have 
-- 10 <= 2x + 3y <= 30
-- does this become
--
-- -2x + -3y + s = 20
-- 2x + 3y + t = 30
--
-- ?
--

data IterateResult z r c
    = Done
    | Progress (SimplexMap z r c)
    | Stuck

deriving instance (Show z, Show r, Show (R c)) => Show (IterateResult z r c)


-- | Perform a single iteration
simplex1 :: (Ord z, Ord r, Rep c)
        => SimplexMap z r c -> IterateResult z r c
simplex1 s
 = case pivotCol of
   Nothing
    -> Done
   Just pc
    -> case pivotRow pc of
       Nothing -> Stuck
       Just ((pr,r),_)
        -> let norm = normaliseRow pc r
               rest = filter ((/=pr) . fst) (_rows s)
           in Progress
            $ SimplexMap
            { _rows = (pc, norm) : map (id *** fixup pc norm) rest
            , _obj  = fixup pc norm (_obj s) }

 where
  pivotCol
   | ls         <- M.toList $ fst $ _obj s
   , Just (k,v) <- minBy' (compare `on` snd) ls
   , v < 0
   = Just k
   | otherwise
   = Nothing

  pivotRow col
   = minBy' (compare `on` snd)
   $ filter ((>0) . snd)
   $ map (\(n,r)
             -> let rv = lookupRow r col
                    o  = objOfRow  r
                in if    rv == 0
                   then ((n,r), 0)
                   else ((n,r), o / rv))
   $ _rows s

  normaliseRow pc row@(rm, ro)
   = let c' = lookupRow row pc
     in  (M.map (/c') rm, ro / c')

  fixup pc _norm@(nm,no) row@(rm,ro)
   = let co = lookupRow row pc
     in  {- row' = row - co*norm -}
         ( M.unionWith (+) rm (M.map ((-co)*) nm)
         , ro - co * no )

  minBy' _ []
   = Nothing
  minBy' f ls
   = Just $ minimumBy f ls

-- Simplex
simplex :: (Ord z, Ord r, Rep c)
        => SimplexMap z r c -> Maybe (SimplexMap z r c)
simplex s
 = case simplex1 s of
    Done        -> Just     s
    Progress s' -> simplex  s'
    Stuck       -> Nothing


extract :: (Rep c)
        => SimplexMap z r c
        -> ([(Either z r, R c)], R c)
extract s
 = ( concatMap go (_rows s)
   , objOfRow     (_obj  s))
 where
  go (SVZ z, row)
   = [(Left  z, objOfRow row)]
  go (SVR r, row)
   = [(Right r, objOfRow row)]
  go _
   = []

assignment
        :: (Ord z, Ord r, Rep c)
        => SimplexMap z r c
        -> (Assignment () (Either z r) c, R c)
assignment s
 = let (vs, o) = extract s
   in  ( Assignment M.empty $ M.fromList vs
       , o )

