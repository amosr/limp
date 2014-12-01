-- | The simplest, stupidest possible simplex algorithm.
-- The idea here is to be slow, but "obviously correct" so other algorithms
-- can be verified against it.
--
-- That's the plan, at least. For now this is just a first cut of trying to implement simplex.
--
module Numeric.Limp.Solve.Simplex.Maps
    where
import Numeric.Limp.Rep

import Numeric.Limp.Solve.Simplex.StandardForm

import Control.Arrow
import qualified Data.Map as M
import Data.Function (on)
import Data.List (minimumBy, sortBy)


-- | Result of a single pivot attempt
data IterateResult z r c
    -- | Maximum reached!
    = Done
    -- | Pivot was made
    | Progress (Standard z r c)
    -- | No progress can be made: unbounded along the objective
    | Stuck

deriving instance (Show z, Show r, Show (R c)) => Show (IterateResult z r c)


-- | Try to find a pivot and then perform it.
-- We're assuming, at this stage, that the existing solution is feasible.
simplex1 :: (Ord z, Ord r, Rep c)
        => Standard z r c -> IterateResult z r c
simplex1 s
 -- Check if there are any positive columns in the objective:
 = case pivotCols of
    -- if there are none, we are already at the maximum
    []
     -> Done
    -- there are some; try to find the first pivot row that works
    _
     -> go pivotCols
 where

  -- Check if there's any row worth pivoting on for this column.
  -- We're trying to see if we can increase the value of this
  -- column's variable from zero.
  go ((pc,_):pcs)
   = case pivotRowForCol s pc of
       Nothing -> go pcs
       Just pr
        -> Progress
        -- Perform the pivot.
        -- This moves the variable pr out of the basis, and pc into the basis.
         $ pivot s (pr,pc)

  -- We've tried all the pivot columns and failed.
  -- This means there's no edge we can take to increase our objective,
  -- so it must be unbounded.
  go []
   = Stuck


  -- We want to find some positive column from the objective.
  -- In fact, find all of them and order descending.
  pivotCols
   = let ls  = M.toList $ fst $ _objective s
         kvs = sortBy (compare `on` (negate . snd)) ls
     in  filter ((>0) . snd) kvs


-- | Find pivot row for given column.
-- We're trying to find a way to increase the value of
-- column from zero, and the returned row will be decreased to zero.
-- Since all variables are >= 0, we cannot return a row that would set the column to negative.
pivotRowForCol :: (Ord z, Ord r, Rep c)
        => Standard z r c
        -> StandardVar z r
        -> Maybe (StandardVar z r)
pivotRowForCol s col
 = fmap   fst
 $ minBy' (compare `on` snd)
 $ concatMap (\(n,r)
           -> let rv = lookupRow r col
                  o  = objOfRow  r
              in if    rv > 0
                 then [(n, o / rv)]
                 else [])
 $ M.toList
 $ _constraints s

-- | Find minimum, or nothing if empty
minBy' :: (a -> a -> Ordering) -> [a] -> Maybe a
minBy' _ []
 = Nothing
minBy' f ls
 = Just $ minimumBy f ls


-- | Perform pivot for given row and column.
-- We normalise row so that row.column = 1
--
-- > norm = row / row[column]
--
-- Then, for all other rows including the objective,
-- we want to make sure its column entry is zero:
--
-- > row' = row - row[column]*norm
--
-- In the end, this means "column" will be an identity column, or a basis column.
--
pivot   :: (Ord z, Ord r, Rep c)
        => Standard z r c
        -> (StandardVar z r, StandardVar z r)
        -> Standard z r c
pivot s (pr,pc)
 = let norm = normaliseRow
       -- All other rows
       rest = filter ((/=pr) . fst) $ M.toList $ _constraints s
   in Standard
    { _constraints = M.fromList ((pc, norm) : map (id *** fixup norm) rest)
    , _objective   = fixup norm $ _objective s
    , _substs      = _substs s }
 where
  -- norm = row / row[column]
  normaliseRow
   | Just row@(rm, ro) <- M.lookup pr $ _constraints s
   = let c' = lookupRow row pc
     in  (M.map (/c') rm, ro / c')

   -- Pivot would not be chosen if row doesn't exist..
   | otherwise
   = (M.empty, 0)

  -- row' = row - row[column]*norm
  fixup (nm,no) row@(rm,ro)
   = let co = lookupRow row pc
     in  {- row' = row - co*norm -}
         ( M.unionWith (+) rm (M.map ((-co)*) nm)
         , ro - co * no )


-- | Single phase of simplex.
-- Keep repeating until no progress can be made.
single_simplex :: (Ord z, Ord r, Rep c)
        => Standard z r c -> Maybe (Standard z r c)
single_simplex s
 = case simplex1 s of
    Done        -> Just     s
    Progress s' -> single_simplex  s'
    Stuck       -> Nothing


-- | Two phase:
--  first, find a satisfying solution.
--  then, solve simplex as normal.
simplex
        :: (Ord z, Ord r, Rep c)
        => Standard z r c -> Maybe (Standard z r c)
simplex s
 =   find_initial_sat s
 >>= single_simplex

-- | Find a satisfying solution.
--   if there are any rows with negative values, this means their basic values are negative
--   (which is not satisfying the x >= 0 constraint)
--   these negative-valued rows must be pivoted around using modified pivot criteria
find_initial_sat
        :: (Ord z, Ord r, Rep c)
        => Standard z r c -> Maybe (Standard z r c)
find_initial_sat s
 = case negative_val_rows of
    []      -> Just s
    rs      -> go rs
 where
  -- Find all rows with negative values
  -- because their current value is not feasible
  negative_val_rows
   = filter ((<0) . objOfRow . snd)
   $ M.toList
   $ _constraints s

  -- Find largest negative (closest to zero) to pivot on:
  -- pivoting on a negative will negate the value, setting it to positive
  min_of_row (_,(rm,_))
   = minBy' (compare `on` (negate . snd))
   $ filter ((<0) . snd)
   $ M.toList rm


  -- There is no feasible solution
  go []
   = Nothing

  -- Try pivoting on the rows 
  go (r:rs)
   | Just (pc,_) <- min_of_row r
   , Just  pr    <- pivotRowForNegatives pc
   = simplex
   $ pivot s (pr, pc)

   | otherwise
   = go rs

  -- opposite of pivotRowForCol...
  pivotRowForNegatives col
   = fmap   fst
   $ minBy' (compare `on` (negate . snd))
   $ concatMap (\(n,r)
             -> let rv = lookupRow r col
                    o  = objOfRow  r
                in if    rv < 0
                   then [(n, o / rv)]
                   else [])
   $ M.toList
   $ _constraints s


  

-- Get map of each constraint's value
assignmentAll :: (Rep c)
        => Standard z r c
        -> (M.Map (StandardVar z r) (R c), R c)
assignmentAll s
 = ( M.map    val (_constraints s)
   , objOfRow     (_objective  s))
 where
  val (_, v)
   = v

-- Perform reverse substitution on constraint values
-- to get original values (see StandardForm)
assignment
        :: (Ord z, Ord r, Rep c)
        => Standard z r c
        -> (Assignment () (Either z r) c, R c)
assignment s
 = ( Assignment M.empty $ M.union vs' rs'
   , o )
 where
  (vs, o) = assignmentAll s

  vs'     = M.fromList
          $ concatMap only_svs
          $ M.toList vs

  rs'     = M.map eval $ _substs s

  eval (lin,co)
          = M.fold (+) co
          $ M.mapWithKey (\k r -> r * (maybe 0 id $ M.lookup k vs))
          $ lin

  only_svs (SV v, val)
   = [(v, val)]
  only_svs _
   = []



-- Junk ---------------

-- | Minimise whatever variables are 'basic' in given standard
-- input must not already have an objective row "SVO",
-- because the existing objective is added as a new row with that name
minimise_basics
        :: (Ord z, Ord r, Rep c)
        => Standard z r c -> Standard z r c
minimise_basics s
 = s
 { _objective   = (M.map (const (1)) $ _constraints s, 0)
 , _constraints = M.insert SVO (_objective s) (_constraints s)
 }

-- | Find the basic variables and "price them out" of the objective function,
-- by subtracting multiples of the basic row from objective
pricing_out 
        :: (Ord z, Ord r, Rep c)
        => Standard z r c -> Standard z r c
pricing_out s
 = s
 { _objective = M.foldWithKey  go
                    (_objective   s)
                    (_constraints s)
 }
 where
  go v row@(rm,ro) obj@(om,oo)
   | coeff <- lookupRow obj v
   , coeff /= 0
   , rowv  <- lookupRow row v
   , mul   <- -(coeff / rowv)
   = -- rowv = 1
     -- obj' = obj - (coeff/rowv)*row
     ( M.unionWith (+) om (M.map (mul*) rm)
     , oo + mul*ro )
   | otherwise
   = obj

-- | Pull the previously-hidden objective out of constraints, and use it
drop_fake_objective
        :: (Ord z, Ord r, Rep c)
        => Standard z r c -> Standard z r c
drop_fake_objective s
 | cs     <- _constraints s
 , Just o <- M.lookup SVO cs
 = s
 { _objective   = o
 , _constraints = M.delete SVO cs }

 | otherwise
 = s



