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


lookupRow :: (Ord z, Ord r, Rep c)
    => StandardRow z r c
    -> StandardVar z r
    -> R c
lookupRow (r,_) v
 = case M.lookup v r of
    Nothing -> 0
    Just vv -> vv

objOfRow
    :: StandardRow z r c
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

--
-- A + B = 10
-- ==> ?
-- A + B + S = 10
-- ????
--
-- A + B >= 10
-- A + B <= 10
--
-- A + B + S = 10
-- A + B - T = 10
--
-- -A -B + T = -10

data IterateResult z r c
    = Done
    | Progress (Standard z r c)
    | Stuck

deriving instance (Show z, Show r, Show (R c)) => Show (IterateResult z r c)


-- | Perform a single iteration
simplex1 :: (Ord z, Ord r, Rep c)
        => Standard z r c -> IterateResult z r c
simplex1 s
 = case pivotCols of
    []
     -> Done
    _
     -> go pivotCols
 where
  go []
   = Stuck
  go ((pc,_):pcs)
   = case pivotRowForCol s pc of
       Nothing -> go pcs
       Just pr
        -> Progress
         $ pivot s (pr,pc)

  pivotCols
   = let ls  = M.toList $ fst $ _objective s
         kvs = sortBy (compare `on` (negate . snd)) ls
     in  filter ((>0) . snd) kvs

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

minBy' :: (a -> a -> Ordering) -> [a] -> Maybe a
minBy' _ []
 = Nothing
minBy' f ls
 = Just $ minimumBy f ls


pivot   :: (Ord z, Ord r, Rep c)
        => Standard z r c
        -> (StandardVar z r, StandardVar z r)
        -> Standard z r c
pivot s (pr,pc)
 = let norm = normaliseRow
       rest = filter ((/=pr) . fst) $ M.toList $ _constraints s
   in Standard
    { _constraints = M.fromList ((pc, norm) : map (id *** fixup norm) rest)
    , _objective   = fixup norm $ _objective s
    , _substs      = _substs s }
 where
  normaliseRow
   | Just row@(rm, ro) <- M.lookup pr $ _constraints s
   = let c' = lookupRow row pc
     in  (M.map (/c') rm, ro / c')
   | otherwise
   = (M.empty, 0)

  fixup _norm@(nm,no) row@(rm,ro)
   = let co = lookupRow row pc
     in  {- row' = row - co*norm -}
         ( M.unionWith (+) rm (M.map ((-co)*) nm)
         , ro - co * no )


-- Single phase of simplex
single_simplex :: (Ord z, Ord r, Rep c)
        => Standard z r c -> Maybe (Standard z r c)
single_simplex s
 = case simplex1 s of
    Done        -> Just     s
    Progress s' -> single_simplex  s'
    Stuck       -> Nothing


-- Two phase
simplex
        :: (Ord z, Ord r, Rep c)
        => Standard z r c -> Maybe (Standard z r c)
simplex s
 = case negative_val_rows of
    []      -> single_simplex s
    rs      -> go rs
 where
  negative_val_rows
   = filter ((<0) . objOfRow . snd)
   $ M.toList
   $ _constraints s

  min_of_row (_,(rm,_))
   = minBy' (compare `on` snd)
   $ filter ((<0) . snd)
   $ M.toList rm


  go []
   = Nothing

  go (r:rs)
   | Just (pc,_) <- min_of_row r
   , Just  pr    <- pivotRowForCol s pc
   = simplex
   $ pivot s (pr, pc)

   | otherwise
   = go rs
  

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



assignmentAll :: (Rep c)
        => Standard z r c
        -> (M.Map (StandardVar z r) (R c), R c)
assignmentAll s
 = ( M.map    val (_constraints s)
   , objOfRow     (_objective  s))
 where
  val (_, v)
   = v

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
