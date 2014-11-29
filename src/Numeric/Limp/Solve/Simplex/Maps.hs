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
   = case pivotRow pc of
       Nothing -> go pcs
       Just ((pr,r),_)
        -> let norm = normaliseRow pc r
               rest = filter ((/=pr) . fst) $ M.toList $ _constraints s
           in Progress
            $ Standard
            { _constraints = M.fromList ((pc, norm) : map (id *** fixup pc norm) rest)
            , _objective   = fixup pc norm $ _objective s
            , _substs      = _substs s }


  pivotCols
   = let ls  = M.toList $ fst $ _objective s
         kvs = sortBy (compare `on` snd) ls
     in  filter ((<0) . snd) kvs

  pivotRow col
   = minBy' (compare `on` snd)
   $ filter ((>0) . snd)
   $ map (\(n,r)
             -> let rv = lookupRow r col
                    o  = objOfRow  r
                in if    rv == 0
                   then ((n,r), 0)
                   else ((n,r), o / rv))
   $ M.toList
   $ _constraints s

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
        => Standard z r c -> Maybe (Standard z r c)
simplex s
 = case simplex1 s of
    Done        -> Just     s
    Progress s' -> simplex  s'
    Stuck       -> Nothing


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
