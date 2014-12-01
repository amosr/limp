-- | The simplest, stupidest possible branch and bound algorithm.
--
--
module Numeric.Limp.Solve.Branch.Simple
    (branch, makeIntegral)
    where
import Numeric.Limp.Canon.Program
import Numeric.Limp.Rep

import Control.Applicative
import Control.Monad
import qualified Data.Map as M

branch
    :: (Ord z, Ord r, Rep c)
    => (Program z r c -> Maybe (Assignment () (Either z r) c, R c))
    -> Program z r c
    -> Maybe (Assignment z r c, R c)
branch solver p
 = do  (ass,co) <- solver p
       case makeIntegral ass of
        Left (var, val)
         -> branchon (Left var) val
        Right r
         -> Just (r, co)
 where
  branchon var val
   = let lo = addBound var (Just (fromZ $ truncate val + 1), Nothing)
         up = addBound var (Nothing, Just (fromZ $ truncate val))
         loB     = branch solver lo
         upB     = branch solver up
     in case (loB, upB) of
        (Just (a1, o1), Just (a2, o2))
         | o1 < o2
         -> Just (a1, o1)
         | otherwise
         -> Just (a2, o2)
        (Just r, Nothing)
         -> Just r
        (Nothing, Just r)
         -> Just r
        (Nothing, Nothing)
         -> Nothing
     

  addBound v b
   = let bs = _bounds p
         b' = maybe (Nothing,Nothing) id
            $ M.lookup v bs
     in  p { _bounds = M.insert v (mergeBounds b b') bs }

makeIntegral
    :: (Ord z, Ord r, Rep c)
    => Assignment () (Either z r) c
    -> Either (z, R c)
              (Assignment z r c)
makeIntegral (Assignment _ vs)
 =   uncurry Assignment
 <$> foldM go (M.empty, M.empty) (M.toList vs)
 where
  go (zs,rs) (var, val)
   = case var of
      Right r
       -> return (zs, M.insert r val rs)
      Left z
       | val' <- truncate val
       , val == fromZ val'
       -> return (M.insert z val' zs, rs)
       | otherwise
       -> Left (z, val)

