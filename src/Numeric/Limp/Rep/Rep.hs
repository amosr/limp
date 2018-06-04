{-# LANGUAGE CPP #-}
-- | Representation of integers (Z) and reals (R) of similar precision.
-- Programs are abstracted over this, so that ideally in the future we could have a
-- solver that produces Integers and Rationals, instead of just Ints and Doubles.
--
-- We bundle Z and R up into a single representation instead of abstracting over both,
-- because we must be able to convert from Z to R without loss.
--
module Numeric.Limp.Rep.Rep where

import Data.Map (Map)
import qualified Data.Map as M

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif

-- | The Representation class. Requires its members @Z c@ and @R c@ to be @Num@, @Ord@ and @Eq@.
--
-- For some reason, for type inference to work, the members must be @data@ instead of @type@.
-- This gives some minor annoyances when unpacking them. See 'unwrapR' below.
--
class ( Num (Z c), Ord (Z c), Eq (Z c), Integral (Z c)
      , Num (R c), Ord (R c), Eq (R c), RealFrac (R c)) => Rep c where

 -- | Integers
 data Z c
 -- | Real numbers
 data R c

 -- | Convert an integer to a real. This should not lose any precision.
 -- (whereas @fromIntegral 1000 :: Word8@ would lose precision)
 fromZ :: Z c -> R c
 fromZ = fromIntegral


-- | An assignment from variables to values.
-- Maps integer variables to integers, and real variables to reals.
data Assignment z r c
 = Assignment (Map z (Z c)) (Map r (R c))

deriving instance (Show (Z c), Show (R c), Show z, Show r) => Show (Assignment z r c)

instance (Ord z, Ord r) => Semigroup (Assignment z r c) where
 (<>) = mappend

instance (Ord z, Ord r) => Monoid (Assignment z r c) where
 mempty = Assignment M.empty M.empty
 mappend (Assignment z1 r1) (Assignment z2 r2)
  = Assignment (M.union z1 z2) (M.union r1 r2)


-- | Retrieve value of integer variable - or 0, if there is no value.
zOf :: (Rep c, Ord z) => Assignment z r c -> z -> Z c
zOf (Assignment zs _) z
 = maybe 0 id $ M.lookup z zs

-- | Retrieve value of real variable - or 0, if there is no value.
rOf :: (Rep c, Ord r) => Assignment z r c -> r -> R c
rOf (Assignment _ rs) r
 = maybe 0 id $ M.lookup r rs

-- | Retrieve value of an integer or real variable, with result cast to a real regardless.
zrOf :: (Rep c, Ord z, Ord r) => Assignment z r c -> Either z r -> R c
zrOf a = either (fromZ . zOf a) (rOf a)

assSize :: Assignment z r c -> Int
assSize (Assignment mz mr)
 = M.size mz + M.size mr
