{-# LANGUAGE CPP #-}
module Numeric.Limp.Program.Constraint where
import Numeric.Limp.Program.Linear
import Numeric.Limp.Program.ResultKind
import Numeric.Limp.Rep

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Monoid

-- | Different kind of constraints.
--
-- These are not all necessary, but I have a hunch that keeping some structure may be helpful in the future.
--
-- Constructors:
--
--   [@:==@]    Equality constraint
--
--   [@:<=@]    Less than or equal
--
--   [@:<@]     Strictly less than: this is only allowed for purely integer functions
--
--   [@:>=@]    Greater than or equal
--
--   [@:>@]     Strictly greater than: this is only allowed for purely integer functions
--
--   [@Between@] @Between a b c@ is equivalent to @a :<= b :&& b :<= c@
--
--   [@:&&@]    Conjunction of two constraints
--
--   [@:!@]     @"name" :! constr@ Annotate a constraint with a name, or other useless information
--
--   [@CTrue@]  Trivially true constraint
--

data Constraint z r c where
 (:==)   :: Linear z r c k1  -> Linear z r c k2  -> Constraint z r c
 (:<=)   :: Linear z r c k1  -> Linear z r c k2  -> Constraint z r c
 (:<)    :: Linear z r c KZ  -> Linear z r c KZ  -> Constraint z r c
 (:>=)   :: Linear z r c k1  -> Linear z r c k2  -> Constraint z r c
 (:>)    :: Linear z r c KZ  -> Linear z r c KZ  -> Constraint z r c
 Between :: Linear z r c k1  -> Linear z r c k2  -> Linear z r c k3   -> Constraint z r c
 (:&&)   :: Constraint z r c -> Constraint z r c -> Constraint z r c
 (:!)    :: String           -> Constraint z r c -> Constraint z r c
 CTrue   ::                                         Constraint z r c

deriving instance (Show z, Show r, Show (Z c), Show (R c)) => (Show (Constraint z r c))

infix  5 :==
infix  5 :<=
infix  5 :<
infix  5 :>=
infix  5 :>
infix  4 :!
infixr 3 :&&

#if MIN_VERSION_base(4,9,0)
instance Semigroup (Constraint z r c) where
 (<>) = (:&&)
#endif


instance Monoid (Constraint z r c) where
 mempty  = CTrue
 mappend = (:&&)
