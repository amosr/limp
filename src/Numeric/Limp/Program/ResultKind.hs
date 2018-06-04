-- | Type-level functions on result types.
--
-- Linear functions are classified as either int-valued or real-valued,
-- so we define @KZ@ and @KR@ as data kinds to denote this in the type.
--
module Numeric.Limp.Program.ResultKind where
import Numeric.Limp.Rep


-- | Classify the result type of a linear function to either integral or real:
data K
 -- | Integral @Z@
 = KZ
 -- | Real or mixed @R@
 | KR


-- | Representation of either integral of real linear functions:
-- a list of variables with coefficients, plus a constant summand.
data Linear z r c k where
 LZ :: [(z, Z c)]          -> (Z c) -> Linear z r c 'KZ
 LR :: [(Either z r, R c)] -> (R c) -> Linear z r c 'KR

deriving instance (Show z, Show r, Show (Z c), Show (R c)) => (Show (Linear z r c k))


-- | Find the result type of merging, or adding, two linear functions:
-- adding two integers produces an integer, while adding a real on either side produces a real.
type family KMerge (a :: K) (b :: K) :: K where
 KMerge 'KZ 'KZ = 'KZ
 KMerge 'KR  b  = 'KR
 KMerge  a  'KR = 'KR

-- | Convert a @K@ to its actual representation (@Z@ or @R@).
type family KRep (a :: K) :: * -> * where
 KRep 'KZ = Z
 KRep 'KR = R



