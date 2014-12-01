-- | Fixed/floating precision number representation
module Numeric.Limp.Rep.IntDouble where
import Numeric.Limp.Rep.Rep

-- | A representation that uses native 64-bit ints and 64-bit doubles.
-- Really, this should be 32-bit ints.
data IntDouble

instance Rep IntDouble where
 -- | Automatically defer numeric operations to the native int.
 newtype Z IntDouble = Z Int
    deriving (Ord,Eq,Integral,Real,Num,Enum)
 newtype R IntDouble = R Double
    deriving (Ord,Eq,Num,Enum,Fractional,Real,RealFrac)

-- | Define show manually, so we can strip out the "Z" and "R" prefixes.
instance Show (Z IntDouble) where
 show (Z i) = show i

instance Show (R IntDouble) where
 show (R i) = show i



-- | Convert a wrapped (R IntDouble) to an actual Double.
unwrapR :: R IntDouble -> Double
unwrapR (R d) = d


