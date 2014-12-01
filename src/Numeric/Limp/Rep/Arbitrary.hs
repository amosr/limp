-- | Arbitrary precision number representation
module Numeric.Limp.Rep.Arbitrary where
import Numeric.Limp.Rep.Rep

-- | A representation that uses arbitrary-sized Integers and Rationals
data Arbitrary

instance Rep Arbitrary where
 -- | Automatically defer numeric operations to the native int.
 newtype Z Arbitrary = Z Integer
    deriving (Ord,Eq,Integral,Real,Num,Enum)
 newtype R Arbitrary = R Rational
    deriving (Ord,Eq,Num,Enum,Fractional,Real,RealFrac)

-- | Define show manually, so we can strip out the "Z" and "R" prefixes.
instance Show (Z Arbitrary) where
 show (Z i) = show i

instance Show (R Arbitrary) where
 show (R i) = show i


