-- | Representation, constructors and limited arithmetic on linear functions.
--
-- The linear function is indexed by its result type: either purely integer (@KZ@) or mixed/real (@KR@).
-- This index is used to allow strictly-less-than constraints only on integer functions,
-- and to allow retrieving integer values from purely integer functions.
--
module Numeric.Limp.Program.Linear
    ( Linear(..)
    , toR

    , z, z1
    , r, r1

    , con, conZ, conR
    , c0, c1

    , neg
    , (.*), (*.)
    , (.+.), (.-.) )
     where
import Numeric.Limp.Rep
import Numeric.Limp.Program.ResultKind

-- | Any linear function can be converted into a real linear function.
toR :: Rep c => Linear z r c k -> Linear z r c 'KR
toR (LZ ls co) = LR (map go ls) (fromZ co)
 where
  go (z',c') = (Left z', fromZ c')
toR l@(LR{}) =        l


-- | Integral variable
z :: Rep c => z -> Z c -> Linear z r c 'KZ
z z' c
 = LZ [(z', c)] 0

-- | Integral variable with coefficient 1
z1 :: Rep c => z -> Linear z r c 'KZ
z1 z'
 = z z' 1

-- | Real variable
r :: Rep c => r -> R c -> Linear z r c 'KR
r r' c
 = LR [(Right r', c)] 0

-- | Real variable with coefficient 1
r1 :: Rep c => r -> Linear z r c 'KR
r1 r'
 = r r' 1


-- | An integral constant summand
con :: Rep c => Z c -> Linear z r c 'KZ
con c'
 = LZ [] c'

-- | An integral constant summand
conZ :: Rep c => Z c -> Linear z r c 'KZ
conZ = con

-- | Constant @0@
c0 :: Rep c => Linear z r c 'KZ
c0 = con 0
-- | Constant @1@
c1 :: Rep c => Linear z r c 'KZ
c1 = con 1

-- | A real constant
conR :: Rep c => R c -> Linear z r c 'KR
conR c'
 = LR [] c'

-- | Helper for applying function to second element of tuple
on2 :: (b -> c) -> (a, b) -> (a, c)
on2 f (a,b) = (a, f b)

-- | Negate a linear function.
-- Negation does not change the kind.
neg :: Rep c => Linear z r c k -> Linear z r c k
neg (LZ ls c)
 = LZ (map (on2 negate) ls) (negate c)
neg (LR ls c)
 = LR (map (on2 negate) ls) (negate c)


-- | Multiply a linear function by some constant.
--
-- Note that you cannot multiply a linear function by another linear function, as the result would likely be non-linear!
(.*) :: Rep c => Linear z r c k -> KRep k c -> Linear z r c k
(.*) (LZ ls c) z'
 = LZ (map (on2 (*z')) ls) (c * z')
(.*) (LR ls c) r'
 = LR (map (on2 (*r')) ls) (c * r')

-- | Multiply a linear function by some constant.
(*.) :: Rep c => KRep k c -> Linear z r c k -> Linear z r c k
(*.) = flip (.*)


-- | Add two linear functions together. They can have different result types.
(.+.) :: Rep c => Linear z r c k1 -> Linear z r c k2 -> Linear z r c (KMerge k1 k2)
(.+.) a b
 = case (a,b) of
    (LZ{}, LZ{}) -> add_KZ      a      b
    (LR{}, LZ{}) -> add_KR      a (toR b)
    (LZ{}, LR{}) -> add_KR (toR a)     b
    (LR{}, LR{}) -> add_KR      a      b
 where
  add_KZ :: Rep c => Linear z r c 'KZ -> Linear z r c 'KZ -> Linear z r c 'KZ
  add_KZ (LZ ls lc) (LZ rs rc) = LZ (ls ++ rs) (lc + rc)

  add_KR :: Rep c => Linear z r c 'KR -> Linear z r c 'KR -> Linear z r c 'KR
  add_KR (LR ls lc) (LR rs rc) = LR (ls ++ rs) (lc + rc)



-- | Subtract one linear function from another. They can have different result types.
(.-.) :: Rep c => Linear z r c k1 -> Linear z r c k2 -> Linear z r c (KMerge k1 k2)
(.-.) a b
 = a .+. neg b


infix  7 *.
infix  7 .*
infixl 6 .+.
infixl 6 .-.

