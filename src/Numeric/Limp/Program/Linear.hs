module Numeric.Limp.Program.Linear where
import Numeric.Limp.Rep

-- import Control.Lens

-- | The kind of a linear function:
-- it can be integral (Z) or real (R).
data K = KZ | KR

data Linear z r c k where
 LZ :: [(z, Z c)]          -> (Z c) -> Linear z r c KZ
 LR :: [(Either z r, R c)] -> (R c) -> Linear z r c KR

-- | The upper bound of two kinds is real, unless both are integral
type family KMerge (a :: K) (b :: K) :: K where
 KMerge KZ KZ = KZ
 KMerge KR b  = KR
 KMerge a  KR = KR

-- | The upper bound of two kinds is real, unless both are integral
type family KRep (a :: K) :: * -> * where
 KRep KZ = Z
 KRep KR = R

-- | Any integral linear function can be made into a real linear function
krOfKz :: Rep c => Linear z r c KZ -> Linear z r c KR
krOfKz (LZ ls co)
 = LR (map go ls) (fromZ co)
 where
  go (z',c') = (Left z', fromZ c')

-- | Any linear function can be made into a real, as it is the upper bound / top
toR :: Rep c => Linear z r c k -> Linear z r c KR
toR l@(LZ{}) = krOfKz l
toR l@(LR{}) =        l


------------------------
-- Creation functions

-- | Integral variable
z :: Rep c => z -> Z c -> Linear z r c KZ
z z' c
 = LZ [(z', c)] 0

-- | Integral variable with coefficient 1
z1 :: Rep c => z -> Linear z r c KZ
z1 z'
 = z z' 1

-- | Real variable
r :: Rep c => r -> R c -> Linear z r c KR
r r' c
 = LR [(Right r', c)] 0

-- | Real variable with coefficient 1
r1 :: Rep c => r -> Linear z r c KR
r1 r'
 = r r' 1


-- | An integral constant
con :: Rep c => Z c -> Linear z r c KZ
con c'
 = LZ [] c'

c0 :: Rep c => Linear z r c KZ
c0 = con 0
c1 :: Rep c => Linear z r c KZ
c1 = con 1

on2 :: (b -> c) -> (a, b) -> (a, c)
on2 f (a,b) = (a, f b)

-- | Negate a linear function.
-- Negation does not change the kind.
neg :: Rep c => Linear z r c k -> Linear z r c k
neg (LZ ls c)
 = LZ (map (on2 negate) ls) (negate c)
neg (LR ls c)
 = LR (map (on2 negate) ls) (negate c)


(.*) :: Rep c => Linear z r c k -> KRep k c -> Linear z r c k
(.*) (LZ ls c) z'
 = LZ (map (on2 (*z')) ls) (c * z')
(.*) (LR ls c) r'
 = LR (map (on2 (*r')) ls) (c * r')

(*.) :: Rep c => KRep k c -> Linear z r c k -> Linear z r c k
(*.) = flip (.*)

add_KZ :: Rep c => Linear z r c KZ -> Linear z r c KZ -> Linear z r c KZ
add_KZ (LZ ls lc) (LZ rs rc) = LZ (ls ++ rs) (lc + rc)

add_KR :: Rep c => Linear z r c KR -> Linear z r c KR -> Linear z r c KR
add_KR (LR ls lc) (LR rs rc) = LR (ls ++ rs) (lc + rc)


(.+.) :: Rep c => Linear z r c k1 -> Linear z r c k2 -> Linear z r c (KMerge k1 k2)
(.+.) a b
 = case (a,b) of
    (LZ{}, LZ{}) -> add_KZ      a      b
    (LR{}, LZ{}) -> add_KR      a (toR b)
    (LZ{}, LR{}) -> add_KR (toR a)     b
    (LR{}, LR{}) -> add_KR      a      b


(.-.) :: Rep c => Linear z r c k1 -> Linear z r c k2 -> Linear z r c (KMerge k1 k2)
(.-.) a b
 = a .+. neg b


infix  7 *.
infix  7 .*
infixl 6 .+.
infixl 6 .-.

eval :: Rep c => Assignment z r c -> Linear z r c k -> KRep k c
eval a (LZ ls c)
 = sum (map get ls) + c
 where
  get (l, co) = zOf a l * co

eval a (LR ls c)
 = sum (map get ls) + c
 where
  get (l, co) = zrOf a l * co

evalR :: Rep c => Assignment z r c -> Linear z r c k -> R c
evalR a l@(LZ{}) = fromZ (eval a l)
evalR a l@(LR{}) =        eval a l
