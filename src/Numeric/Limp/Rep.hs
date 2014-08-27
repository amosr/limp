module Numeric.Limp.Rep where

import Data.Map (Map)
import qualified Data.Map as M

class ( Num (Z c), Ord (Z c), Eq (Z c), Integral (Z c)
      , Num (R c), Ord (R c), Eq (R c)) => Rep c where
 data Z c
 data R c

 fromZ :: Z c -> R c
 fromZ = fromIntegral

data Assignment z r c
 = Assignment (Map z (Z c)) (Map r (R c))

zOf :: (Rep c, Ord z) => Assignment z r c -> z -> Z c
zOf (Assignment zs _) z
 = zs M.! z

rOf :: (Rep c, Ord r) => Assignment z r c -> r -> R c
rOf (Assignment _ rs) r
 = rs M.! r

zrOf :: (Rep c, Ord z, Ord r) => Assignment z r c -> Either z r -> R c
zrOf a = either (fromZ . zOf a) (rOf a)

data IntDouble

instance Rep IntDouble where
 newtype Z IntDouble = Z Int
    deriving (Ord,Eq,Show,Read,Integral,Real,Num,Enum)
 newtype R IntDouble = R Double
    deriving (Ord,Eq,Show,Read,Num,Enum)

unwrapR :: R IntDouble -> Double
unwrapR (R d) = d

