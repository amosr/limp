module Numeric.Limp.Program.Bounds where
import Numeric.Limp.Rep

data Bounds z r c
 = BoundZ (B (Z c) z)
 | BoundR (B (R c) r)

type B rep v
 = (Maybe rep, v, Maybe rep)

lowerUpperZ :: Rep c => Z c -> z -> Z c -> Bounds z r c
lowerUpperZ l v u
 = BoundZ (Just l, v, Just u)

lowerZ :: Rep c => Z c -> z -> Bounds z r c
lowerZ l v
 = BoundZ (Just l, v, Nothing)

upperZ :: Rep c => z -> Z c -> Bounds z r c
upperZ v u
 = BoundZ (Nothing, v, Just u)

binary :: Rep c => z -> Bounds z r c
binary v
 = BoundZ (Just 0, v, Just 1)

lowerUpperR :: Rep c => R c -> r -> R c -> Bounds z r c
lowerUpperR l v u
 = BoundR (Just l, v, Just u)

lowerR :: Rep c => R c -> r -> Bounds z r c
lowerR l v
 = BoundR (Just l, v, Nothing)

upperR :: Rep c => r -> R c -> Bounds z r c
upperR v u
 = BoundR (Nothing, v, Just u)


