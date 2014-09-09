-- | Define upper and lower bounds of program variables.
module Numeric.Limp.Program.Bounds where
import Numeric.Limp.Rep

-- | Define upper and lower bounds of program variables.
-- Bounds may be specified multiple times: the intersection of all bounds is used.
data Bounds z r c
 = BoundZ (B (Z c) z)
 | BoundR (B (R c) r)

deriving instance (Show z, Show r, Show (Z c), Show (R c)) => (Show (Bounds z r c))

-- | Maybe a lower bound, the variable's name, and maybe an upper bound.
type B rep v
 = (Maybe rep, v, Maybe rep)

-- | Create a lower and upper bound for an integer variable.
lowerUpperZ :: Rep c => Z c -> z -> Z c -> Bounds z r c
lowerUpperZ l v u
 = BoundZ (Just l, v, Just u)

-- | Create only a lower bound for an integer variable. 
lowerZ :: Rep c => Z c -> z -> Bounds z r c
lowerZ l v
 = BoundZ (Just l, v, Nothing)

-- | Create only an upper bound for an integer variable. 
upperZ :: Rep c => z -> Z c -> Bounds z r c
upperZ v u
 = BoundZ (Nothing, v, Just u)

-- | A binary integer variable: can only be @0@ or @1@.
binary :: Rep c => z -> Bounds z r c
binary v
 = BoundZ (Just 0, v, Just 1)

-- | Create a lower and upper bound for a real variable.
lowerUpperR :: Rep c => R c -> r -> R c -> Bounds z r c
lowerUpperR l v u
 = BoundR (Just l, v, Just u)

-- | Create only a lower bound for a real variable. 
lowerR :: Rep c => R c -> r -> Bounds z r c
lowerR l v
 = BoundR (Just l, v, Nothing)

-- | Create only an upper bound for a real variable. 
upperR :: Rep c => r -> R c -> Bounds z r c
upperR v u
 = BoundR (Nothing, v, Just u)


