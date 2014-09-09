module Arbitrary.Var where
import Test.QuickCheck

data ZVar = ZVar String
 deriving (Eq,Ord)

instance Show ZVar where
 show (ZVar z) = "z$" ++ z

instance Arbitrary ZVar where
 arbitrary
        -- 26 variables should be enough for anyone!
  = do  c <- elements ['a'..'z']
        return $ ZVar [c]


data RVar = RVar String
 deriving (Eq,Ord)

instance Show RVar where
 show (RVar r) = "r$" ++ r

instance Arbitrary RVar where
 arbitrary
  = do  c <- elements ['a'..'z']
        return $ RVar [c]


data Vars = Vars [ZVar] [RVar]
 deriving Show

instance Arbitrary Vars where
 arbitrary
  = do  NonEmpty zs   <- arbitrary :: Gen (NonEmptyList ZVar)
        NonEmpty rs   <- arbitrary :: Gen (NonEmptyList RVar)
        return $ Vars zs rs


