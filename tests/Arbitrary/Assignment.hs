module Arbitrary.Assignment where

import Numeric.Limp.Rep

import Arbitrary.Var

import Test.QuickCheck
import Control.Applicative
import Data.Map (fromList)

type Assignment' = Assignment ZVar RVar IntDouble

instance Arbitrary (Z IntDouble) where
 arbitrary = Z <$> arbitrary

instance Arbitrary (R IntDouble) where
 arbitrary = R <$> (fromIntegral <$> (arbitrary :: Gen Int))


instance Arbitrary (Assignment ZVar RVar IntDouble) where
 arbitrary = arbitrary >>= assignment


assignment :: Vars -> Gen Assignment'
assignment (Vars zs rs)
 = do   zs' <- listOf (elements zs)
        zvs <- infiniteListOf arbitrary

        rs' <- listOf (elements rs)
        rvs <- infiniteListOf arbitrary

        return $ Assignment (fromList $ zs' `zip` zvs) (fromList $ rs' `zip` rvs)

