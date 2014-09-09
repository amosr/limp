
import Test.Tasty

import qualified Convert
import qualified Simplify

main = defaultMain properties

properties :: TestTree
properties
 = testGroup "Properties"
 [ Convert.tests
 , Simplify.tests
 ]

