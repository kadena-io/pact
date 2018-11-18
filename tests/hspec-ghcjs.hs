import Test.Hspec

import qualified Blake2Spec
import qualified KeysetSpec
import qualified TypesSpec

main :: IO ()
main = hspec $ do
  describe "Blake2Spec" Blake2Spec.spec
  describe "KeysetSpec" KeysetSpec.spec
  describe "TypesSpec" TypesSpec.spec
