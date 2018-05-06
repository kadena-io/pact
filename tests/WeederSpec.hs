module WeederSpec where

import Test.Hspec
import Weeder

spec :: Spec
spec = do
  ws <- runIO $ weeder ["."]
  it "No weeder warnings" $ ws `shouldBe` 0
