module HlintSpec where

import Test.Hspec
import Language.Haskell.HLint3

spec :: Spec
spec = do
  ws <- runIO $ hlint ["."]
  it "No hlint warnings" $ ws `shouldBe` []
