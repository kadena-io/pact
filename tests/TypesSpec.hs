{-# LANGUAGE OverloadedStrings #-}
module TypesSpec (spec) where


import Test.Hspec

import Data.Aeson
import Data.Map.Strict (fromList)
import qualified Data.Set as S

import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Term
import Data.Default (def)

spec :: Spec
spec = do
  describe "JSONPersistables" testJSONPersist
  describe "JSONColumns "testJSONColumns
-- describe "JSONModules" testJSONModules

rt :: (FromJSON a,ToJSON a,Show a,Eq a) => a -> Spec
rt p = it ("roundtrips " ++ show p) $ decode (encode p) `shouldBe` Just p

testJSONPersist :: Spec
testJSONPersist = do
  rt (PLiteral (LInteger 123))
  rt (PLiteral (LDecimal 123.34857))
  rt (PLiteral (LBool False))
  rt (PLiteral (LString "hello"))
  rt (PLiteral (LTime (read "2016-09-17 22:47:31.904733 UTC")))
  rt (PGuard (GKeySet $ keysetFromList [PublicKey "askjh",PublicKey "dfgh"] (Name $ BareName "predfun" def)))

testJSONColumns :: Spec
testJSONColumns =
  rt (ObjectMap (fromList [("A",PLiteral (LInteger 123)),("B",PLiteral (LBool False))]))

{- major TODO for module code!
testJSONModules :: Spec
testJSONModules =
  rt (Module "mname" "mk" "code!")
-}
