{-# LANGUAGE OverloadedStrings #-}
module TypesSpec (spec) where


import Test.Hspec

import Data.Aeson
import Data.Map.Strict (fromList)

import Pact.Types.PactValue as PV
import Pact.Types.Exp
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
  rt (VLiteral (LInteger 123))
  rt (VLiteral (LDecimal 123.34857))
  rt (VLiteral (LBool False))
  rt (VLiteral (LString "hello"))
  rt (VLiteral (LTime (read "2016-09-17 22:47:31.904733 UTC")))
  rt (VGuard (GKeySet (KeySet [PublicKey "askjh",PublicKey "dfgh"] (Name "predfun" def))))

testJSONColumns :: Spec
testJSONColumns =
  rt (ObjectMap (fromList [("A",VLiteral (LInteger 123)),("B", VLiteral (LBool False))]))

{- major TODO for module code!
testJSONModules :: Spec
testJSONModules =
  rt (Module "mname" "mk" "code!")
-}
