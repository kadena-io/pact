{-# LANGUAGE OverloadedStrings #-}
module TypesSpec (spec) where


import Test.Hspec

import Data.Aeson
import Data.Map.Strict (fromList)

import Pact.Types.Runtime
import Pact.Types.PactValue

spec :: Spec
spec = do
  describe "JSONRoundtrips" $ do
    describe "testJSONPersist" testJSONPersist
    describe "testJSONColumns "testJSONColumns
    xdescribe "testJSONModules" testJSONModules
  describe "testUnification" testUnification

rt :: (FromJSON a,ToJSON a,Show a,Eq a) => a -> Spec
rt p = it ("roundtrips " ++ show p) $ decode (encode p) `shouldBe` Just p

testJSONPersist :: Spec
testJSONPersist = do
  rt (PLiteral (LInteger 123))
  rt (PLiteral (LDecimal 123.34857))
  rt (PLiteral (LBool False))
  rt (PLiteral (LString "hello"))
  rt (PLiteral (LTime (read "2016-09-17 22:47:31.904733 UTC")))
  rt (PGuard (GKeySet $ mkKeySet [PublicKey "askjh",PublicKey "dfgh"] "predfun"))

testJSONColumns :: Spec
testJSONColumns =
  rt (ObjectMap (fromList [("A",PLiteral (LInteger 123)),("B",PLiteral (LBool False))]))

testJSONModules :: Spec
testJSONModules = return () -- TODO cover module persistence


testUnification :: Spec
testUnification = do

  it "mod spec unifies with a module that implements spec and another iface" $
      (TyModSpec [ifaceA], TyModSpec [ifaceA,ifaceB]) `shouldSatisfy`
      (uncurry canUnifyWith)

  -- TODO: add other cases, TyList and TySchema being important candidates

  where
    ifaceA, ifaceB :: String
    ifaceA = "ifaceA"
    ifaceB = "ifaceB"
