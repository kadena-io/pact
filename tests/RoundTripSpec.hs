{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module RoundTripSpec (spec) where


import Test.Hspec

import Bound
import Data.Aeson
import Data.Default
import Data.Map.Strict (fromList)
import qualified Data.HashSet as HS
import qualified Data.Vector as V

import Pact.Types.RowData
import Pact.Types.Runtime
import Pact.Types.PactValue

spec :: Spec
spec = do
  describe "JSONRoundtrips" $ do
    describe "testJSONPersist" testJSONPersist
    describe "testJSONColumns "testJSONColumns
    describe "testJSONModules" testJSONModules
  describe "testUnification" testUnification

rt :: (FromJSON a,ToJSON a,Show a,Eq a) => String -> a -> Spec
rt n p = it ("roundtrips " ++ n) $ do
  decode (encode p) `shouldBe` Just p

testJSONPersist :: Spec
testJSONPersist = do
  rt "integer" (PLiteral (LInteger 123))
  rt "decimal" (PLiteral (LDecimal 123.34857))
  rt "bool" (PLiteral (LBool False))
  rt "string" (PLiteral (LString "hello"))
  rt "time" (PLiteral (LTime (read "2016-09-17 22:47:31.904733 UTC")))
  rt "keyset" (PGuard (GKeySet $ mkKeySet [PublicKey "askjh",PublicKey "dfgh"] "predfun"))
  rt "modref" (PModRef (ModRef "foo.bar" (Just ["baz", "bof.quux"]) def))
  rt "list" (PList (V.fromList [PLiteral (LInteger 123), PLiteral (LBool False), PLiteral (LTime (read "2016-09-17 22:47:31.904733 UTC"))]))
  rt "object" (PObject (ObjectMap (fromList [("A",PLiteral (LInteger 123)), ("B",PLiteral (LBool False))])))

testJSONColumns :: Spec
testJSONColumns = do
  rt "object" obj
  it "roundtrips as rowdata" $ do
    decode @RowData (encode obj) `shouldBe` Just (RowData RDV0 (pactValueToRowData <$> obj))
  where
  uguard = GUser $ UserGuard (Name (BareName "a" def)) [PLiteral (LInteger 123)]
  pguard = GPact $ PactGuard (PactId "123") "456"
  ksguard = GKeySet $ mkKeySet [PublicKey "askjh",PublicKey "dfgh"] "predfun"
  ksrguard = GKeySetRef $ KeySetName "beepboop"
  mguard = GModule $ ModuleGuard (ModuleName "beep" Nothing) "boop"
  obj = ObjectMap $ fromList
    [("A", PLiteral (LInteger 123))
    ,("B", PLiteral (LBool False))
    ,("C", PLiteral (LString "hello"))
    ,("D", PLiteral (LTime (read "2016-09-17 22:47:31.904733 UTC")))
    ,("E", PGuard uguard)
    ,("F", PModRef (ModRef "foo.bar" (Just ["baz", "bof.quux"]) def))
    ,("G", PObject (ObjectMap (fromList [("A",PLiteral (LInteger 123))])))
    ,("H", PList (V.fromList [PLiteral (LInteger 123), PLiteral (LBool False), PLiteral (LTime (read "2016-09-17 22:47:31.904733 UTC"))]))
    ,("I", PGuard pguard)
    ,("J", PGuard ksguard)
    ,("K", PGuard ksrguard)
    ,("L", PGuard mguard)]

testJSONModules :: Spec
testJSONModules = rt "module" tmod
  where
    tmod = TModule
           (MDModule (Module "foo" (Governance (Right (tStr "hi")))
                      def "" (ModuleHash pactInitialHash) HS.empty [] []))
           (abstract (const (Just ()))
            (toTList TyAny def
             [tlet1]))
           def
    tlet1 = TBinding []
           (abstract (\b -> if b == na then Just 0 else Nothing)
            (toTList TyAny def
             [(TVar na def),tlet2])) -- bound var + let
           BindLet def
    tlet2 = TBinding []
           (abstract (\b -> if b == nb then Just 0 else Nothing)
            (toTList TyAny def
             [(TVar na def),(TVar nb def)])) -- free var + bound var
           BindLet def
    na = Name $ BareName "a" def
    nb = Name $ BareName "b" def



testUnification :: Spec
testUnification = do

  it "mod spec unifies with a module with greater coverage" $
      (modRef [ifaceA], modRef [ifaceA,ifaceB]) `shouldSatisfy`
      (uncurry canUnifyWith)

  it "mod spec unifies with a module with same coverage" $
      (modRef [ifaceA,ifaceB], modRef [ifaceA,ifaceB]) `shouldSatisfy`
      (uncurry canUnifyWith)

  it "mod spec does not unify with a module with less coverage" $
      (modRef [ifaceA,ifaceB], modRef [ifaceA]) `shouldSatisfy`
      (not . uncurry canUnifyWith)

  it "mod spec does not unify with a module with different coverage" $
      (modRef [ifaceB], modRef [ifaceA]) `shouldSatisfy`
      (not . uncurry canUnifyWith)

  -- TODO: add other cases, TyList and TySchema being important candidates

  where
    ifaceA = "ifaceA"
    ifaceB = "ifaceB"
    modRef :: [ModuleName] -> Type (Term ())
    modRef is = modRefTy (ModRef "a" (Just is) def)
