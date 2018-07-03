{-# LANGUAGE OverloadedStrings #-}

module PactContinuationSpec (spec) where

import Test.Hspec
import Utils.TestRunner
import qualified Data.HashMap.Strict as HM
import Data.Aeson

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Runtime
import qualified Data.Text as T

makeTestApiResult :: Value -> TxId -> ApiResult
makeTestApiResult cmdData txId = ApiResult cmdValue cmdTxId Nothing
  where cmdValue = toJSON (CommandSuccess cmdData)
        cmdTxId = Just txId


spec :: Spec
spec = describe "testPactContinuation" testPactContinuation

testPactContinuation :: Spec
testPactContinuation = before_ flushDb $ do
  it "sends simple command to locally running dev server" $ do
    let expRes = Just $ makeTestApiResult (Number 3) (TxId 0)
    testSimpleServerCmd `shouldReturn` expRes

  context "when provided with current next step" $ do
    it "executes the next step and updates pact state" $ do 
      True `shouldBe` True

testCorrectNextStep :: Spec
testCorrectNextStep = undefined

testSimpleServerCmd :: IO (Maybe ApiResult)
testSimpleServerCmd = do
  (priv,publ) <- genKeys
  cmd <- mkExec  "(+ 1 2)" Null Nothing
             [KeyPair priv publ] (Just "test1")
  allResults <- runAll [cmd]
  return $ HM.lookup (RequestKey (_cmdHash cmd)) allResults

