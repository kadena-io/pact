{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

spec :: Spec
spec = describe "testPactContinuation" testPactContinuation

testPactContinuation :: Spec
testPactContinuation = before_ flushDb $ do
  it "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (toJSON . CommandSuccess . Number) 3
        expRes = Just $ ApiResult cmdData ((Just . TxId) 0) Nothing
    testSimpleServerCmd `shouldReturn` expRes

  context "when provided with correct next step" $ do
    it "executes the next step and updates pact state" $ do 
      testNextStep `shouldReturn` True

testNextStep :: IO Bool
testNextStep = do
  let moduleName = "testNextStep"
  adminKeys <- genKeys

  moduleCmd       <- mkExec (T.unpack (threeStepPactCode moduleName))
                     (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                     Nothing [adminKeys] (Just "test1")
  executePactCmd  <- mkExec ("(" ++ moduleName ++ ".tester)")
                     Null Nothing [adminKeys] (Just "test2")
  contNextStepCmd <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  checkStateCmd   <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test4")
  allResults      <- runAll [moduleCmd, executePactCmd, contNextStepCmd, checkStateCmd]
  flushDb
  

  let moduleCheck       = makeCheck moduleCmd False Nothing
      executePactCheck  = makeCheck executePactCmd False $ Just "step 1"
      contNextStepCheck = makeCheck contNextStepCmd False $ Just "step 2"
      checkStateCheck   = makeCheck checkStateCmd True
                          (Just "Invalid continuation step value: Received 1 but expected 2")
      allChecks         = [moduleCheck, executePactCheck, contNextStepCheck, checkStateCheck]
 
  return (checkResults allResults allChecks)


testSimpleServerCmd :: IO (Maybe ApiResult)
testSimpleServerCmd = do
  simpleKeys <- genKeys
  cmd <- mkExec  "(+ 1 2)" Null Nothing
             [simpleKeys] (Just "test1")
  allResults <- runAll [cmd]
  return $ HM.lookup (RequestKey (_cmdHash cmd)) allResults

