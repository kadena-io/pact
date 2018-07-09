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

spec :: Spec
spec = describe "pacts in dev server" $ do
  describe "testPactContinuation" $ do
    testPactContinuation

  {--describe "testPactRollback" $ do
    testPactRollback--}

testPactContinuation :: Spec
testPactContinuation = before_ flushDb $ after_ flushDb $ do
  it "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (toJSON . CommandSuccess . Number) 3
        expRes = Just $ ApiResult cmdData ((Just . TxId) 0) Nothing
    testSimpleServerCmd `shouldReturn` expRes

  context "when provided with correct next step" $ do
    it "executes the next step and updates pact's state" $ do 
      testCorrectNextStep `shouldReturn` True

  context "when provided with incorrect next step" $ do
    it "throws error and does not update pact's state" $ do
      testIncorrectNextStep `shouldReturn` True

  context "when last step of a pact executed" $ do
    it "deletes pact from the state" $ do
      testLastStep `shouldReturn` True

  context "when error occurs when executing pact step" $ do
    it "throws error and does not update pact's state" $ do
      testErrStep `shouldReturn` True

testSimpleServerCmd :: IO (Maybe ApiResult)
testSimpleServerCmd = do
  simpleKeys <- genKeys
  cmd <- mkExec  "(+ 1 2)" Null Nothing
             [simpleKeys] (Just "test1")
  allResults <- runAll [cmd]
  return $ HM.lookup (RequestKey (_cmdHash cmd)) allResults

testCorrectNextStep :: IO Bool
testCorrectNextStep = do
  let moduleName = "testCorrectNextStep"
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
      executePactCheck  = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck = makeCheck contNextStepCmd False $ Just "step 1"
      checkStateCheck   = makeCheck checkStateCmd True
                          (Just "Invalid continuation step value: Received 1 but expected 2")
      allChecks         = [moduleCheck, executePactCheck, contNextStepCheck, checkStateCheck]
 
  return (checkResults allResults allChecks)

testIncorrectNextStep :: IO Bool
testIncorrectNextStep = do
  let moduleName = "testIncorrectNextStep"
  adminKeys <- genKeys

  moduleCmd         <- mkExec (T.unpack (threeStepPactCode moduleName))
                       (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                       Nothing [adminKeys] (Just "test1")
  executePactCmd    <- mkExec ("(" ++ moduleName ++ ".tester)")
                       Null Nothing [adminKeys] (Just "test2")
  incorrectStepCmd  <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test3")
  checkStateCmd     <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test4")
  allResults        <- runAll [moduleCmd, executePactCmd, incorrectStepCmd, checkStateCmd]
  flushDb

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      incorrectStepCheck = makeCheck incorrectStepCmd True $
                           (Just "Invalid continuation step value: Received 2 but expected 1")
      checkStateCheck    = makeCheck checkStateCmd False $ Just "step 1"
      allChecks          = [moduleCheck, executePactCheck, incorrectStepCheck, checkStateCheck]

  return (checkResults allResults allChecks)

testLastStep :: IO Bool
testLastStep = do
  let moduleName = "testLastStep"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (threeStepPactCode moduleName))
                      (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                      Nothing [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                      Null Nothing [adminKeys] (Just "test2")
  contNextStep1Cmd <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  contNextStep2Cmd <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test4")
  checkStateCmd    <- mkCont (TxId 1) 3 False Null Nothing [adminKeys] (Just "test5")
  allResults       <- runAll [moduleCmd, executePactCmd, contNextStep1Cmd,
                              contNextStep2Cmd, checkStateCmd]
  flushDb

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contNextStep1Check = makeCheck contNextStep1Cmd False $ Just "step 1"
      contNextStep2Check = makeCheck contNextStep2Cmd False $ Just "step 2"
      checkStateCheck    = makeCheck checkStateCmd True
                           (Just "applyContinuation: txid not found: 1")
      allChecks          = [moduleCheck, executePactCheck, contNextStep1Check,
                            contNextStep2Check, checkStateCheck]
 
  return (checkResults allResults allChecks)

testErrStep :: IO Bool
testErrStep = do
  let moduleName = "testErrStep"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (errorStepPactCode moduleName))
                      (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                      Nothing [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                      Null Nothing [adminKeys] (Just "test2")
  contErrStepCmd   <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  checkStateCmd    <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test4")
  allResults       <- runAll [moduleCmd, executePactCmd, contErrStepCmd, checkStateCmd]
  flushDb

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contErrStepCheck   = makeCheck contErrStepCmd True Nothing
      checkStateCheck    = makeCheck checkStateCmd True
                           (Just "Invalid continuation step value: Received 2 but expected 1")
      allChecks          = [moduleCheck, executePactCheck, contErrStepCheck, checkStateCheck]
 
  return (checkResults allResults allChecks)

{--testPactRollback :: Spec
testPactRollback = before_ flushDb $ after_ flushDb $ do
  testCorrectRollbackStep

  {--context "when provided with incorrect rollback step" $ do
    it "throws error and does not delete pact from state" $ do
      testIncorrectRollbackStep `shouldReturn` True

  context "when error occurs when executing rollback function" $ do
    it "throws error and does not delete pact from state" $ do
      testRollbackErr `shouldReturn` True

  context "when trying to rollback a step without a rollback function" $ do
    it "does not delete pact from state" $ do
      testNoRollbackFunc `shouldReturn` True--}

testCorrectRollbackStep :: Spec
testCorrectRollbackStep = 
  context "when provided with correct rollback step" $
    it "executes the rollback function and deletes pact from state" $ do
      getExpect
      print "hello"
      "hello" `shouldBe` "world"

getExpect :: Expectation
getExpect = True `shouldBe` True--}
