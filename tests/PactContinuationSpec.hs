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
  describe "testPactContinuation" testPactContinuation
  describe "testPactRollback" testPactRollback
  describe "testPactYield" testPactYield



-- CONTINUATIONS TESTS

testPactContinuation :: Spec
testPactContinuation = before_ flushDb $ after_ flushDb $ do
  it "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (toJSON . CommandSuccess . Number) 3
        expRes = Just $ ApiResult cmdData ((Just . TxId) 0) Nothing
    testSimpleServerCmd `shouldReturn` expRes

  context "when provided with correct next step" $
    it "executes the next step and updates pact's state" $
      testCorrectNextStep `shouldReturn` True

  context "when provided with incorrect next step" $
    it "throws error and does not update pact's state" $
      testIncorrectNextStep `shouldReturn` True

  context "when last step of a pact executed" $
    it "deletes pact from the state" $
      testLastStep `shouldReturn` True

  context "when error occurs when executing pact step" $
    it "throws error and does not update pact's state" $
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

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contErrStepCheck   = makeCheck contErrStepCmd True Nothing
      checkStateCheck    = makeCheck checkStateCmd True
                           (Just "Invalid continuation step value: Received 2 but expected 1")
      allChecks          = [moduleCheck, executePactCheck, contErrStepCheck, checkStateCheck]
 
  return (checkResults allResults allChecks)



-- ROLLBACK TESTS

testPactRollback :: Spec
testPactRollback = before_ flushDb $ after_ flushDb $ do
  context "when provided with correct rollback step" $
    it "executes the rollback function and deletes pact from state" $
      testCorrectRollbackStep `shouldReturn` True

  context "when provided with incorrect rollback step" $
    it "throws error and does not delete pact from state" $
      testIncorrectRollbackStep `shouldReturn` True

  context "when error occurs when executing rollback function" $
    it "throws error and does not delete pact from state" $
      testRollbackErr `shouldReturn` True

  context "when trying to rollback a step without a rollback function" $
    it "outputs that no rollback function exists for step and deletes pact from state" $
      testNoRollbackFunc `shouldReturn` True

testCorrectRollbackStep :: IO Bool
testCorrectRollbackStep = do
  let moduleName = "testCorrectRollbackStep"
  adminKeys <- genKeys

  moduleCmd       <- mkExec (T.unpack (pactWithRollbackCode moduleName))
                     (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                     Nothing [adminKeys] (Just "test1")
  executePactCmd  <- mkExec ("(" ++ moduleName ++ ".tester)")
                     Null Nothing [adminKeys] (Just "test2")
  contNextStepCmd <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  rollbackStepCmd <- mkCont (TxId 1) 1 True Null Nothing [adminKeys] (Just "test4") -- rollback = True
  checkStateCmd   <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test5")
  allResults      <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                             rollbackStepCmd, checkStateCmd]

  let moduleCheck       = makeCheck moduleCmd False Nothing
      executePactCheck  = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck = makeCheck contNextStepCmd False $ Just "step 1"
      rollbackStepCheck = makeCheck rollbackStepCmd False $ Just "rollback 1"
      checkStateCheck   = makeCheck checkStateCmd True
                          (Just "applyContinuation: txid not found: 1")
      allChecks         = [moduleCheck, executePactCheck, contNextStepCheck,
                           rollbackStepCheck, checkStateCheck]
 
  return (checkResults allResults allChecks)

testIncorrectRollbackStep :: IO Bool
testIncorrectRollbackStep = do
  let moduleName = "testIncorrectRollbackStep"
  adminKeys <- genKeys

  moduleCmd       <- mkExec (T.unpack (pactWithRollbackCode moduleName))
                     (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                     Nothing [adminKeys] (Just "test1")
  executePactCmd  <- mkExec ("(" ++ moduleName ++ ".tester)")
                     Null Nothing [adminKeys] (Just "test2")
  contNextStepCmd <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  incorrectRbCmd  <- mkCont (TxId 1) 2 True Null Nothing [adminKeys] (Just "test4")
  checkStateCmd   <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test5")
  allResults      <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                             incorrectRbCmd, checkStateCmd]

  let moduleCheck       = makeCheck moduleCmd False Nothing
      executePactCheck  = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck = makeCheck contNextStepCmd False $ Just "step 1"
      incorrectRbCheck  = makeCheck incorrectRbCmd True $
                          (Just "Invalid rollback step value: Received 2 but expected 1")
      checkStateCheck   = makeCheck checkStateCmd False $ Just "step 2"
      allChecks         = [moduleCheck, executePactCheck, contNextStepCheck,
                           incorrectRbCheck, checkStateCheck]
 
  return (checkResults allResults allChecks)

testRollbackErr :: IO Bool
testRollbackErr = do
  let moduleName = "testRollbackErr"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (pactWithRollbackErrCode moduleName))
                      (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                      Nothing [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                      Null Nothing [adminKeys] (Just "test2")
  contNextStepCmd  <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  rollbackErrCmd   <- mkCont (TxId 1) 1 True Null Nothing [adminKeys] (Just "test4")
  checkStateCmd    <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test5")
  allResults       <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                              rollbackErrCmd, checkStateCmd]
  flushDb

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck  = makeCheck contNextStepCmd False $ Just "step 1"
      rollbackErrCheck   = makeCheck rollbackErrCmd True Nothing
      checkStateCheck    = makeCheck checkStateCmd False $ Just "step 2"
      allChecks          = [moduleCheck, executePactCheck, contNextStepCheck,
                            rollbackErrCheck, checkStateCheck]
 
  return (checkResults allResults allChecks)

testNoRollbackFunc :: IO Bool
testNoRollbackFunc = do
  let moduleName = "testNoRollbackFunc"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (threeStepPactCode moduleName))
                      (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                      Nothing [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                      Null Nothing [adminKeys] (Just "test2")
  contNextStepCmd  <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  noRollbackCmd    <- mkCont (TxId 1) 1 True Null Nothing [adminKeys] (Just "test4")
  checkStateCmd    <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test5")
  allResults       <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                              noRollbackCmd, checkStateCmd]

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck  = makeCheck contNextStepCmd False $ Just "step 1"
      noRollbackCheck    = makeCheck noRollbackCmd False $ Just "No rollback on step 1" -- not a failure
      checkStateCheck    = makeCheck checkStateCmd True
                           (Just "applyContinuation: txid not found: 1")
      allChecks          = [moduleCheck, executePactCheck, contNextStepCheck,
                            noRollbackCheck, checkStateCheck]

  return (checkResults allResults allChecks)



-- YIELD / RESUME TESTS

testPactYield :: Spec
testPactYield = before_ flushDb $ after_ flushDb $ do
  context "when previous step yields value" $
    it "resumes value" $
      testValidYield `shouldReturn` True

  context "when previous step does not yield value" $
    it "throws error when trying to resume, and does not delete pact from state" $
      testNoYield `shouldReturn` True

  it "resets yielded values after each step" $
    testResetYield `shouldReturn` True
    
testValidYield :: IO Bool
testValidYield = do
  let moduleName = "testValidYield"
  adminKeys <- genKeys

  moduleCmd          <- mkExec (T.unpack (pactWithYield moduleName))
                        (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                        Nothing [adminKeys] (Just "test1")
  executePactCmd <- mkExec ("(" ++ moduleName ++ ".tester \"testing\")") -- pact takes an input
                        Null Nothing [adminKeys] (Just "test2")
  resumeAndYieldCmd  <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  resumeOnlyCmd      <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test4")
  checkStateCmd      <- mkCont (TxId 1) 3 False Null Nothing [adminKeys] (Just "test5")
  allResults         <- runAll [moduleCmd, executePactCmd, resumeAndYieldCmd,
                                resumeOnlyCmd, checkStateCmd]

  let moduleCheck         = makeCheck moduleCmd False Nothing
      executePactCheck    = makeCheck executePactCmd False $ Just "testing->Step0"
      resumeAndYieldCheck = makeCheck resumeAndYieldCmd False $ Just "testing->Step0->Step1"
      resumeOnlyCheck     = makeCheck resumeOnlyCmd False $ Just "testing->Step0->Step1->Step2"
      checkStateCheck     = makeCheck checkStateCmd True 
                           (Just "applyContinuation: txid not found: 1")
      allChecks           = [moduleCheck, executePactCheck, resumeAndYieldCheck,
                             resumeOnlyCheck, checkStateCheck]

  return (checkResults allResults allChecks)
       
testNoYield :: IO Bool
testNoYield = do
  let moduleName = "testNoYield"
  adminKeys <- genKeys

  moduleCmd      <- mkExec (T.unpack (pactWithYieldErr moduleName))
                    (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                    Nothing [adminKeys] (Just "test1")
  executePactCmd <- mkExec ("(" ++ moduleName ++ ".tester \"testing\")") -- pact takes an input
                    Null Nothing [adminKeys] (Just "test2")
  noYieldStepCmd <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  resumeErrCmd   <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test3")
  checkStateCmd  <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test5")
  allResults     <- runAll [moduleCmd, executePactCmd, noYieldStepCmd,
                           resumeErrCmd, checkStateCmd]

  let moduleCheck      = makeCheck moduleCmd False Nothing
      executePactCheck = makeCheck executePactCmd False $ Just "testing->Step0"
      noYieldStepCheck = makeCheck noYieldStepCmd False $ Just "step 1 has no yield"
      resumeErrCheck   = makeCheck resumeErrCmd True Nothing
      checkStateCheck  = makeCheck checkStateCmd True
                         (Just "Invalid continuation step value: Received 1 but expected 2")
      allChecks        = [moduleCheck, executePactCheck, noYieldStepCheck,
                         resumeErrCheck, checkStateCheck]

  return (checkResults allResults allChecks)

testResetYield :: IO Bool
testResetYield = do
  let moduleName = "testResetYield"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (pactWithSameNameYield moduleName))
                        (object ["admin-keyset" .= [(_kpPublic adminKeys)]])
                        Nothing [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)") 
                        Null Nothing [adminKeys] (Just "test2")
  yieldSameKeyCmd  <- mkCont (TxId 1) 1 False Null Nothing [adminKeys] (Just "test3")
  resumeStepCmd    <- mkCont (TxId 1) 2 False Null Nothing [adminKeys] (Just "test4")
  checkStateCmd    <- mkCont (TxId 1) 3 False Null Nothing [adminKeys] (Just "test5")
  allResults       <- runAll [moduleCmd, executePactCmd, yieldSameKeyCmd,
                              resumeStepCmd, checkStateCmd]

  let moduleCheck       = makeCheck moduleCmd False Nothing
      executePactCheck  = makeCheck executePactCmd False $ Just "step 0"
      yieldSameKeyCheck = makeCheck yieldSameKeyCmd False $ Just "step 1"
      resumeStepCheck   = makeCheck resumeStepCmd False $ Just "step 1"
      checkStateCheck   = makeCheck checkStateCmd True 
                           (Just "applyContinuation: txid not found: 1")
      allChecks         = [moduleCheck, executePactCheck, yieldSameKeyCheck,
                           resumeStepCheck, checkStateCheck]

  return (checkResults allResults allChecks)
