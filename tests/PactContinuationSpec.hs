{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PactContinuationSpec (spec) where

import Test.Hspec
import Utils.TestRunner
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import qualified Network.HTTP.Client as HTTP
import Data.Default (def)

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Runtime
import qualified Data.Text as T

shouldMatch :: HM.HashMap RequestKey ApiResult -> [ApiResultCheck] -> Expectation
shouldMatch results checks = mapM_ match checks
  where match ApiResultCheck{..} = do
          let apiRes = HM.lookup _arcReqKey results
          checkResult _arcIsFailure _arcExpect apiRes

spec :: Spec
spec = describe "pacts in dev server" $ do
  mgr <- runIO $ HTTP.newManager HTTP.defaultManagerSettings
  describe "testPactContinuation" $ testPactContinuation mgr
  describe "testPactRollback" $ testPactRollback mgr
  describe "testPactYield" $ testPactYield mgr
  describe "testTwoPartyEscrow" $ testTwoPartyEscrow mgr
  describe "testNestedPacts" $ testNestedPacts mgr

testNestedPacts :: HTTP.Manager -> Spec
testNestedPacts mgr = before_ flushDb $ after_ flushDb $
  it "throws error when multiple defpact executions occur in same transaction" $ do
    adminKeys <- genKeys

    moduleCmd <- mkExec (T.unpack (threeStepPactCode "nestedPact"))
                 (object ["admin-keyset" .= [_kpPublic adminKeys]])
                 def [adminKeys] (Just "test1")
    nestedExecPactCmd <- mkExec ("(nestedPact.tester)" ++ " (nestedPact.tester)")
                         Null def [adminKeys] (Just "test2")
    allResults <- runAll mgr [moduleCmd, nestedExecPactCmd]

    let allChecks = [makeCheck moduleCmd False Nothing,
                     makeCheck nestedExecPactCmd True
                      (Just "(defpact tester ()\n  (step \"st...: Failure: Nested pact execution, aborting")]

    allResults `shouldMatch` allChecks


-- CONTINUATIONS TESTS

testPactContinuation :: HTTP.Manager -> Spec
testPactContinuation mgr = before_ flushDb $ after_ flushDb $ do
  it "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (toJSON . CommandSuccess . Number) 3
        expRes = Just $ ApiResult cmdData ((Just . TxId) 0) Nothing
    testSimpleServerCmd mgr `shouldReturn` expRes

  context "when provided with correct next step" $
    it "executes the next step and updates pact's state" $
      testCorrectNextStep mgr

  context "when provided with incorrect next step" $
    it "throws error and does not update pact's state" $
      testIncorrectNextStep mgr

  context "when last step of a pact executed" $
    it "deletes pact from the state" $
      testLastStep mgr

  context "when error occurs when executing pact step" $
    it "throws error and does not update pact's state" $
      testErrStep mgr

testSimpleServerCmd :: HTTP.Manager -> IO (Maybe ApiResult)
testSimpleServerCmd mgr = do
  simpleKeys <- genKeys
  cmd <- mkExec  "(+ 1 2)" Null def
             [simpleKeys] (Just "test1")
  allResults <- runAll mgr [cmd]
  return $ HM.lookup (RequestKey (_cmdHash cmd)) allResults


testCorrectNextStep :: HTTP.Manager -> Expectation
testCorrectNextStep mgr = do
  let moduleName = "testCorrectNextStep"
  adminKeys <- genKeys

  moduleCmd       <- mkExec (T.unpack (threeStepPactCode moduleName))
                     (object ["admin-keyset" .= [_kpPublic adminKeys]])
                     def [adminKeys] (Just "test1")
  executePactCmd  <- mkExec ("(" ++ moduleName ++ ".tester)")
                     Null def [adminKeys] (Just "test2")
  contNextStepCmd <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  checkStateCmd   <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test4")
  allResults      <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd, checkStateCmd]

  let moduleCheck       = makeCheck moduleCmd False Nothing
      executePactCheck  = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck = makeCheck contNextStepCmd False $ Just "step 1"
      checkStateCheck   = makeCheck checkStateCmd True
                          (Just "Invalid continuation step value: Received 1 but expected 2")
      allChecks         = [moduleCheck, executePactCheck, contNextStepCheck, checkStateCheck]

  allResults `shouldMatch` allChecks


testIncorrectNextStep :: HTTP.Manager -> Expectation
testIncorrectNextStep mgr = do
  let moduleName = "testIncorrectNextStep"
  adminKeys <- genKeys

  moduleCmd         <- mkExec (T.unpack (threeStepPactCode moduleName))
                       (object ["admin-keyset" .= [_kpPublic adminKeys]])
                       def [adminKeys] (Just "test1")
  executePactCmd    <- mkExec ("(" ++ moduleName ++ ".tester)")
                       Null def [adminKeys] (Just "test2")
  incorrectStepCmd  <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test3")
  checkStateCmd     <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test4")
  allResults        <- runAll mgr [moduleCmd, executePactCmd, incorrectStepCmd, checkStateCmd]

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      incorrectStepCheck = makeCheck incorrectStepCmd True
                           (Just "Invalid continuation step value: Received 2 but expected 1")
      checkStateCheck    = makeCheck checkStateCmd False $ Just "step 1"
      allChecks          = [moduleCheck, executePactCheck, incorrectStepCheck, checkStateCheck]

  allResults `shouldMatch` allChecks


testLastStep :: HTTP.Manager -> Expectation
testLastStep mgr = do
  let moduleName = "testLastStep"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (threeStepPactCode moduleName))
                      (object ["admin-keyset" .= [_kpPublic adminKeys]])
                      def [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                      Null def [adminKeys] (Just "test2")
  contNextStep1Cmd <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  contNextStep2Cmd <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test4")
  checkStateCmd    <- mkCont (TxId 1) 3 False Null def [adminKeys] (Just "test5")
  allResults       <- runAll mgr [moduleCmd, executePactCmd, contNextStep1Cmd,
                              contNextStep2Cmd, checkStateCmd]

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contNextStep1Check = makeCheck contNextStep1Cmd False $ Just "step 1"
      contNextStep2Check = makeCheck contNextStep2Cmd False $ Just "step 2"
      checkStateCheck    = makeCheck checkStateCmd True
                           (Just "applyContinuation: txid not found: 1")
      allChecks          = [moduleCheck, executePactCheck, contNextStep1Check,
                            contNextStep2Check, checkStateCheck]

  allResults `shouldMatch` allChecks


testErrStep :: HTTP.Manager -> Expectation
testErrStep mgr = do
  let moduleName = "testErrStep"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (errorStepPactCode moduleName))
                      (object ["admin-keyset" .= [_kpPublic adminKeys]])
                      def [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                      Null def [adminKeys] (Just "test2")
  contErrStepCmd   <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  checkStateCmd    <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test4")
  allResults       <- runAll mgr [moduleCmd, executePactCmd, contErrStepCmd, checkStateCmd]

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contErrStepCheck   = makeCheck contErrStepCmd True Nothing
      checkStateCheck    = makeCheck checkStateCmd True
                           (Just "Invalid continuation step value: Received 2 but expected 1")
      allChecks          = [moduleCheck, executePactCheck, contErrStepCheck, checkStateCheck]

  allResults `shouldMatch` allChecks



-- ROLLBACK TESTS

testPactRollback :: HTTP.Manager -> Spec
testPactRollback mgr = before_ flushDb $ after_ flushDb $ do
  context "when provided with correct rollback step" $
    it "executes the rollback function and deletes pact from state" $
      testCorrectRollbackStep mgr

  context "when provided with incorrect rollback step" $
    it "throws error and does not delete pact from state" $
      testIncorrectRollbackStep mgr

  context "when error occurs when executing rollback function" $
    it "throws error and does not delete pact from state" $
      testRollbackErr mgr

  context "when trying to rollback a step without a rollback function" $
    it "outputs that no rollback function exists for step and deletes pact from state" $
      testNoRollbackFunc mgr


testCorrectRollbackStep :: HTTP.Manager -> Expectation
testCorrectRollbackStep mgr = do
  let moduleName = "testCorrectRollbackStep"
  adminKeys <- genKeys

  moduleCmd       <- mkExec (T.unpack (pactWithRollbackCode moduleName))
                     (object ["admin-keyset" .= [_kpPublic adminKeys]])
                     def [adminKeys] (Just "test1")
  executePactCmd  <- mkExec ("(" ++ moduleName ++ ".tester)")
                     Null def [adminKeys] (Just "test2")
  contNextStepCmd <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  rollbackStepCmd <- mkCont (TxId 1) 1 True Null def [adminKeys] (Just "test4") -- rollback = True
  checkStateCmd   <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test5")
  allResults      <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd,
                             rollbackStepCmd, checkStateCmd]

  let moduleCheck       = makeCheck moduleCmd False Nothing
      executePactCheck  = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck = makeCheck contNextStepCmd False $ Just "step 1"
      rollbackStepCheck = makeCheck rollbackStepCmd False $ Just "rollback 1"
      checkStateCheck   = makeCheck checkStateCmd True
                          (Just "applyContinuation: txid not found: 1")
      allChecks         = [moduleCheck, executePactCheck, contNextStepCheck,
                           rollbackStepCheck, checkStateCheck]

  allResults `shouldMatch` allChecks


testIncorrectRollbackStep :: HTTP.Manager -> Expectation
testIncorrectRollbackStep mgr = do
  let moduleName = "testIncorrectRollbackStep"
  adminKeys <- genKeys

  moduleCmd       <- mkExec (T.unpack (pactWithRollbackCode moduleName))
                     (object ["admin-keyset" .= [_kpPublic adminKeys]])
                     def [adminKeys] (Just "test1")
  executePactCmd  <- mkExec ("(" ++ moduleName ++ ".tester)")
                     Null def [adminKeys] (Just "test2")
  contNextStepCmd <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  incorrectRbCmd  <- mkCont (TxId 1) 2 True Null def [adminKeys] (Just "test4")
  checkStateCmd   <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test5")
  allResults      <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd,
                             incorrectRbCmd, checkStateCmd]

  let moduleCheck       = makeCheck moduleCmd False Nothing
      executePactCheck  = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck = makeCheck contNextStepCmd False $ Just "step 1"
      incorrectRbCheck  = makeCheck incorrectRbCmd True
                          (Just "Invalid rollback step value: Received 2 but expected 1")
      checkStateCheck   = makeCheck checkStateCmd False $ Just "step 2"
      allChecks         = [moduleCheck, executePactCheck, contNextStepCheck,
                           incorrectRbCheck, checkStateCheck]

  allResults `shouldMatch` allChecks


testRollbackErr :: HTTP.Manager -> Expectation
testRollbackErr mgr = do
  let moduleName = "testRollbackErr"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (pactWithRollbackErrCode moduleName))
                      (object ["admin-keyset" .= [_kpPublic adminKeys]])
                      def [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                      Null def [adminKeys] (Just "test2")
  contNextStepCmd  <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  rollbackErrCmd   <- mkCont (TxId 1) 1 True Null def [adminKeys] (Just "test4")
  checkStateCmd    <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test5")
  allResults       <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd,
                              rollbackErrCmd, checkStateCmd]

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck  = makeCheck contNextStepCmd False $ Just "step 1"
      rollbackErrCheck   = makeCheck rollbackErrCmd True Nothing
      checkStateCheck    = makeCheck checkStateCmd False $ Just "step 2"
      allChecks          = [moduleCheck, executePactCheck, contNextStepCheck,
                            rollbackErrCheck, checkStateCheck]

  allResults `shouldMatch` allChecks


testNoRollbackFunc :: HTTP.Manager -> Expectation
testNoRollbackFunc mgr = do
  let moduleName = "testNoRollbackFunc"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (threeStepPactCode moduleName))
                      (object ["admin-keyset" .= [_kpPublic adminKeys]])
                      def [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                      Null def [adminKeys] (Just "test2")
  contNextStepCmd  <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  noRollbackCmd    <- mkCont (TxId 1) 1 True Null def [adminKeys] (Just "test4")
  checkStateCmd    <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test5")
  allResults       <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd,
                              noRollbackCmd, checkStateCmd]

  let moduleCheck        = makeCheck moduleCmd False Nothing
      executePactCheck   = makeCheck executePactCmd False $ Just "step 0"
      contNextStepCheck  = makeCheck contNextStepCmd False $ Just "step 1"
      noRollbackCheck    = makeCheck noRollbackCmd False $ Just "No rollback on step 1" -- not a failure
      checkStateCheck    = makeCheck checkStateCmd True
                           (Just "applyContinuation: txid not found: 1")
      allChecks          = [moduleCheck, executePactCheck, contNextStepCheck,
                            noRollbackCheck, checkStateCheck]

  allResults `shouldMatch` allChecks



-- YIELD / RESUME TESTS

testPactYield :: HTTP.Manager -> Spec
testPactYield mgr = before_ flushDb $ after_ flushDb $ do
  context "when previous step yields value" $
    it "resumes value" $
      testValidYield mgr

  context "when previous step does not yield value" $
    it "throws error when trying to resume, and does not delete pact from state" $
      testNoYield mgr

  it "resets yielded values after each step" $
    testResetYield mgr


testValidYield :: HTTP.Manager -> Expectation
testValidYield mgr = do
  let moduleName = "testValidYield"
  adminKeys <- genKeys

  moduleCmd          <- mkExec (T.unpack (pactWithYield moduleName))
                        (object ["admin-keyset" .= [_kpPublic adminKeys]])
                        def [adminKeys] (Just "test1")
  executePactCmd <- mkExec ("(" ++ moduleName ++ ".tester \"testing\")") -- pact takes an input
                        Null def [adminKeys] (Just "test2")
  resumeAndYieldCmd  <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  resumeOnlyCmd      <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test4")
  checkStateCmd      <- mkCont (TxId 1) 3 False Null def [adminKeys] (Just "test5")
  allResults         <- runAll mgr [moduleCmd, executePactCmd, resumeAndYieldCmd,
                                resumeOnlyCmd, checkStateCmd]

  let moduleCheck         = makeCheck moduleCmd False Nothing
      executePactCheck    = makeCheck executePactCmd False $ Just "testing->Step0"
      resumeAndYieldCheck = makeCheck resumeAndYieldCmd False $ Just "testing->Step0->Step1"
      resumeOnlyCheck     = makeCheck resumeOnlyCmd False $ Just "testing->Step0->Step1->Step2"
      checkStateCheck     = makeCheck checkStateCmd True
                            (Just "applyContinuation: txid not found: 1")
      allChecks           = [moduleCheck, executePactCheck, resumeAndYieldCheck,
                             resumeOnlyCheck, checkStateCheck]

  allResults `shouldMatch` allChecks


testNoYield :: HTTP.Manager -> Expectation
testNoYield mgr = do
  let moduleName = "testNoYield"
  adminKeys <- genKeys

  moduleCmd      <- mkExec (T.unpack (pactWithYieldErr moduleName))
                    (object ["admin-keyset" .= [_kpPublic adminKeys]])
                    def [adminKeys] (Just "test1")
  executePactCmd <- mkExec ("(" ++ moduleName ++ ".tester \"testing\")") -- pact takes an input
                    Null def [adminKeys] (Just "test2")
  noYieldStepCmd <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  resumeErrCmd   <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test3")
  checkStateCmd  <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test5")
  allResults     <- runAll mgr [moduleCmd, executePactCmd, noYieldStepCmd,
                           resumeErrCmd, checkStateCmd]

  let moduleCheck      = makeCheck moduleCmd False Nothing
      executePactCheck = makeCheck executePactCmd False $ Just "testing->Step0"
      noYieldStepCheck = makeCheck noYieldStepCmd False $ Just "step 1 has no yield"
      resumeErrCheck   = makeCheck resumeErrCmd True Nothing
      checkStateCheck  = makeCheck checkStateCmd True
                         (Just "Invalid continuation step value: Received 1 but expected 2")
      allChecks        = [moduleCheck, executePactCheck, noYieldStepCheck,
                         resumeErrCheck, checkStateCheck]

  allResults `shouldMatch` allChecks


testResetYield :: HTTP.Manager -> Expectation
testResetYield mgr = do
  let moduleName = "testResetYield"
  adminKeys <- genKeys

  moduleCmd        <- mkExec (T.unpack (pactWithSameNameYield moduleName))
                        (object ["admin-keyset" .= [_kpPublic adminKeys]])
                        def [adminKeys] (Just "test1")
  executePactCmd   <- mkExec ("(" ++ moduleName ++ ".tester)")
                        Null def [adminKeys] (Just "test2")
  yieldSameKeyCmd  <- mkCont (TxId 1) 1 False Null def [adminKeys] (Just "test3")
  resumeStepCmd    <- mkCont (TxId 1) 2 False Null def [adminKeys] (Just "test4")
  checkStateCmd    <- mkCont (TxId 1) 3 False Null def [adminKeys] (Just "test5")
  allResults       <- runAll mgr [moduleCmd, executePactCmd, yieldSameKeyCmd,
                              resumeStepCmd, checkStateCmd]

  let moduleCheck       = makeCheck moduleCmd False Nothing
      executePactCheck  = makeCheck executePactCmd False $ Just "step 0"
      yieldSameKeyCheck = makeCheck yieldSameKeyCmd False $ Just "step 1"
      resumeStepCheck   = makeCheck resumeStepCmd False $ Just "step 1"
      checkStateCheck   = makeCheck checkStateCmd True
                           (Just "applyContinuation: txid not found: 1")
      allChecks         = [moduleCheck, executePactCheck, yieldSameKeyCheck,
                           resumeStepCheck, checkStateCheck]

  allResults `shouldMatch` allChecks



-- TWO PARTY ESCROW TESTS

testTwoPartyEscrow :: HTTP.Manager -> Spec
testTwoPartyEscrow mgr = before_ flushDb $ after_ flushDb $ do
  context "when debtor tries to cancel pre-timeout" $
    it "throws error and money still escrowed" $
      testDebtorPreTimeoutCancel mgr

  context "when debtor tries to cancel after timeout" $
    it "cancels escrow and deposits escrowed amount back to debtor" $
      testDebtorPostTimeoutCancel mgr

  it "cancels escrow immediately if creditor cancels" $
    testCreditorCancel mgr

  it "throws error when creditor or debtor try to finish alone" $
    testFinishAlone mgr

  it "throws error when final price negotiated up" $
    testPriceNegUp mgr

  context "when both debtor and creditor finish together" $
    it "finishes escrow if final price stays the same or negotiated down" $
      testValidEscrowFinish mgr

testDebtorPreTimeoutCancel :: HTTP.Manager -> Expectation
testDebtorPreTimeoutCancel mgr = do
  let testPath = testDir ++ "cont-scripts/fail-deb-cancel-"

  (_, tryCancelCmd)        <- mkApiReq (testPath ++ "01-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "02-balance.yaml")
  let allCmds = [tryCancelCmd, checkStillEscrowCmd]

  let cancelMsg = T.concat ["(enforce-one\n        \"Cancel c...: Failure:",
                            " Tx Failed: Cancel can only be effected by",
                            " creditor, or debitor after timeout"]
      tryCancelCheck        = makeCheck tryCancelCmd True $ Just $ String cancelMsg
      checkStillEscrowCheck = makeCheck checkStillEscrowCmd False $ Just "98.00"
      allChecks             = [tryCancelCheck, checkStillEscrowCheck]

  twoPartyEscrow allCmds allChecks mgr

testDebtorPostTimeoutCancel :: HTTP.Manager -> Expectation
testDebtorPostTimeoutCancel mgr = do
  let testPath = testDir ++ "cont-scripts/pass-deb-cancel-"

  (_, setTimeCmd)          <- mkApiReq (testPath ++ "01-set-time.yaml")
  (_, tryCancelCmd)        <- mkApiReq (testPath ++ "02-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "03-balance.yaml")
  let allCmds = [setTimeCmd, tryCancelCmd, checkStillEscrowCmd]

  let setTimeCheck = makeCheck setTimeCmd False Nothing
      tryCancelCheck = makeCheck tryCancelCmd False Nothing
      checkStillEscrowCheck = makeCheck checkStillEscrowCmd False $ Just "100.00"
      allChecks = [setTimeCheck, tryCancelCheck, checkStillEscrowCheck]

  twoPartyEscrow allCmds allChecks mgr

testCreditorCancel :: HTTP.Manager -> Expectation
testCreditorCancel mgr = do
  let testPath = testDir ++ "cont-scripts/pass-cred-cancel-"

  (_, resetTimeCmd)        <- mkApiReq (testPath ++ "01-reset.yaml")
  (_, credCancelCmd)       <- mkApiReq (testPath ++ "02-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "03-balance.yaml")
  let allCmds = [resetTimeCmd, credCancelCmd, checkStillEscrowCmd]

  let resetTimeCheck = makeCheck resetTimeCmd False Nothing
      credCancelCheck = makeCheck credCancelCmd False Nothing
      checkStillEscrowCheck = makeCheck checkStillEscrowCmd False $ Just "100.00"
      allChecks = [resetTimeCheck, credCancelCheck, checkStillEscrowCheck]

  twoPartyEscrow allCmds allChecks mgr

testFinishAlone :: HTTP.Manager -> Expectation
testFinishAlone mgr = do
  let testPathCred  = testDir ++ "cont-scripts/fail-cred-finish-"
      testPathDeb   = testDir ++ "cont-scripts/fail-deb-finish-"

  (_, tryCredAloneCmd) <- mkApiReq (testPathCred ++ "01-cont.yaml")
  (_, tryDebAloneCmd)  <- mkApiReq (testPathDeb ++ "01-cont.yaml")
  let allCmds = [tryCredAloneCmd, tryDebAloneCmd]

  let tryCredAloneCheck = makeCheck tryCredAloneCmd True
                          (Just "(enforce-guard g): Failure: Tx Failed: Keyset failure (keys-all)")
      tryDebAloneCheck  = makeCheck tryDebAloneCmd True
                          (Just "(enforce-guard g): Failure: Tx Failed: Keyset failure (keys-all)")
      allChecks         = [tryCredAloneCheck, tryDebAloneCheck]

  twoPartyEscrow allCmds allChecks mgr

testPriceNegUp :: HTTP.Manager -> Expectation
testPriceNegUp mgr = do
  let testPath = testDir ++ "cont-scripts/fail-both-price-up-"

  (_, tryNegUpCmd) <- mkApiReq (testPath ++ "01-cont.yaml")
  let tryNegUpCheck = makeCheck tryNegUpCmd True
                      (Just "(enforce (>= escrow-amount pri...: Failure: Tx Failed: Price cannot negotiate up")

  twoPartyEscrow [tryNegUpCmd] [tryNegUpCheck] mgr

testValidEscrowFinish :: HTTP.Manager -> Expectation
testValidEscrowFinish mgr = do
  let testPath = testDir ++ "cont-scripts/pass-both-price-down-"

  (_, tryNegDownCmd)  <- mkApiReq (testPath ++ "01-cont.yaml")
  (_, credBalanceCmd) <- mkApiReq (testPath ++ "02-cred-balance.yaml")
  (_, debBalanceCmd) <- mkApiReq (testPath ++ "03-deb-balance.yaml")
  let allCmds = [tryNegDownCmd, credBalanceCmd, debBalanceCmd]

  let tryNegDownCheck  = makeCheck tryNegDownCmd False
                         (Just "Escrow completed with 1.75 paid and 0.25 refunded")
      credBalanceCheck = makeCheck credBalanceCmd False $ Just "1.75"
      debBalanceCheck  = makeCheck debBalanceCmd False $ Just "98.25"
      allChecks        = [tryNegDownCheck, credBalanceCheck, debBalanceCheck]

  twoPartyEscrow allCmds allChecks mgr

twoPartyEscrow :: [Command T.Text] -> [ApiResultCheck] -> HTTP.Manager -> Expectation
twoPartyEscrow testCmds testChecks mgr = do
  let setupPath = testDir ++ "cont-scripts/setup-"

  (_, sysModuleCmd)  <- mkApiReq (setupPath ++ "01-system.yaml")
  (_, acctModuleCmd) <- mkApiReq (setupPath ++ "02-accounts.yaml")
  (_, testModuleCmd) <- mkApiReq (setupPath ++ "03-test.yaml")
  (_, createAcctCmd) <- mkApiReq (setupPath ++ "04-create.yaml")
  (_, resetTimeCmd)  <- mkApiReq (setupPath ++ "05-reset.yaml")
  (_, runEscrowCmd)  <- mkApiReq (setupPath ++ "06-escrow.yaml")
  (_, balanceCmd)    <- mkApiReq (setupPath ++ "07-balance.yaml")
  let allCmds = sysModuleCmd : acctModuleCmd : testModuleCmd : createAcctCmd
                : resetTimeCmd : runEscrowCmd : balanceCmd : testCmds
  allResults <- runAll mgr allCmds

  let sysModuleCheck      = makeCheck sysModuleCmd False $ Just "system module loaded"
      acctModuleCheck     = makeCheck acctModuleCmd False $ Just "TableCreated"
      testModuleCheck     = makeCheck testModuleCmd False $ Just "test module loaded"
      createAcctCheck     = makeCheck createAcctCmd False Nothing -- Alice should be funded with $100
      resetTimeCheck      = makeCheck resetTimeCmd False Nothing
      runEscrowCheck      = makeCheck runEscrowCmd False Nothing
      balanceCheck        = makeCheck balanceCmd False $ Just "98.00"
      allChecks           = sysModuleCheck : acctModuleCheck : testModuleCheck
                            : createAcctCheck : resetTimeCheck : runEscrowCheck
                            : balanceCheck : testChecks

  allResults `shouldMatch` allChecks
