{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PactContinuationSpec (spec) where

import Test.Hspec
import Utils.TestRunner
import qualified Data.HashMap.Strict as HM
import Data.Aeson
import qualified Network.HTTP.Client as HTTP
import Data.Default (def)
import Data.Decimal
import Control.Monad.Reader

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Runtime
import Pact.Types.PactValue
import qualified Data.Text as T


----- UTILS ------

shouldMatch' :: HasCallStack => ApiResultCheck -> HM.HashMap RequestKey ApiResult -> Expectation
shouldMatch' ApiResultCheck{..} results = do
          let apiRes = HM.lookup _arcReqKey results
          checkResult _arcIsFailure _arcExpect apiRes

succeedsWith :: HasCallStack => Command Text -> Maybe Value ->
                ReaderT (HM.HashMap RequestKey ApiResult) IO ()
succeedsWith cmd r = ask >>= liftIO . shouldMatch' (makeCheck cmd False r)

failsWith :: HasCallStack => Command Text -> Maybe Value ->
             ReaderT (HM.HashMap RequestKey ApiResult) IO ()
failsWith cmd r = ask >>= liftIO . shouldMatch' (makeCheck cmd True r)

runResults :: r -> ReaderT r m a -> m a
runResults rs act = runReaderT act rs

makeExecCmd :: SomeKeyPair -> String -> IO (Command Text)
makeExecCmd keyPairs code =
  mkExec code (object ["admin-keyset" .= [formatPubKeyForCmd keyPairs]]) def [keyPairs] Nothing


makeContCmd :: SomeKeyPair -> Bool -> Value -> Command Text -> Int -> String -> IO (Command Text)
makeContCmd keyPairs isRollback cmdData pactExecCmd step nonce =
  mkCont (getPactId pactExecCmd) step isRollback cmdData def [keyPairs] (Just nonce)


getPactId :: Command Text -> PactId
getPactId cmd = toPactId hsh
  where hsh = (toUntypedHash . _cmdHash) cmd


pactIdNotFoundMsg :: Command Text -> Maybe Value
pactIdNotFoundMsg cmd = (Just . String) escaped
  where txtPact = asString (getPactId cmd)
        escaped = ": Failure: resumePact: pact completed: " <> txtPact



---- TESTS -----

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
    let makeExecCmdWith = makeExecCmd adminKeys

    moduleCmd <- makeExecCmdWith (T.unpack (threeStepPactCode "nestedPact"))
    nestedExecPactCmd <- makeExecCmdWith ("(nestedPact.tester)" ++ " (nestedPact.tester)")
    allResults <- runAll mgr [moduleCmd, nestedExecPactCmd]

    runResults allResults $ do
      moduleCmd `succeedsWith`  Nothing
      nestedExecPactCmd `failsWith`
        (Just "(nestedPact.tester): Failure: Multiple or nested pact exec found")


-- CONTINUATIONS TESTS

testPactContinuation :: HTTP.Manager -> Spec
testPactContinuation mgr = before_ flushDb $ after_ flushDb $ do
  it "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (toJSON . CommandSuccess . PLiteral . LDecimal) 3
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
  return $ HM.lookup (cmdToRequestKey cmd) allResults


testCorrectNextStep :: HTTP.Manager -> Expectation
testCorrectNextStep mgr = do
  let moduleName = "testCorrectNextStep"
  adminKeys <- genKeys
  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith (T.unpack (threeStepPactCode moduleName))
  executePactCmd  <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  checkStateCmd   <- makeContCmdWith 1 "test4"
  allResults      <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    contNextStepCmd `succeedsWith` Just "step 1"
    checkStateCmd `failsWith`
      (Just ": Failure: resumePactExec: exec step mismatch with context: (1, 1)")



testIncorrectNextStep :: HTTP.Manager -> Expectation
testIncorrectNextStep mgr = do
  let moduleName = "testIncorrectNextStep"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd         <- makeExecCmdWith (T.unpack (threeStepPactCode moduleName))
  executePactCmd    <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  incorrectStepCmd  <- makeContCmdWith 2 "test3"
  checkStateCmd     <- makeContCmdWith 1 "test4"
  allResults        <- runAll mgr [moduleCmd, executePactCmd, incorrectStepCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    incorrectStepCmd `failsWith`
      (Just ": Failure: resumePactExec: exec step mismatch with context: (2, 0)")
    checkStateCmd `succeedsWith` Just "step 1"


testLastStep :: HTTP.Manager -> Expectation
testLastStep mgr = do
  let moduleName = "testLastStep"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (T.unpack (threeStepPactCode moduleName))
  executePactCmd   <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  contNextStep1Cmd <- makeContCmdWith 1 "test3"
  contNextStep2Cmd <- makeContCmdWith 2 "test4"
  checkStateCmd    <- makeContCmdWith 3 "test5"
  allResults       <- runAll mgr [moduleCmd, executePactCmd, contNextStep1Cmd,
                              contNextStep2Cmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    contNextStep1Cmd `succeedsWith` Just "step 1"
    contNextStep2Cmd `succeedsWith` Just "step 2"
    checkStateCmd `failsWith`
      pactIdNotFoundMsg executePactCmd



testErrStep :: HTTP.Manager -> Expectation
testErrStep mgr = do
  let moduleName = "testErrStep"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (T.unpack (errorStepPactCode moduleName))
  executePactCmd   <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  contErrStepCmd   <- makeContCmdWith 1 "test3"
  checkStateCmd    <- makeContCmdWith 2 "test4"
  allResults       <- runAll mgr [moduleCmd, executePactCmd, contErrStepCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    contErrStepCmd `failsWith`  Nothing
    checkStateCmd `failsWith`
      (Just ": Failure: resumePactExec: exec step mismatch with context: (2, 0)")



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
    it "outputs a rollback failure and doesn't change the pact's state" $
      testNoRollbackFunc mgr


testCorrectRollbackStep :: HTTP.Manager -> Expectation
testCorrectRollbackStep mgr = do
  let moduleName = "testCorrectRollbackStep"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith (T.unpack (pactWithRollbackCode moduleName))
  executePactCmd  <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True Null executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  rollbackStepCmd <- makeContCmdWithRollback 1 "test4" -- rollback = True
  checkStateCmd   <- makeContCmdWith 2 "test5"
  allResults      <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd,
                             rollbackStepCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    contNextStepCmd `succeedsWith` Just "step 1"
    rollbackStepCmd `succeedsWith` Just "rollback 1"
    checkStateCmd `failsWith`
      pactIdNotFoundMsg executePactCmd


testIncorrectRollbackStep :: HTTP.Manager -> Expectation
testIncorrectRollbackStep mgr = do
  let moduleName = "testIncorrectRollbackStep"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith (T.unpack (pactWithRollbackCode moduleName))
  executePactCmd  <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True Null executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  incorrectRbCmd  <- makeContCmdWithRollback 2 "test4"
  checkStateCmd   <- makeContCmdWith 2 "test5"
  allResults      <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd,
                             incorrectRbCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    contNextStepCmd `succeedsWith` Just "step 1"
    incorrectRbCmd `failsWith`
      (Just ": Failure: resumePactExec: rollback step mismatch with context: (2, 1)")
    checkStateCmd `succeedsWith` Just "step 2"


testRollbackErr :: HTTP.Manager -> Expectation
testRollbackErr mgr = do
  let moduleName = "testRollbackErr"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (T.unpack (pactWithRollbackErrCode moduleName))
  executePactCmd   <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True Null executePactCmd
  contNextStepCmd  <- makeContCmdWith 1 "test3"
  rollbackErrCmd   <- makeContCmdWithRollback 1 "test4"
  checkStateCmd    <- makeContCmdWith 2 "test5"
  allResults       <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd,
                              rollbackErrCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    contNextStepCmd `succeedsWith` Just "step 1"
    rollbackErrCmd `failsWith`  Nothing
    checkStateCmd `succeedsWith` Just "step 2"


testNoRollbackFunc :: HTTP.Manager -> Expectation
testNoRollbackFunc mgr = do
  let moduleName = "testNoRollbackFunc"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (T.unpack (threeStepPactCode moduleName))
  executePactCmd   <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True Null executePactCmd
  contNextStepCmd  <- makeContCmdWith 1 "test3"
  noRollbackCmd    <- makeContCmdWithRollback 1 "test4"
  checkStateCmd    <- makeContCmdWith 2 "test5"
  allResults       <- runAll mgr [moduleCmd, executePactCmd, contNextStepCmd,
                              noRollbackCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    contNextStepCmd `succeedsWith` Just "step 1"
    noRollbackCmd `failsWith` Just "(step \"step 1\")   : Failure: Rollback requested but none in step"
    checkStateCmd `succeedsWith` Just "step 2"



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

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd          <- makeExecCmdWith (T.unpack (pactWithYield moduleName))
  executePactCmd     <- makeExecCmdWith ("(" ++ moduleName ++ ".tester \"testing\")")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
                        -- pact takes an input
  resumeAndYieldCmd  <- makeContCmdWith 1 "test3"
  resumeOnlyCmd      <- makeContCmdWith 2 "test4"
  checkStateCmd      <- makeContCmdWith 3 "test5"
  allResults         <- runAll mgr [moduleCmd, executePactCmd, resumeAndYieldCmd,
                                resumeOnlyCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "testing->Step0"
    resumeAndYieldCmd `succeedsWith` Just "testing->Step0->Step1"
    resumeOnlyCmd `succeedsWith` Just "testing->Step0->Step1->Step2"
    checkStateCmd `failsWith`
      pactIdNotFoundMsg executePactCmd


testNoYield :: HTTP.Manager -> Expectation
testNoYield mgr = do
  let moduleName = "testNoYield"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd      <- makeExecCmdWith (T.unpack (pactWithYieldErr moduleName))
  executePactCmd <- makeExecCmdWith ("(" ++ moduleName ++ ".tester \"testing\")") -- pact takes an input

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  noYieldStepCmd <- makeContCmdWith 1 "test3"
  resumeErrCmd   <- makeContCmdWith 2 "test3"
  checkStateCmd  <- makeContCmdWith 1 "test5"
  allResults     <- runAll mgr [moduleCmd, executePactCmd, noYieldStepCmd,
                           resumeErrCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "testing->Step0"
    noYieldStepCmd `succeedsWith` Just "step 1 has no yield"
    resumeErrCmd `failsWith`  Nothing
    checkStateCmd `failsWith`
      (Just ": Failure: resumePactExec: exec step mismatch with context: (1, 1)")


testResetYield :: HTTP.Manager -> Expectation
testResetYield mgr = do
  let moduleName = "testResetYield"
  adminKeys <- genKeys

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (T.unpack (pactWithSameNameYield moduleName))
  executePactCmd   <- makeExecCmdWith ("(" ++ moduleName ++ ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  yieldSameKeyCmd  <- makeContCmdWith 1 "test3"
  resumeStepCmd    <- makeContCmdWith 2 "test4"
  checkStateCmd    <- makeContCmdWith 3 "test5"
  allResults       <- runAll mgr [moduleCmd, executePactCmd, yieldSameKeyCmd,
                              resumeStepCmd, checkStateCmd]

  runResults allResults $ do
    moduleCmd `succeedsWith`  Nothing
    executePactCmd `succeedsWith` Just "step 0"
    yieldSameKeyCmd `succeedsWith` Just "step 1"
    resumeStepCmd `succeedsWith` Just "step 1"
    checkStateCmd `failsWith`
      pactIdNotFoundMsg executePactCmd



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


twoPartyEscrow :: [Command T.Text] -> HTTP.Manager ->
                  ReaderT (HM.HashMap RequestKey ApiResult) IO () -> Expectation
twoPartyEscrow testCmds mgr act = do
  let setupPath = testDir ++ "cont-scripts/setup-"

  (_, sysModuleCmd)  <- mkApiReq (setupPath ++ "01-system.yaml")
  (_, acctModuleCmd) <- mkApiReq (setupPath ++ "02-accounts.yaml")
  (_, testModuleCmd) <- mkApiReq (setupPath ++ "03-test.yaml")
  (_, createAcctCmd) <- mkApiReq (setupPath ++ "04-create.yaml")
  (_, resetTimeCmd)  <- mkApiReq (setupPath ++ "05-reset.yaml")
  (_, runEscrowCmd)  <- mkApiReq (setupPath ++ "06-escrow.yaml")  -- When escrow pact executed
  (_, balanceCmd)    <- mkApiReq (setupPath ++ "07-balance.yaml")
  let allCmds = sysModuleCmd : acctModuleCmd : testModuleCmd : createAcctCmd
                : resetTimeCmd : runEscrowCmd : balanceCmd : testCmds
  allResults <- runAll mgr allCmds

  runResults allResults $ do
    sysModuleCmd `succeedsWith` Just "system module loaded"
    acctModuleCmd `succeedsWith` Just "TableCreated"
    testModuleCmd `succeedsWith` Just "test module loaded"
    createAcctCmd `succeedsWith`  Nothing -- Alice should be funded with $100
    resetTimeCmd `succeedsWith`  Nothing
    runEscrowCmd `succeedsWith`  Nothing
    balanceCmd `succeedsWith` decValue 98.00
    act


decValue :: Decimal -> Maybe Value
decValue = Just . toJSON . PLiteral . LDecimal

testDebtorPreTimeoutCancel :: HTTP.Manager -> Expectation
testDebtorPreTimeoutCancel mgr = do
  let testPath = testDir ++ "cont-scripts/fail-deb-cancel-"

  (_, tryCancelCmd)        <- mkApiReq (testPath ++ "01-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "02-balance.yaml")
  let allCmds = [tryCancelCmd, checkStillEscrowCmd]

  let cancelMsg = T.concat ["(enforce-one         \"Cancel c...: Failure:",
                            " Tx Failed: Cancel can only be effected by",
                            " creditor, or debitor after timeout"]
  twoPartyEscrow allCmds mgr $ do
    tryCancelCmd `failsWith` Just (String cancelMsg)
    checkStillEscrowCmd `succeedsWith` decValue 98.00


testDebtorPostTimeoutCancel :: HTTP.Manager -> Expectation
testDebtorPostTimeoutCancel mgr = do
  let testPath = testDir ++ "cont-scripts/pass-deb-cancel-"

  (_, setTimeCmd)          <- mkApiReq (testPath ++ "01-set-time.yaml")
  (_, tryCancelCmd)        <- mkApiReq (testPath ++ "02-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "03-balance.yaml")
  let allCmds = [setTimeCmd, tryCancelCmd, checkStillEscrowCmd]

  twoPartyEscrow allCmds mgr $ do
    setTimeCmd `succeedsWith`  Nothing
    tryCancelCmd `succeedsWith`  Nothing
    checkStillEscrowCmd `succeedsWith` decValue 100.00


testCreditorCancel :: HTTP.Manager -> Expectation
testCreditorCancel mgr = do
  let testPath = testDir ++ "cont-scripts/pass-cred-cancel-"

  (_, resetTimeCmd)        <- mkApiReq (testPath ++ "01-reset.yaml")
  (_, credCancelCmd)       <- mkApiReq (testPath ++ "02-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "03-balance.yaml")
  let allCmds = [resetTimeCmd, credCancelCmd, checkStillEscrowCmd]

  twoPartyEscrow allCmds mgr $ do
    resetTimeCmd `succeedsWith`  Nothing
    credCancelCmd `succeedsWith`  Nothing
    checkStillEscrowCmd `succeedsWith` decValue 100.00


testFinishAlone :: HTTP.Manager -> Expectation
testFinishAlone mgr = do
  let testPathCred  = testDir ++ "cont-scripts/fail-cred-finish-"
      testPathDeb   = testDir ++ "cont-scripts/fail-deb-finish-"

  (_, tryCredAloneCmd) <- mkApiReq (testPathCred ++ "01-cont.yaml")
  (_, tryDebAloneCmd)  <- mkApiReq (testPathDeb ++ "01-cont.yaml")
  let allCmds = [tryCredAloneCmd, tryDebAloneCmd]

  twoPartyEscrow allCmds mgr $ do
    tryCredAloneCmd `failsWith`
                          (Just "(enforce-guard g): Failure: Tx Failed: Keyset failure (keys-all)")
    tryDebAloneCmd `failsWith`
                          (Just "(enforce-guard g): Failure: Tx Failed: Keyset failure (keys-all)")


testPriceNegUp :: HTTP.Manager -> Expectation
testPriceNegUp mgr = do
  let testPath = testDir ++ "cont-scripts/fail-both-price-up-"

  (_, tryNegUpCmd) <- mkApiReq (testPath ++ "01-cont.yaml")
  twoPartyEscrow [tryNegUpCmd] mgr $ do
    tryNegUpCmd `failsWith`
                      (Just "(enforce (>= escrow-amount pri...: Failure: Tx Failed: Price cannot negotiate up")


testValidEscrowFinish :: HTTP.Manager -> Expectation
testValidEscrowFinish mgr = do
  let testPath = testDir ++ "cont-scripts/pass-both-price-down-"

  (_, tryNegDownCmd)  <- mkApiReq (testPath ++ "01-cont.yaml")
  (_, credBalanceCmd) <- mkApiReq (testPath ++ "02-cred-balance.yaml")
  (_, debBalanceCmd)  <- mkApiReq (testPath ++ "03-deb-balance.yaml")
  let allCmds = [tryNegDownCmd, credBalanceCmd, debBalanceCmd]

  twoPartyEscrow allCmds mgr $ do
    tryNegDownCmd `succeedsWith`
                         (Just "Escrow completed with 1.75 paid and 0.25 refunded")
    credBalanceCmd `succeedsWith` decValue 1.75
    debBalanceCmd `succeedsWith` decValue 98.25
