{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PactContinuationSpec (spec) where

import qualified Control.Exception as Exception
import Control.Lens hiding ((.=))
import Control.Monad (forM_)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.ByteString.Short as SBS
import Data.Decimal
import Data.Default (def)
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf)
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text, unpack)
import qualified Data.Text as T
import NeatInterpolation (text)
import Prelude hiding (concat)
import Servant.Client
import System.Environment (withArgs)
import System.Timeout
import qualified Data.Vector as V

import Test.Hspec

import Pact.ApiReq
import Pact.Server.API
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Crypto as Crypto
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Pretty
import Pact.Types.Runtime
import Pact.Types.SPV
import qualified Pact.JSON.Encode as J

import Utils

---- TESTS -----

spec :: Spec
spec = describe "pacts in dev server" $ do
  describe "testPactContinuation" testPactContinuation
  describe "testPactRollback" testPactRollback
  describe "testPactYield" testPactYield
  describe "testTwoPartyEscrow" $ sequential testTwoPartyEscrow
  describe "testOldNestedPacts" testOldNestedPacts
  describe "testManagedCaps" testManagedCaps
  describe "testElideModRefEvents" testElideModRefEvents
  describe "testNestedPactContinuation" testNestedPactContinuation
  describe "testNestedPactYield" testNestedPactYield

testElideModRefEvents :: Spec
testElideModRefEvents = do
  it "elides modref infos" $ do
    cmd <- mkExec code Null def [] [] Nothing Nothing
    results <- runAll' [cmd] noSPVSupport testFlags
    runResults results $ do
      shouldMatch cmd $ ExpectResult $ \cr ->
        J.encode (J.Array (_crEvents cr)) `shouldSatisfy`
          (not . ("refInfo" `isInfixOf`) . BSL8.unpack)

  it "doesn't elide on backcompat" $ do
    cmd <- mkExec codePreFork Null def [] [] Nothing Nothing
    results <- runAll' [cmd] noSPVSupport backCompatFlags
    runResults results $ do
      shouldMatch cmd $ ExpectResult $ \cr ->
        J.encode (J.Array (_crEvents cr)) `shouldSatisfy`
          (("refInfo" `isInfixOf`) . BSL8.unpack)
  where
    codePreFork =
      [text|

           (interface iface
             (defun f:bool ()))

           (module evmodule G

             (defcap G () true)

             (defcap BURN (a:module{iface})
               @event
               1)

             (implements iface)

             (defun f:bool () true)

             (defun usecap (a:module{iface})
               (with-capability (BURN a)
                 1
               )
             )
            )

           (usecap evmodule)
           |]
    code =
      [text|

           (interface iface
             (defun f:bool ()))

           (module evmodule G

             (defcap G () true)

             (implements iface)

             (defun f:bool () true)

             (defcap EVENT (mod:module{iface})
               @event true)

             (defun emit(mod:module{iface})
               (emit-event (EVENT mod))))

           (evmodule.emit evmodule)
           |]


mkModuleHash :: Text -> IO ModuleHash
mkModuleHash =
  either (fail . show) (return . ModuleHash . Hash . SBS.toShort) . parseB64UrlUnpaddedText'

testManagedCaps :: Spec
testManagedCaps = do
  it "exercises managed PAY cap" $ do
    let setupPath = testDir ++ "cont-scripts/setup-"
        testPath = testDir ++ "cont-scripts/managed-"

    (_, sysModuleCmd)  <- mkApiReq (setupPath ++ "01-system.yaml")
    (_, acctModuleCmd) <- mkApiReq (setupPath ++ "02-accounts.yaml")
    (_, createAcctCmd) <- mkApiReq (setupPath ++ "04-create.yaml")
    (_, managedPay) <- mkApiReq (testPath ++ "01-pay.yaml")
    (_, managedPayFails) <- mkApiReq (testPath ++ "02-pay-fails.yaml")
    let allCmds = [sysModuleCmd,acctModuleCmd,createAcctCmd,managedPay,managedPayFails]
    allResults <- runAll allCmds

    mhash <- mkModuleHash "HniQBJ-NUJan20k4t6MiqpzhqkSsKmIzN5ef76pcLCU"

    runResults allResults $ do
      sysModuleCmd `succeedsWith` (`shouldBe` textVal "system module loaded")
      acctModuleCmd `succeedsWith` (`shouldBe` textVal "TableCreated")
      succeeds createAcctCmd
      managedPay `succeedsWith'`
        (`shouldBe` (textVal "Transfer succeeded",
         [PactEvent "PAY"
          [textVal "Alice",textVal "Bob",decValue 0.9]
          "accounts"
          mhash]))
      managedPayFails `failsWith` (`shouldBe` "insufficient balance")


-- | allows passing e.g. "-m CrossChain" to match only `testCrossChainYield` in ghci
_runArgs :: String -> IO ()
_runArgs args = withArgs (words args) $ hspec spec

testOldNestedPacts :: Spec
testOldNestedPacts = do
  it "throws error when multiple defpact executions occur in same transaction" $ do
    adminKeys <- DynEd25519KeyPair <$> genKeyPair
    let makeExecCmdWith = makeExecCmd adminKeys

    moduleCmd <- makeExecCmdWith (threeStepPactCode "nestedPact")
    nestedExecPactCmd <- makeExecCmdWith ("(nestedPact.tester)" <> " (nestedPact.tester)")
    allResults <- runAll [moduleCmd, nestedExecPactCmd]

    runResults allResults $ do
      succeeds moduleCmd
      nestedExecPactCmd `failsWith` (`shouldBe` "Multiple or nested pact exec found")


-- CONTINUATIONS TESTS

testPactContinuation :: Spec
testPactContinuation = do
  it "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (PactResult . Right . PLiteral . LDecimal) 3
        --expRes = Just $ CommandResult _ ((Just . TxId) 0) cmdData (Gas 0)
    cr <- testSimpleServerCmd
    (_crResult <$> cr)`shouldBe` Just cmdData

  context "when provided with correct next step" $
    it "executes the next step and updates pact's state" $ do
      let mname1 = "testCorrectNextStep"
      testCorrectNextStep (threeStepPactCode mname1) ("(" <> mname1 <> ".tester)") testFlags

  context "when provided with incorrect next step" $
    it "throws error and does not update pact's state" $ do
      let mname2 = "testIncorrectNextStep"
      testIncorrectNextStep (threeStepPactCode mname2) ("(" <> mname2 <> ".tester)") testFlags

  context "when last step of a pact executed" $
    it "deletes pact from the state" $ do
      let mname3 = "testLastStep"
      testLastStep (threeStepPactCode mname3) ("(" <> mname3 <> ".tester)") testFlags

  context "when error occurs when executing pact step" $
    it "throws error and does not update pact's state" $ do
      let mname4 = "testErrStep"
      testErrStep (errorStepPactCode mname4) ("(" <> mname4 <> ".tester)") testFlags

testNestedPactContinuation :: Spec
testNestedPactContinuation = do
  it "sends (+ 1 2) command to locally running dev server" $ do
    let cmdData = (PactResult . Right . PLiteral . LDecimal) 3
    cr <- testSimpleServerCmd
    (_crResult <$> cr)`shouldBe` Just cmdData

  context "when provided with correct next step" $
    it "executes the next step and updates nested pact's state" $ do
    let mname1 = "testCorrectNextNestedStep"
    testCorrectNextStep (threeStepNestedPactCode mname1) ("(" <> mname1 <> "-nested.nestedTester " <> mname1 <> "-2)") nestedDefPactFlags

  context "when provided with incorrect next step" $
    it "throws error and does not update nested pact's state" $ do
      let mname2 = "testIncorrectNextNestedStep"
      testIncorrectNextStep (threeStepNestedPactCode mname2) ("(" <> mname2 <> "-nested.nestedTester " <> mname2 <> "-2)") nestedDefPactFlags
  context "when last step of a pact executed" $
    it "deletes pact from the state" $ do
      let mname3 = "testNestedLastStep"
      testLastStep (threeStepNestedPactCode mname3) ("(" <> mname3 <> "-nested.nestedTester " <> mname3 <> "-2)") nestedDefPactFlags

  context "when error occurs when executing pact step" $
    it "throws error and does not update pact's state" $ do
      let mname4 = "testNestedErrStep"
      testErrStep (errorStepNestedPactCode mname4) ("(" <> mname4 <> "-nested.nestedTester)") nestedDefPactFlags

testSimpleServerCmd :: IO (Maybe (CommandResult Hash))
testSimpleServerCmd = do
  simpleKeys <- DynEd25519KeyPair <$> genKeyPair
  cmd <- mkExec  "(+ 1 2)" Null def [(simpleKeys,[])] [] Nothing (Just "test1")
  allResults <- runAll [cmd]
  return $ HM.lookup (cmdToRequestKey cmd) allResults


testCorrectNextStep :: Text -> Text -> [ExecutionFlag] -> Expectation
testCorrectNextStep code command flags = do
  adminKeys <- DynEd25519KeyPair <$> genKeyPair
  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith code
  executePactCmd  <- makeExecCmdWith command

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  checkStateCmd   <- makeContCmdWith 1 "test4"
  allResults      <- runAll' [moduleCmd, executePactCmd, contNextStepCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    checkStateCmd `failsWith` (`shouldBe` stepMisMatchMsg False 1 1)


threeStepPactCode :: T.Text -> T.Text
threeStepPactCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
      (module $moduleName 'k
        (defpact tester ()
          (step "step 0")
          (step "step 1")
          (step "step 2"))) |]

threeStepNestedPactCode :: T.Text -> T.Text
threeStepNestedPactCode moduleName =
  [text|
    (interface iface

     (defpact ndp:string ())
    )
    (module $moduleName g
     (defcap g () true)
     (defpact tester ()
       (step "step 0")
       (step "step 1")
       (step "step 2")))
     (module $moduleName-2 g
     (defcap g () true)
     (implements iface)
     (defpact ndp:string ()
       (step
         (let
           ((unused 1))
           ($moduleName.tester)
           "step 0")
       )
       (step
         (let
           ((unused 1))
           (continue ($moduleName.tester))
           "step 1")
       )
       (step
         (let
           ((unused 1))
           (continue ($moduleName.tester))
           "step 2")
       ))
       )

     (module $moduleName-nested g
       (defcap g () true)
       (defpact nestedTester (m:module{iface})
         (step
         (let
           ((unused 1))
           (m::ndp)
           "step 0")
       )
       (step
         (let
           ((unused 1))
           (continue (m::ndp))
           "step 1")
       )
       (step
         (let
           ((unused 1))
           (continue (m::ndp))
           "step 2")
       )
       ))
       |]


testIncorrectNextStep :: Text -> Text -> [ExecutionFlag] -> Expectation
testIncorrectNextStep code command flags = do
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd         <- makeExecCmdWith code
  executePactCmd    <- makeExecCmdWith command

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  incorrectStepCmd  <- makeContCmdWith 2 "test3"
  checkStateCmd     <- makeContCmdWith 1 "test4"
  allResults        <- runAll' [moduleCmd, executePactCmd, incorrectStepCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    incorrectStepCmd `failsWith` (`shouldBe` stepMisMatchMsg False 2 0)
    checkStateCmd `succeedsWith` (`shouldBe` textVal "step 1")


testLastStep :: Text -> Text -> [ExecutionFlag] -> Expectation
testLastStep code command flags = do
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith code
  executePactCmd   <- makeExecCmdWith command

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  contNextStep1Cmd <- makeContCmdWith 1 "test3"
  contNextStep2Cmd <- makeContCmdWith 2 "test4"
  checkStateCmd    <- makeContCmdWith 3 "test5"
  allResults       <- runAll' [moduleCmd, executePactCmd, contNextStep1Cmd,
                              contNextStep2Cmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStep1Cmd `succeedsWith` (`shouldBe` textVal "step 1")
    contNextStep2Cmd `succeedsWith` (`shouldBe` textVal "step 2")
    checkStateCmd `failsWith`
      (`shouldBe` pactIdNotFoundMsg executePactCmd)



testErrStep :: Text -> Text -> [ExecutionFlag] -> Expectation
testErrStep code command flags = do
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith code
  executePactCmd   <- makeExecCmdWith command

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  contErrStepCmd   <- makeContCmdWith 1 "test3"
  checkStateCmd    <- makeContCmdWith 2 "test4"
  allResults       <- runAll' [moduleCmd, executePactCmd, contErrStepCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    fails contErrStepCmd
    checkStateCmd `failsWith` (`shouldBe` stepMisMatchMsg False 2 0)


errorStepPactCode :: T.Text -> T.Text
errorStepPactCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
    (module $moduleName 'k
      (defpact tester ()
        (step "step 0")
        (step (+ "will throw error in step 1"))
    (step "step 2")))|]

errorStepNestedPactCode :: T.Text -> T.Text
errorStepNestedPactCode moduleName =
  [text|
      (module $moduleName g
       (defcap g () true)
       (defpact tester ()
         (step "step 0")
         (step (+ "will throw error in step 1"))
         (step "step 2")))
       (module $moduleName-nested g
         (defcap g () true)
         (defpact nestedTester ()
           (step
           (let
             ((unused 1))
             ($moduleName.tester)
             "step 0")
         )
         (step
           (let
             ((unused 1))
             (continue ($moduleName.tester))
             "step 1")
         )
         (step
           (let
             ((unused 1))
             (continue ($moduleName.tester))
             "step 2")
         )
         ))
                |]


-- ROLLBACK TESTS

testPactRollback :: Spec
testPactRollback = do
  context "when provided with correct rollback step" $
    it "executes the rollback function and deletes pact from state"
      testCorrectRollbackStep

  context "when provided with incorrect rollback step" $
    it "throws error and does not delete pact from state"
      testIncorrectRollbackStep

  context "when error occurs when executing rollback function" $
    it "throws error and does not delete pact from state"
      testRollbackErr

  context "when trying to rollback a step without a rollback function" $
    it "outputs a rollback failure and doesn't change the pact's state"
      testNoRollbackFunc


testCorrectRollbackStep :: Expectation
testCorrectRollbackStep = do
  let moduleName = "testCorrectRollbackStep"
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith (pactWithRollbackCode moduleName)
  executePactCmd  <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True Null executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  rollbackStepCmd <- makeContCmdWithRollback 1 "test4" -- rollback = True
  checkStateCmd   <- makeContCmdWith 2 "test5"
  allResults      <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                             rollbackStepCmd, checkStateCmd]

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    rollbackStepCmd `succeedsWith` (`shouldBe` textVal "rollback 1")
    checkStateCmd `failsWith`
      (`shouldBe` pactIdNotFoundMsg executePactCmd)



pactWithRollbackCode :: T.Text -> T.Text
pactWithRollbackCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
    (module $moduleName 'k
      (defpact tester ()
        (step-with-rollback "step 0" "rollback 0")
        (step-with-rollback "step 1" "rollback 1")
        (step               "step 2")))
        |]


testIncorrectRollbackStep :: Expectation
testIncorrectRollbackStep = do
  let moduleName = "testIncorrectRollbackStep"
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd       <- makeExecCmdWith (pactWithRollbackCode moduleName)
  executePactCmd  <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True Null executePactCmd
  contNextStepCmd <- makeContCmdWith 1 "test3"
  incorrectRbCmd  <- makeContCmdWithRollback 2 "test4"
  checkStateCmd   <- makeContCmdWith 2 "test5"
  allResults      <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                             incorrectRbCmd, checkStateCmd]

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    incorrectRbCmd `failsWith` (`shouldBe` stepMisMatchMsg True 2 1)
    checkStateCmd `succeedsWith` (`shouldBe` textVal "step 2")


testRollbackErr :: Expectation
testRollbackErr = do
  let moduleName = "testRollbackErr"
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (pactWithRollbackErrCode moduleName)
  executePactCmd   <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True Null executePactCmd
  contNextStepCmd  <- makeContCmdWith 1 "test3"
  rollbackErrCmd   <- makeContCmdWithRollback 1 "test4"
  checkStateCmd    <- makeContCmdWith 2 "test5"
  allResults       <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                              rollbackErrCmd, checkStateCmd]

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    fails rollbackErrCmd
    checkStateCmd `succeedsWith` (`shouldBe` textVal "step 2")


pactWithRollbackErrCode :: T.Text -> T.Text
pactWithRollbackErrCode moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
    (module $moduleName 'k
      (defpact tester ()
        (step-with-rollback "step 0" "rollback 0")
        (step-with-rollback "step 1" (+ "will throw error in rollback 1"))
        (step               "step 2")))
        |]


testNoRollbackFunc :: Expectation
testNoRollbackFunc = do
  let moduleName = "testNoRollbackFunc"
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (threeStepPactCode moduleName)
  executePactCmd   <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
      makeContCmdWithRollback = makeContCmd adminKeys True Null executePactCmd
  contNextStepCmd  <- makeContCmdWith 1 "test3"
  noRollbackCmd    <- makeContCmdWithRollback 1 "test4"
  checkStateCmd    <- makeContCmdWith 2 "test5"
  allResults       <- runAll [moduleCmd, executePactCmd, contNextStepCmd,
                              noRollbackCmd, checkStateCmd]

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    contNextStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    noRollbackCmd `failsWith` (`shouldBe` "Rollback requested but none in step")
    checkStateCmd `succeedsWith` (`shouldBe` textVal "step 2")



-- YIELD / RESUME TESTS

testPactYield :: Spec
testPactYield = do
  context "when previous step yields value" $
    it "resumes value" $ do
      let mname1 = "testValidYield"
      testValidYield mname1 pactWithYield testFlags

  context "when previous step does not yield value" $
    it "throws error when trying to resume, and does not delete pact from state" $ do
      let mname2 = "testNoYield"
      testNoYield mname2 pactWithYieldErr testFlags

  it "resets yielded values after each step" $ do
    let mname3 = "testResetYield"
    testResetYield mname3 pactWithSameNameYield testFlags

  it "testCrossChainYield:succeeds with same module" $
      testCrossChainYield "" Nothing mkFakeSPV False testFlags

  it "testCrossChainYield:succeeds with back compat" $
      testCrossChainYield "" Nothing mkFakeSPV True testFlags

  it "testCrossChainYield:fails with different module" $
      testCrossChainYield ";;1"
        (Just ((`shouldContain` "enforceYield: yield provenance") . show . peDoc))
        mkFakeSPV False testFlags

  it "testCrossChainYield:succeeds with blessed module" $
      testCrossChainYield "(bless \"_9xPxvYomOU0iEqXpcrChvoA-E9qoaE1TqU460xN1xc\")" Nothing mkFakeSPV False testFlags

  it "testCrossChainYield:fails with a userError pre-fork" $
      testCrossChainYield "(bless \"_9xPxvYomOU0iEqXpcrChvoA-E9qoaE1TqU460xN1xc\")"
        (Just $ \e -> do
          peType e `shouldBe` EvalError
          peDoc e `shouldBe` "user error (\"Cross-chain continuations not supported\")"
          )
        (const noSPVSupport) False (FlagDisablePact47 : testFlags)

  it "testCrossChainYield:fails with a ContinuationError post-fork" $
      testCrossChainYield "(bless \"_9xPxvYomOU0iEqXpcrChvoA-E9qoaE1TqU460xN1xc\")"
        (Just $ \e -> do
          peType e `shouldBe` ContinuationError
          peDoc e `shouldBe` "Cross-chain continuations not supported"
          )
        (const noSPVSupport) False testFlags

testNestedPactYield :: Spec
testNestedPactYield = do
  context "when previous step yields value" $
    it "resumes value" $ do
      let mname1 = "testNestedValidYield"
      testValidYield mname1 nestedPactWithYield nestedDefPactFlags

  context "when previous step does not yield value" $
    it "throws error when trying to resume, and does not delete pact from state" $ do
      let mname2 = "testNestedNoYield"
      testNoYield mname2 nestedPactWithYieldErr nestedDefPactFlags

  it "resets yielded values after each step" $ do
    let mname3 = "testNestedResetYield"
    testResetYield mname3 nestedPactWithSameNameYield nestedDefPactFlags

  it "testCrossChainYield:succeeds with same module"
      testNestedCrossChainYield
  where
  testNestedCrossChainYield = step0
    where
    -- STEP 0: runs on server for "chain0results"
    -- note we're not changing server ID, just starting with
    -- a fresh server to prove that a new pact coming through
    -- SPV can start from step 1.
    step0 = do
      adminKeys <- DynEd25519KeyPair <$> genKeyPair

      let makeExecCmdWith = makeExecCmd' (Just "xchain") adminKeys
      moduleCmd        <- makeExecCmdWith nestedPactCrossChainYield
      executePactCmd   <- makeExecCmdWith "(cross-chain-tester.cross-chain \"jose\")"

      chain0Results <-
        runAll' [moduleCmd,executePactCmd] noSPVSupport nestedDefPactFlags

      mhash <- mkModuleHash "mGbCL-I0xXho_dxYfYAVmHfSfj3o43gbJ3ZgLHpaq14"

      runResults chain0Results $ do
        succeeds moduleCmd
        executePactCmd `succeedsWith'`
            (`shouldBe` (textVal "jose->A",
                    [PactEvent
                     "X_YIELD"
                     [ textVal ""
                     , textVal "cross-chain-tester.cross-chain"
                     , PList $ V.fromList [ textVal "jose" ]]
                     "pact"
                     mhash]))
        shouldMatch executePactCmd $ ExpectResult $ \cr ->
          preview (crContinuation . _Just . peYield . _Just . ySourceChain . _Just) cr
          `shouldBe`
          (Just (ChainId ""))

      let rk = cmdToRequestKey executePactCmd

      case HM.lookup rk chain0Results of
        Nothing -> expectationFailure $
          "Could not find result " ++ show rk ++ ": " ++ show chain0Results
        Just cr -> case _crContinuation cr of
          Nothing -> expectationFailure $
            "No continuation in result: " ++ show rk
          Just pe -> do
            step1 adminKeys executePactCmd moduleCmd pe mhash

    -- STEP 1: found the pact exec from step 0; return this
    -- from the SPV operation. Run a fresh server, reload
    -- the module, and run the step.
    step1 adminKeys executePactCmd moduleCmd pe mhash = do

      let proof = (ContProof "hi there")
          makeContCmdWith = makeContCmd' (Just proof) adminKeys False Null executePactCmd
          spv = noSPVSupport {
            _spvVerifyContinuation = \cp ->
                if cp == proof then
                  return $ Right pe
                else
                  return $ Left "Invalid proof"
            }

      chain1Cont <- makeContCmdWith 1 "chain1Cont"
      chain1ContDupe <- makeContCmdWith 1 "chain1ContDupe"

      chain1Results <-
        runAll' [moduleCmd, chain1Cont,chain1ContDupe] spv nestedDefPactFlags
      let completedPactMsg =
            "resumePact: pact completed: " ++ showPretty (_cmdHash executePactCmd)

      runResults chain1Results $ do
        succeeds moduleCmd
        chain1Cont `succeedsWith'`
          (`shouldBe` (textVal "jose->A->B",
                  [PactEvent
                    "X_RESUME"
                    [ textVal ""
                    , textVal "cross-chain-tester.cross-chain"
                    , PList $ V.fromList [ textVal "jose" ]]
                    "pact"
                    mhash]))
        chain1ContDupe `failsWith` (`shouldBe` completedPactMsg)


testValidYield :: Text -> (Text -> Text) -> [ExecutionFlag] -> Expectation
testValidYield moduleName mkCode flags = do
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd          <- makeExecCmdWith (mkCode moduleName)
  executePactCmd     <- makeExecCmdWith ("(" <> moduleName <> ".tester \"testing\")")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
                        -- pact takes an input
  resumeAndYieldCmd  <- makeContCmdWith 1 "test3"
  resumeOnlyCmd      <- makeContCmdWith 2 "test4"
  checkStateCmd      <- makeContCmdWith 3 "test5"
  allResults         <- runAll' [moduleCmd, executePactCmd, resumeAndYieldCmd,
                                resumeOnlyCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "testing->Step0")
    resumeAndYieldCmd `succeedsWith` (`shouldBe` textVal "testing->Step0->Step1")
    resumeOnlyCmd `succeedsWith` (`shouldBe` textVal "testing->Step0->Step1->Step2")
    checkStateCmd `failsWith`
      (`shouldBe` pactIdNotFoundMsg executePactCmd)


pactWithYield :: T.Text -> T.Text
pactWithYield moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
    (module $moduleName 'k
      (defpact tester (name)
        (step
          (let ((result0 (+ name "->Step0")))
            (yield { "step0-result": result0})
            result0))
        (step
          (resume {"step0-result" := res0 }
          (let ((result1 (+ res0 "->Step1")))
            (yield {"step1-result": result1})
            result1)))
        (step
          (resume { "step1-result" := res1 }
             (+ res1 "->Step2")))))|]

nestedPactWithYield :: T.Text -> T.Text
nestedPactWithYield moduleName =
  [text|
    (module nested-$moduleName g
      (defcap g () true)
      (defpact tester (name)
        (step
          (let ((result0 (+ name "->Step0")))
            (yield { "step0-result": result0})
            result0))
        (step
          (resume {"step0-result" := res0 }
          (let ((result1 (+ res0 "->Step1")))
            (yield {"step1-result": result1})
            result1)))
        (step
          (resume { "step1-result" := res1 }
             (+ res1 "->Step2")))))
     (module $moduleName g
      (defcap g () true)
      (defpact tester (name)
        (step
          (let ((result0 (+ name "->Step0")))
            (nested-$moduleName.tester name)
            (yield { "step0-result": result0})
            result0))
        (step
          (resume {"step0-result" := res0 }
          (let ((result1 (+ res0 "->Step1")))
            (continue (nested-$moduleName.tester name))
            (yield {"step1-result": result1})
            result1)))
        (step
          (resume { "step1-result" := res1 }
             (continue (nested-$moduleName.tester name))
             (+ res1 "->Step2")))))
             |]



testNoYield :: Text -> (Text -> Text) -> [ExecutionFlag] -> Expectation
testNoYield moduleName mkCode flags = do
  -- let moduleName = "testNoYield"
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd      <- makeExecCmdWith (mkCode moduleName)
  executePactCmd <- makeExecCmdWith ("(" <> moduleName <> ".tester \"testing\")") -- pact takes an input

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  noYieldStepCmd <- makeContCmdWith 1 "test3"
  resumeErrCmd   <- makeContCmdWith 2 "test3"
  checkStateCmd  <- makeContCmdWith 1 "test5"
  allResults     <- runAll' [moduleCmd, executePactCmd, noYieldStepCmd,
                           resumeErrCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "testing->Step0")
    noYieldStepCmd `succeedsWith` (`shouldBe` textVal "step 1 has no yield")
    fails resumeErrCmd
    checkStateCmd `failsWith` (`shouldBe` stepMisMatchMsg False 1 1)


pactWithYieldErr :: T.Text -> T.Text
pactWithYieldErr moduleName =
  [text| (define-keyset 'k (read-keyset "admin-keyset"))
             (module $moduleName 'k
               (defpact tester (name)
                 (step
                   (let ((result0 (+ name "->Step0")))
                    (yield { "step0-result": result0 })
                    result0))
                 (step "step 1 has no yield")
                 (step
                   (resume { "step0-result" := res0 }
                      (+ res0 "->Step2")))))|]

nestedPactWithYieldErr :: T.Text -> T.Text
nestedPactWithYieldErr moduleName =
  [text|
    (module nested-$moduleName g
      (defcap g () true)
      (defpact tester (name)
       (step
         (let ((result0 (+ name "->Step0")))
          (yield { "step0-result": result0 })
          result0))
       (step "step 1 has no yield")
       (step
         (resume { "step0-result" := res0 }
            (+ res0 "->Step2")))))
    (module $moduleName g
     (defcap g () true)
     (defpact tester (name)
       (step
         (let ((result0 (+ name "->Step0")))
          (nested-$moduleName.tester name)
          (yield { "step0-result": result0 })
            result0))
       (step
       (let ((unused 1))
       (continue (nested-$moduleName.tester name))
       "step 1 has no yield"
       ))
       (step
         (resume { "step0-result" := res0 }
            (continue (nested-$moduleName.tester name))
            (+ res0 "->Step2")))))
            |]


testResetYield :: Text -> (Text -> Text) -> [ExecutionFlag] -> Expectation
testResetYield moduleName mkCode flags = do
  -- let moduleName = "testResetYield"
  adminKeys <- DynEd25519KeyPair <$> genKeyPair

  let makeExecCmdWith = makeExecCmd adminKeys
  moduleCmd        <- makeExecCmdWith (mkCode moduleName)
  executePactCmd   <- makeExecCmdWith ("(" <> moduleName <> ".tester)")

  let makeContCmdWith = makeContCmd adminKeys False Null executePactCmd
  yieldSameKeyCmd  <- makeContCmdWith 1 "test3"
  resumeStepCmd    <- makeContCmdWith 2 "test4"
  checkStateCmd    <- makeContCmdWith 3 "test5"
  allResults       <- runAll' [moduleCmd, executePactCmd, yieldSameKeyCmd,
                              resumeStepCmd, checkStateCmd] noSPVSupport flags

  runResults allResults $ do
    succeeds moduleCmd
    executePactCmd `succeedsWith` (`shouldBe` textVal "step 0")
    yieldSameKeyCmd `succeedsWith` (`shouldBe` textVal "step 1")
    resumeStepCmd `succeedsWith` (`shouldBe` textVal "step 1")
    checkStateCmd `failsWith`
      (`shouldBe` pactIdNotFoundMsg executePactCmd)



pactWithSameNameYield :: T.Text -> T.Text
pactWithSameNameYield moduleName =
  [text|
    (define-keyset 'k (read-keyset "admin-keyset"))
      (module $moduleName 'k
        (defpact tester ()
          (step
            (let ((result0 "step 0"))
             (yield { "result": result0 })
             result0))
          (step
            (let ((result1 "step 1"))
             (yield { "result": result1 })
             result1))
          (step
            (resume { "result" := res }
              res))))|]


nestedPactWithSameNameYield :: T.Text -> T.Text
nestedPactWithSameNameYield moduleName =
  [text|
    (module nested-$moduleName g
      (defcap g () true)
      (defpact tester ()
        (step
          (let ((result0 "step 0"))
           (yield { "result": result0 })
           result0))
        (step
          (let ((result1 "step 1"))
           (yield { "result": result1 })
           result1))
        (step
          (resume { "result" := res }
            res))))
     (module $moduleName g
      (defcap g () true)
      (defpact tester ()
        (step
          (let ((result0 "step 0"))
           (nested-$moduleName.tester)
           (yield { "result": result0 })
           result0))
        (step
          (let ((result1 "step 1"))
           (continue (nested-$moduleName.tester))
           (yield { "result": result1 })
           result1))
        (step
          (resume { "result" := res }
            (enforce (= (continue (nested-$moduleName.tester)) "step 1") "failure")
            res))))
            |]

fakeProof :: ContProof
fakeProof = ContProof "hi there"

mkFakeSPV :: PactExec -> SPVSupport
mkFakeSPV pe =
  noSPVSupport {
    _spvVerifyContinuation = \cp ->
      if cp == fakeProof then
        return $ Right pe
      else
        return $ Left "Invalid proof"
  }

testCrossChainYield :: T.Text -> Maybe (PactError -> Expectation) -> (PactExec -> SPVSupport) -> Bool -> [ExecutionFlag] -> Expectation
testCrossChainYield blessCode expectFailure mkSpvSupport backCompat spvFlags = step0
  where

    -- STEP 0: runs on server for "chain0results"
    -- note we're not changing server ID, just starting with
    -- a fresh server to prove that a new pact coming through
    -- SPV can start from step 1.
    step0 = do
      adminKeys <- DynEd25519KeyPair <$> genKeyPair

      let makeExecCmdWith = makeExecCmd' (Just "xchain") adminKeys
      moduleCmd        <- makeExecCmdWith (pactCrossChainYield "")
      moduleCmd'       <- makeExecCmdWith (pactCrossChainYield blessCode)
      executePactCmd   <- makeExecCmdWith "(cross-chain-tester.cross-chain \"emily\")"

      chain0Results <-
        runAll' [moduleCmd,executePactCmd] noSPVSupport $
        if backCompat then backCompatFlags else testFlags

      mhash <- mkModuleHash "_9xPxvYomOU0iEqXpcrChvoA-E9qoaE1TqU460xN1xc"

      runResults chain0Results $ do
        succeeds moduleCmd
        executePactCmd `succeedsWith'`
            (`shouldBe` (textVal "emily->A",
                  if backCompat then [] else
                    [PactEvent
                     "X_YIELD"
                     [ textVal ""
                     , textVal "cross-chain-tester.cross-chain"
                     , PList $ V.fromList [ textVal "emily" ]]
                     "pact"
                     mhash]))
        shouldMatch executePactCmd $ ExpectResult $ \cr ->
          preview (crContinuation . _Just . peYield . _Just . ySourceChain . _Just) cr
          `shouldBe`
          (if backCompat then Nothing else Just (ChainId ""))

      let rk = cmdToRequestKey executePactCmd

      case HM.lookup rk chain0Results of
        Nothing -> expectationFailure $
          "Could not find result " ++ show rk ++ ": " ++ show chain0Results
        Just cr -> case _crContinuation cr of
          Nothing -> expectationFailure $
            "No continuation in result: " ++ show rk
          Just pe -> do
            step1 adminKeys executePactCmd moduleCmd' pe mhash
            -- step1fail adminKeys executePactCmd moduleCmd pe

    -- STEP 1: found the pact exec from step 0; return this
    -- from the SPV operation. Run a fresh server, reload
    -- the module, and run the step.
    step1 adminKeys executePactCmd moduleCmd pe mhash = do

      let
          makeContCmdWith = makeContCmd' (Just fakeProof) adminKeys False Null executePactCmd

      chain1Cont <- makeContCmdWith 1 "chain1Cont"
      chain1ContDupe <- makeContCmdWith 1 "chain1ContDupe"

      chain1Results <-
        runAll' [moduleCmd,chain1Cont,chain1ContDupe] (mkSpvSupport pe) spvFlags
      let completedPactMsg =
            "resumePact: pact completed: " ++ showPretty (_cmdHash executePactCmd)

      runResults chain1Results $ do
        succeeds moduleCmd
        case expectFailure of
          Nothing -> do
            -- chain1Cont `succeedsWith` textVal "emily->A->B"
            chain1Cont `succeedsWith'`
              (`shouldBe` (textVal "emily->A->B",
                   if backCompat then [] else
                   [PactEvent
                   "X_RESUME"
                   [ textVal ""
                   , textVal "cross-chain-tester.cross-chain"
                   , PList $ V.fromList [ textVal "emily" ]]
                   "pact"
                   mhash]))
            chain1ContDupe `failsWith` (`shouldBe` completedPactMsg)
          Just expected ->
            chain1ContDupe `failsWith'` expected


pactCrossChainYield :: T.Text -> T.Text
pactCrossChainYield blessExpr =
  [text|
    (module cross-chain-tester GOV
      (defcap GOV () true)
      (defschema schema-a a-result:string)
      $blessExpr
      (defpact cross-chain (name)
        (step
          (let*
            ((nameA (+ name "->A"))
             (r:object{schema-a} { "a-result": nameA }))

            (yield r "")
            nameA))

        (step
          (resume { "a-result" := ar }
                  (+ ar "->B")))
        ))
  |]

nestedPactCrossChainYield :: T.Text
nestedPactCrossChainYield =
  [text|
    (module nested-tester GOV
      (defcap GOV () true)
      (defschema schema-a a-result:string)
      (defpact cross-chain (name)
        (step
          (let*
            ((nameA (+ name "->A"))
             (r:object{schema-a} { "a-result": nameA }))

            (yield r "")
            nameA))

        (step
          (resume { "a-result" := ar }
                  (+ ar "->B")))
        ))
    (module cross-chain-tester GOV
      (defcap GOV () true)
      (defschema schema-a a-result:string)
      (defpact cross-chain (name)
        (step
          (let*
            ((nameA (+ name "->A"))
             (r:object{schema-a} { "a-result": nameA }))
            (nested-tester.cross-chain name)
            (yield r "")
            nameA))

        (step
          (resume { "a-result" := ar }
          (continue (nested-tester.cross-chain name))
                  (+ ar "->B")))
        ))
  |]


-- TWO PARTY ESCROW TESTS

testTwoPartyEscrow :: Spec
testTwoPartyEscrow = do
  context "when debtor tries to cancel pre-timeout" $
    it "throws error and money still escrowed"
      testDebtorPreTimeoutCancel

  context "when debtor tries to cancel after timeout" $
    it "cancels escrow and deposits escrowed amount back to debtor"
      testDebtorPostTimeoutCancel

  it "cancels escrow immediately if creditor cancels"
    testCreditorCancel

  it "throws error when creditor or debtor try to finish alone"
    testFinishAlone

  it "throws error when final price negotiated up"
    testPriceNegUp

  context "when both debtor and creditor finish together" $ do
    it "finishes escrow if final price stays the same or negotiated down"
      testValidEscrowFinish
    it "with valid price, still fails if bad cap is on a signature"
      testPriceNegDownBadCaps


twoPartyEscrow
  :: [Command Text]
  -> (PactHash -> ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ())
  -> Expectation
twoPartyEscrow testCmds act = do
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
  allResults <- runAll allCmds

  runResults allResults $ do
    sysModuleCmd `succeedsWith` (`shouldBe` textVal "system module loaded")
    acctModuleCmd `succeedsWith` (`shouldBe` textVal "TableCreated")
    testModuleCmd `succeedsWith` (`shouldBe` textVal "test module loaded")
    succeeds createAcctCmd -- Alice should be funded with $100
    succeeds resetTimeCmd
    succeeds runEscrowCmd
    balanceCmd `succeedsWith` (`shouldBe` decValue 98.00)
    act (_cmdHash runEscrowCmd)

decValue :: Decimal -> PactValue
decValue = PLiteral . LDecimal

checkContHash
  :: HasCallStack
  => [ApiReqParts]
  -> ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
  -> PactHash
  -> ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
checkContHash reqs act hsh = forM_ reqs $ \req ->
  let desc = show $ view (_1 . to _ylNonce) req
  in case preview (_1 . to _ylPactTxHash . _Just) req of
    Nothing -> liftIO $ expectationFailure $ "Expected pact hash in request: " ++ desc
    Just ph | ph == toUntypedHash hsh -> act
            | otherwise -> liftIO $ toExpectationFailure' ("checkContHash for req " ++ desc ++ ": ") ph hsh


testDebtorPreTimeoutCancel :: Expectation
testDebtorPreTimeoutCancel = do
  let testPath = testDir ++ "cont-scripts/fail-deb-cancel-"

  (req, tryCancelCmd)        <- mkApiReq (testPath ++ "01-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "02-balance.yaml")

  let allCmds = [tryCancelCmd, checkStillEscrowCmd]

  let cancelMsg = "Cancel can only be effected by" <>
                  " creditor, or debitor after timeout"

  twoPartyEscrow allCmds $ checkContHash [req] $ do
    tryCancelCmd `failsWith` (`shouldBe` cancelMsg)
    checkStillEscrowCmd `succeedsWith` (`shouldBe` decValue 98.00)


testDebtorPostTimeoutCancel :: Expectation
testDebtorPostTimeoutCancel = do
  let testPath = testDir ++ "cont-scripts/pass-deb-cancel-"

  (_, setTimeCmd)          <- mkApiReq (testPath ++ "01-set-time.yaml")
  (req, tryCancelCmd)        <- mkApiReq (testPath ++ "02-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "03-balance.yaml")
  let allCmds = [setTimeCmd, tryCancelCmd, checkStillEscrowCmd]

  twoPartyEscrow allCmds $ checkContHash [req] $ do
    succeeds setTimeCmd
    succeeds tryCancelCmd
    checkStillEscrowCmd `succeedsWith` (`shouldBe` decValue 100.00)


testCreditorCancel :: Expectation
testCreditorCancel = do
  let testPath = testDir ++ "cont-scripts/pass-cred-cancel-"

  (_, resetTimeCmd)        <- mkApiReq (testPath ++ "01-reset.yaml")
  (req, credCancelCmd)       <- mkApiReq (testPath ++ "02-rollback.yaml")
  (_, checkStillEscrowCmd) <- mkApiReq (testPath ++ "03-balance.yaml")
  let allCmds = [resetTimeCmd, credCancelCmd, checkStillEscrowCmd]

  twoPartyEscrow allCmds $ checkContHash [req] $ do
    succeeds resetTimeCmd
    succeeds credCancelCmd
    checkStillEscrowCmd `succeedsWith` (`shouldBe` decValue 100.00)


testFinishAlone :: Expectation
testFinishAlone = do
  let testPathCred  = testDir ++ "cont-scripts/fail-cred-finish-"
      testPathDeb   = testDir ++ "cont-scripts/fail-deb-finish-"

  (r1, tryCredAloneCmd) <- mkApiReq (testPathCred ++ "01-cont.yaml")
  (r2, tryDebAloneCmd)  <- mkApiReq (testPathDeb ++ "01-cont.yaml")
  let allCmds = [tryCredAloneCmd, tryDebAloneCmd]

  twoPartyEscrow allCmds $ checkContHash [r1, r2] $ do
    tryCredAloneCmd `failsWith`
      (`shouldBe` "Keyset failure (keys-all): [7d0c9ba1...]")
    tryDebAloneCmd `failsWith`
      (`shouldBe` "Keyset failure (keys-all): [ac69d985...]")


testPriceNegUp :: Expectation
testPriceNegUp = do
  let testPath = testDir ++ "cont-scripts/fail-both-price-up-"

  (req, tryNegUpCmd) <- mkApiReq (testPath ++ "01-cont.yaml")
  twoPartyEscrow [tryNegUpCmd] $ checkContHash [req] $ do
    tryNegUpCmd `failsWith` (`shouldBe` "Price cannot negotiate up")


testValidEscrowFinish :: Expectation
testValidEscrowFinish = do
  let testPath = testDir ++ "cont-scripts/pass-both-price-down-"

  (req, tryNegDownCmd)  <- mkApiReq (testPath ++ "01-cont.yaml")
  (_, credBalanceCmd) <- mkApiReq (testPath ++ "02-cred-balance.yaml")
  (_, debBalanceCmd)  <- mkApiReq (testPath ++ "03-deb-balance.yaml")
  let allCmds = [tryNegDownCmd, credBalanceCmd, debBalanceCmd]

  twoPartyEscrow allCmds $ checkContHash [req] $ do
    tryNegDownCmd `succeedsWith`
                         (`shouldBe` textVal "Escrow completed with 1.75 paid and 0.25 refunded")
    credBalanceCmd `succeedsWith` (`shouldBe` decValue 1.75)
    debBalanceCmd `succeedsWith` (`shouldBe` decValue 98.25)

testPriceNegDownBadCaps :: Expectation
testPriceNegDownBadCaps = do
  let testPath = testDir ++ "cont-scripts/fail-both-price-down-"

  (req, tryNegUpCmd) <- mkApiReq (testPath ++ "01-cont-badcaps.yaml")
  twoPartyEscrow [tryNegUpCmd] $ checkContHash [req] $ do
    tryNegUpCmd `failsWith` (`shouldBe` "Keyset failure (keys-all): [7d0c9ba1...]")




--- UTILS ---

shouldMatch
    :: HasCallStack
    => Command Text
    -> ExpectResult
    -> ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
shouldMatch cmd er = ask >>= liftIO . shouldMatch' (makeCheck cmd er)

shouldMatch' :: HasCallStack => CommandResultCheck -> HM.HashMap RequestKey (CommandResult Hash) -> Expectation
shouldMatch' CommandResultCheck{..} results = checkResult _crcExpect apiRes
  where
    apiRes = HM.lookup _crcReqKey results
    checkResult (ExpectResult crTest) result = case result of
      Nothing -> expectationFailure $ "Failed to find ApiResult for " ++ show _crcReqKey
      Just cr -> crTest cr

succeeds :: HasCallStack => Command Text ->
                ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
succeeds cmd = cmd `succeedsWith` (\_ -> pure ())

succeedsWith :: HasCallStack => Command Text -> (PactValue -> Expectation) ->
                ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
succeedsWith cmd r = succeedsWith' cmd (\(pv,es) -> (es `shouldBe` []) *> r pv)

succeedsWith' :: HasCallStack => Command Text -> ((PactValue,[PactEvent]) -> Expectation) ->
                ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
succeedsWith' cmd r = shouldMatch cmd (resultShouldBe $ Right r)

fails :: HasCallStack => Command Text ->
         ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
fails cmd = cmd `failsWith` (\_ -> pure ())

failsWith :: HasCallStack => Command Text -> (String -> Expectation) ->
             ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
failsWith cmd r = failsWith' cmd (\e -> r (show (peDoc e)))

failsWith' :: HasCallStack => Command Text -> (PactError -> Expectation) ->
             ReaderT (HM.HashMap RequestKey (CommandResult Hash)) IO ()
failsWith' cmd r = shouldMatch cmd (resultShouldBe $ Left r)

runResults :: r -> ReaderT r m a -> m a
runResults rs act = runReaderT act rs

makeExecCmd :: DynKeyPair -> Text -> IO (Command Text)
makeExecCmd keyPairs code = makeExecCmd' Nothing keyPairs code

makeExecCmd' :: Maybe Text -> DynKeyPair -> Text -> IO (Command Text)
makeExecCmd' nonce keyPairs code = mkExec code
  (object ["admin-keyset" .= [formatPubKeyForCmd keyPairs]]) def [(keyPairs,[])] [] Nothing nonce


formatPubKeyForCmd :: DynKeyPair -> Value
formatPubKeyForCmd (DynEd25519KeyPair kp) = toJSON $ toB16Text $ getPublic kp
formatPubKeyForCmd (DynWebAuthnKeyPair _ pub _priv) = toJSON $ toB16Text $ exportWebAuthnPublicKey pub



makeContCmd
  :: DynKeyPair     -- signing pair
  -> Bool           -- isRollback
  -> Value          -- data
  -> Command Text   -- cmd to get pact Id from
  -> Int            -- step
  -> Text           -- nonce
  -> IO (Command Text)
makeContCmd = makeContCmd' Nothing


makeContCmd'
  :: Maybe ContProof
  -> DynKeyPair
      -- ^ signing pair
  -> Bool
      -- ^ isRollback
  -> Value
      -- ^ data
  -> Command Text
      -- ^ cmd to get pact Id from
  -> Int
      -- ^ step
  -> Text
      -- ^ nonce
  -> IO (Command Text)
makeContCmd' contProofM keyPairs isRollback cmdData pactExecCmd step nonce =
  mkCont (getPactId pactExecCmd) step isRollback cmdData def [(keyPairs,[])] [] (Just nonce) contProofM Nothing

textVal :: Text -> PactValue
textVal = PLiteral . LString

getPactId :: Command Text -> PactId
getPactId cmd = toPactId hsh
  where hsh = (toUntypedHash . _cmdHash) cmd


pactIdNotFoundMsg :: Command Text -> String
pactIdNotFoundMsg cmd = "resumePact: pact completed: " <> txtPact
  where txtPact = unpack (asString (getPactId cmd))

stepMisMatchMsg :: Bool -> Int -> Int -> String
stepMisMatchMsg isRollback attemptStep currStep =
  "resumePactExec: " <> typeOfStep <> " step mismatch with context: ("
        <> show attemptStep <> ", " <> show currStep <> ")"
  where typeOfStep = if isRollback then "rollback" else "exec"

newtype ExpectResult = ExpectResult (CommandResult Hash -> Expectation)
    deriving (Semigroup)

data CommandResultCheck = CommandResultCheck
  { _crcReqKey :: RequestKey
  , _crcExpect :: ExpectResult
  }


makeCheck :: Command T.Text -> ExpectResult -> CommandResultCheck
makeCheck c@Command{} expect = CommandResultCheck (cmdToRequestKey c) expect

runAll :: [Command T.Text] -> IO (HM.HashMap RequestKey (CommandResult Hash))
runAll cmds = runAll' cmds noSPVSupport testFlags

runAll'
  :: [Command T.Text]
  -> SPVSupport
  -> [ExecutionFlag]
  -> IO (HM.HashMap RequestKey (CommandResult Hash))
runAll' cmds spv flags =
  withTestPactServerWithSpv "continuationspec" flags spv $ \clientEnv ->
    run clientEnv cmds



run :: ClientEnv -> [Command T.Text] -> IO (HM.HashMap RequestKey (CommandResult Hash))
run clientEnv cmds = do
  sendResp <- doSend clientEnv . SubmitBatch $ NEL.fromList cmds
  case sendResp of
    Left servantErr -> Exception.evaluate (error $ show servantErr)
    Right RequestKeys{..} -> do
      results <- timeout 3000000 (helper _rkRequestKeys)
      case results of
        Nothing -> Exception.evaluate (error "Received empty poll. Timeout in retrying.")
        Just res -> return res

  where helper reqKeys = do
          pollResp <- doPoll clientEnv $ Poll reqKeys
          case pollResp of
            Left servantErr -> Exception.evaluate (error $ show servantErr)
            Right (PollResponses apiResults) ->
              if null apiResults then helper reqKeys
              else return apiResults



doSend :: ClientEnv -> SubmitBatch -> IO (Either ClientError RequestKeys)
doSend clientEnv req = runClientM (sendClient req) clientEnv

doPoll :: ClientEnv -> Poll -> IO (Either ClientError PollResponses)
doPoll clientEnv req = runClientM (pollClient req) clientEnv

resultShouldBe
    :: HasCallStack
    => Either (PactError -> Expectation) ((PactValue,[PactEvent]) -> Expectation)
    -> ExpectResult
resultShouldBe expect = ExpectResult $ \cr ->
  case (expect, _pactResult (_crResult cr)) of
    (Left expErr, Left e) -> expErr e
    (Right expVal, Right pv) -> expVal (pv, _crEvents cr)
    _ -> expectationFailure $ unwords
      [ "Expected", either (\_ -> "PactError") (\_ -> "PactValue") expect, ", found", show cr ]

toExpectationFailure' :: (HasCallStack, Show e, Show a) => String -> e -> a -> Expectation
toExpectationFailure' msg expect actual =
  expectationFailure $ msg ++ "Expected " ++ show expect ++ ", found " ++ show actual
