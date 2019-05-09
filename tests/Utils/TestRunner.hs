{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.TestRunner
  ( CommandResultCheck(..), OptionalPactResult
  , testDir
  , runAll
  , flushDb
  , Crypto.SomeKeyPair
  , genKeys
  , formatPubKeyForCmd
  , makeCheck
  , checkResult
  , threeStepPactCode
  , errorStepPactCode
  , pactWithRollbackCode
  , pactWithRollbackErrCode
  , pactWithYield
  , pactWithYieldErr
  , pactWithSameNameYield
  , startServer
  , stopServer
  ) where

import Pact.Server.Server (serve)
import qualified Pact.Server.Client as C
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Crypto as Crypto
import Pact.Types.Util (toB16JSON)
import Pact.Types.Hash (Hash)
import Pact.Types.Runtime (PactError(..))
import Pact.Types.PactValue (PactValue(..))

import Control.Exception
import Data.Aeson
import Test.Hspec
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Control.Exception as Exception
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Network.HTTP.Client (Manager)
import qualified Network.HTTP.Client as HTTP
import Servant.Client
import System.Directory
import System.Timeout
import NeatInterpolation (text)

testDir, _testLogDir, _testConfigFilePath, _testPort, _serverPath, _serverRootPath :: String
testDir = "tests/"
_testLogDir = testDir ++ "test-log/"
_testConfigFilePath = testDir ++ "test-config.yaml"

_testPort = "8080"
_serverPath = "http://localhost:" ++ _testPort ++ "/api/v1/"

_serverRootPath = "http://localhost:" ++ _testPort ++ "/"

_serverBaseUrl :: IO BaseUrl
_serverBaseUrl = parseBaseUrl _serverRootPath

_logFiles :: [String]
_logFiles = ["access.log","commands.sqlite","error.log","pact.sqlite"]

data CommandResultCheck = CommandResultCheck
  { _arcReqKey :: RequestKey
  , _arcExpect :: OptionalPactResult
  } deriving (Show, Eq)


type OptionalPactResult = Either (Maybe PactError) (Maybe PactValue)



runAll :: Manager -> [Command T.Text] -> IO (HM.HashMap RequestKey (CommandResult Hash))
runAll mgr cmds = Exception.bracket
              (startServer _testConfigFilePath)
               stopServer
              (const (run mgr cmds))

startServer :: FilePath -> IO (Async ())
startServer configFile = do
  asyncServer <- async $ serve configFile
  waitUntilStarted 0
  return asyncServer

waitUntilStarted :: Int -> IO ()
waitUntilStarted i | i > 10 = throwIO $ userError "waitUntilStarted: failing after 10 attempts"
waitUntilStarted i = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  baseUrl <- _serverBaseUrl
  let clientEnv = mkClientEnv mgr baseUrl
  r <- runClientM (C.version C.pactServerApiClient) clientEnv
  case r of
    Right _ -> pure ()
    Left _ -> do
      threadDelay 500
      waitUntilStarted (succ i)

stopServer :: Async () -> IO ()
stopServer asyncServer = do
  cancel asyncServer
  mapM_ checkFinished [asyncServer]
  where checkFinished asy = poll asy >>= \case
          Nothing -> Exception.evaluate $ error $
                    "Thread " ++ show (asyncThreadId asy) ++ " could not be cancelled."
          _ -> return ()

run :: Manager -> [Command T.Text] -> IO (HM.HashMap RequestKey (CommandResult Hash))
run mgr cmds = do
  sendResp <- doSend mgr $ SubmitBatch cmds
  case sendResp of
    Left servantErr -> Exception.evaluate (error $ show servantErr)
    Right RequestKeys{..} -> do
      results <- timeout 3000000 (helper _rkRequestKeys)
      case results of
        Nothing -> Exception.evaluate (error "Received empty poll. Timeout in retrying.")
        Just res -> return res

  where helper reqKeys = do
          pollResp <- doPoll mgr $ Poll reqKeys
          case pollResp of
            Left servantErr -> Exception.evaluate (error $ show servantErr)
            Right (PollResponses apiResults) ->
              if null apiResults then helper reqKeys
              else return apiResults

doSend :: Manager -> SubmitBatch -> IO (Either ServantError RequestKeys)
doSend mgr req = do
  baseUrl <- _serverBaseUrl
  runClientM (C.send C.pactServerApiClient req) (mkClientEnv mgr baseUrl)

doPoll :: Manager -> Poll -> IO (Either ServantError PollResponses)
doPoll mgr req = do
  baseUrl <- _serverBaseUrl
  runClientM (C.poll C.pactServerApiClient req) (mkClientEnv mgr baseUrl)

flushDb :: IO ()
flushDb = mapM_ deleteIfExists _logFiles
  where deleteIfExists filename = do
          let fp = _testLogDir ++ filename
          isFile <- doesFileExist fp
          when isFile $ removeFile fp

genKeys :: IO SomeKeyPair
genKeys = genKeyPair defaultScheme

formatPubKeyForCmd :: SomeKeyPair -> Value
formatPubKeyForCmd kp = toB16JSON $ formatPublicKey kp



makeCheck :: Command T.Text -> OptionalPactResult -> CommandResultCheck
makeCheck c@Command{..} expect = CommandResultCheck (cmdToRequestKey c) expect


checkResult :: HasCallStack => OptionalPactResult -> Maybe (CommandResult Hash) -> Expectation
checkResult expect result =
  case result of
    Nothing -> expectationFailure $ show result ++ " should be Just CommandResult"
    Just CommandResult{..} -> _crResult `resultShouldBe` expect


resultShouldBe :: HasCallStack => PactResult -> OptionalPactResult -> Expectation
resultShouldBe (PactResult actual) expect =
  case (actual, expect) of
    (Left (PactError aTyp _ _ aDoc),
     Left (Just (PactError eTyp _ _ eDoc)))   -> do
                                                 unless (aTyp == eTyp) $
                                                   toExpectation aTyp eTyp "PactError->type" actual
                                                 unless (aDoc == eDoc) $
                                                   toExpectation aDoc eDoc "PactError->doc" actual
    (Right aVal, Right (Just eVal)) -> unless (aVal == eVal) $
                                         toExpectation aVal eVal "PactValue" actual
    (Left _, Left Nothing)          -> return ()
    (Right _, Right Nothing)        -> return ()
    _ -> toExpectation
         (eitherToStatus actual)
         (eitherToStatus expect) "status" actual


toExpectation :: (HasCallStack, Show a, Show e, Show c) => a -> e -> T.Text -> c -> Expectation
toExpectation actual expect sectionName fullActual =
  expectationFailure $ "Expected " ++ show expect ++ ", found " ++ show actual
                       ++ " for section " ++ show sectionName ++ " in " ++ show fullActual


eitherToStatus :: Either l r -> T.Text
eitherToStatus (Left _) = "failure"
eitherToStatus (Right _) = "success"



-- SAMPLE PACT CODE

threeStepPactCode :: String -> T.Text
threeStepPactCode moduleName = T.concat [begCode, T.pack moduleName, endCode]
     where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
            (module|]
           endCode = [text| 'k
              (defpact tester ()
                (step "step 0")
                (step "step 1")
                (step "step 2")))
              |]

errorStepPactCode :: String -> T.Text
errorStepPactCode moduleName = T.concat [begCode, T.pack moduleName, endCode]
     where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
                          (module|]
           endCode = [text| 'k
              (defpact tester ()
                (step "step 0")
                (step (+ "will throw error in step 1"))
                (step "step 2")))
              |]

pactWithRollbackCode :: String -> T.Text
pactWithRollbackCode moduleName = T.concat [begCode, T.pack moduleName, endCode]
  where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
                       (module|]
        endCode = [text| 'k
            (defpact tester ()
              (step-with-rollback "step 0" "rollback 0")
              (step-with-rollback "step 1" "rollback 1")
              (step               "step 2")))
            |]

pactWithRollbackErrCode :: String -> T.Text
pactWithRollbackErrCode moduleName = T.concat [begCode, T.pack moduleName, endCode]
  where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
                       (module|]
        endCode = [text| 'k
            (defpact tester ()
              (step-with-rollback "step 0" "rollback 0")
              (step-with-rollback "step 1" (+ "will throw error in rollback 1"))
              (step               "step 2")))
            |]

pactWithYield :: String -> T.Text
pactWithYield moduleName = T.concat [begCode, T.pack moduleName, endCode]
  where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
                       (module|]
        endCode = [text| 'k
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
            |]

pactWithYieldErr :: String -> T.Text
pactWithYieldErr moduleName = T.concat [begCode, T.pack moduleName, endCode]
  where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
                       (module|]
        endCode = [text| 'k
            (defpact tester (name)
              (step
                (let ((result0 (+ name "->Step0")))
                  (yield { "step0-result": result0 })
                result0))
              (step "step 1 has no yield")
              (step
                (resume { "step0-result" := res0 }
                      (+ res0 "->Step2")))))
            |]

pactWithSameNameYield :: String -> T.Text
pactWithSameNameYield moduleName = T.concat [begCode, T.pack moduleName, endCode]
  where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
                       (module|]
        endCode = [text| 'k
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
                     res)))))
            |]
