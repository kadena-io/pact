{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.TestRunner
  ( ApiResultCheck (..)
  , testDir
  , runAll
  , flushDb
  , genKeys
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

import Pact.Server.Server (setupServer)
import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command

import "crypto-api" Crypto.Random
import Crypto.Ed25519.Pure

import Data.Aeson hiding (Options)
import Test.Hspec
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Control.Exception as Exception
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Control.Lens
import Network.Wreq
import System.Directory
import System.Timeout
import NeatInterpolation (text)

testDir, _testLogDir, _testConfigFilePath, _testPort, _serverPath :: String
testDir = "tests/"
_testLogDir = testDir ++ "test-log/"
_testConfigFilePath = testDir ++ "test-config.yaml"

_testPort = "8080"
_serverPath = "http://localhost:" ++ _testPort ++ "/api/v1/"

_logFiles :: [String]
_logFiles = ["access.log","commands.sqlite","error.log","pact.sqlite"]

data ApiResultCheck = ApiResultCheck
  { _arcReqKey :: RequestKey
  , _arcIsFailure :: Bool
  , _arcExpect :: Maybe Value
  } deriving (Show, Eq)

runAll :: Options -> [Command T.Text] -> IO (HM.HashMap RequestKey ApiResult)
runAll opts cmds = Exception.bracket
              (startServer _testConfigFilePath)
               stopServer
              (const (run opts cmds))

startServer :: FilePath -> IO (Async (), Async (), Async ())
startServer configFile = do
  (runServer, asyncCmd, asyncHist) <- setupServer configFile
  asyncServer <- async runServer
  link2 asyncServer asyncCmd
  link2 asyncServer asyncHist
  waitUntilStarted
  return (asyncServer, asyncCmd, asyncHist)

waitUntilStarted :: IO ()
waitUntilStarted = do
  r <- get $ _serverPath ++ "poll"
  let s = r ^. responseStatus . statusCode
  if s == 200
    then return ()
    else do
      threadDelay 500
      waitUntilStarted

stopServer :: (Async (), Async (), Async ()) -> IO ()
stopServer (asyncServer, asyncCmd, asyncHist) = do
  uninterruptibleCancel asyncCmd
  uninterruptibleCancel asyncHist
  uninterruptibleCancel asyncServer
  mapM_ checkFinished [asyncServer, asyncCmd, asyncHist]
  where checkFinished asy = poll asy >>= \case
          Nothing -> Exception.evaluate $ error $
                    "Thread " ++ show (asyncThreadId asy) ++ " could not be cancelled."
          _ -> return ()

run :: Options -> [Command T.Text] -> IO (HM.HashMap RequestKey ApiResult)
run opts cmds = do
  sendResp <- doSend opts $ SubmitBatch cmds
  case sendResp of
    ApiFailure err -> Exception.evaluate (error err)
    ApiSuccess RequestKeys{..} -> do
      results <- timeout 3000000 (helper _rkRequestKeys)
      case results of
        Nothing -> Exception.evaluate (error "Received empty poll. Timeout in retrying.")
        Just res -> return res

  where helper reqKeys = do
          pollResp <- doPoll opts $ Poll reqKeys
          case pollResp of
            ApiFailure err -> Exception.evaluate (error err)
            ApiSuccess (PollResponses apiResults) ->
              if null apiResults then helper reqKeys
              else return apiResults

doSend :: (ToJSON req) => Options -> req -> IO (ApiResponse RequestKeys)
doSend opts req = view responseBody <$> doSend' opts req

doSend' :: (ToJSON req) => Options -> req -> IO (Response (ApiResponse RequestKeys))
doSend' opts req = do
  sendResp <- postWith opts (_serverPath ++ "send") (toJSON req)
  asJSON sendResp

doPoll :: (ToJSON req) => Options -> req -> IO (ApiResponse PollResponses)
doPoll opts req = view responseBody <$> doPoll' opts req

doPoll' :: (ToJSON req) => Options -> req -> IO (Response (ApiResponse PollResponses))
doPoll' opts req = do
  pollResp <- postWith opts (_serverPath ++ "poll") (toJSON req)
  asJSON pollResp


flushDb :: IO ()
flushDb = mapM_ deleteIfExists _logFiles
  where deleteIfExists filename = do
          let fp = _testLogDir ++ filename
          isFile <- doesFileExist fp
          when isFile $ removeFile fp

genKeys :: IO KeyPair
genKeys = do
  g :: SystemRandom <- newGenIO
  case generateKeyPair g of
    Left _ -> error "Something went wrong in genKeys"
    Right (s,p,_) -> return $ KeyPair s p



makeCheck :: Command T.Text -> Bool -> Maybe Value -> ApiResultCheck
makeCheck Command{..} isFailure expect = ApiResultCheck (RequestKey _cmdHash) isFailure expect

checkResult :: Bool -> Maybe Value -> Maybe ApiResult -> Expectation
checkResult isFailure expect result =
  case result of
    Nothing -> expectationFailure $ show result ++ " should be Just ApiResult"
    Just (ApiResult cmdRes _ _) ->
      case cmdRes of
        Object h -> if isFailure then checkIfFailure h expect
                    else checkIfSuccess h expect
        _ -> expectationFailure $ show cmdRes ++ " should be Object"


fieldShouldBe :: (T.Text,HM.HashMap T.Text Value) -> Maybe Value -> Expectation
fieldShouldBe (k,m) b = do
  let a = HM.lookup k m
  unless (a == b) $
    expectationFailure $
    "Expected " ++ show b ++ ", found " ++ show a ++ " for field " ++ show k ++ " in " ++ show m

checkIfSuccess :: Object -> Maybe Value -> Expectation
checkIfSuccess h Nothing =
  ("status",h) `fieldShouldBe` (Just . String . T.pack) "success"
checkIfSuccess h (Just expect) = do
  ("status", h) `fieldShouldBe` (Just . String . T.pack) "success"
  ("data", h) `fieldShouldBe` Just (toJSON expect)

checkIfFailure :: Object -> Maybe Value -> Expectation
checkIfFailure h Nothing =
  ("status", h) `fieldShouldBe` (Just . String . T.pack) "failure"
checkIfFailure h (Just expect) = do
  ("status", h) `fieldShouldBe` (Just . String . T.pack) "failure"
  ("detail", h) `fieldShouldBe` Just (toJSON expect)



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
              (step-with-rollback "step 2" "rollback 2")))
            |]

pactWithRollbackErrCode :: String -> T.Text
pactWithRollbackErrCode moduleName = T.concat [begCode, T.pack moduleName, endCode]
  where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
                       (module|]
        endCode = [text| 'k
            (defpact tester ()
              (step-with-rollback "step 0" "rollback 0")
              (step-with-rollback "step 1" (+ "will throw error in rollback 1"))
              (step-with-rollback "step 2" "rollback 2")))
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
