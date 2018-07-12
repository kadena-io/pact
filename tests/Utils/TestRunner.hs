{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

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
  ) where

import Pact.Server.Server
import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command

import Crypto.Random
import Crypto.Ed25519.Pure

import Data.Aeson
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Async
import Control.Monad
import Control.Lens
import Network.Wreq
import System.Directory
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

runAll :: [Command T.Text] -> IO (HM.HashMap RequestKey ApiResult)
runAll cmds = do
  withAsync (serve _testConfigFilePath) $ \_ -> do
    sendResp <- doSend $ SubmitBatch cmds
    case sendResp of
      ApiFailure _ -> return $ HM.empty
      ApiSuccess RequestKeys{..} -> do
        pollResp <- doPoll $ Poll _rkRequestKeys
        case pollResp of
          ApiFailure _ -> return $ HM.empty
          ApiSuccess (PollResponses apiResults) -> do
            return apiResults

doSend :: (ToJSON req) => req -> IO (ApiResponse RequestKeys)
doSend req = do
  sendResp <- doSend' req
  return $ view responseBody sendResp
    
doSend' :: (ToJSON req) => req -> IO (Response (ApiResponse RequestKeys))
doSend' req = do
  sendResp <- post (_serverPath ++ "send") (toJSON req)
  asJSON sendResp

doPoll :: (ToJSON req) => req -> IO (ApiResponse PollResponses)
doPoll req = do
  pollResp <- doPoll' req
  return $ view responseBody pollResp

doPoll' :: (ToJSON req) => req -> IO (Response (ApiResponse PollResponses))
doPoll' req = do
  pollResp <- post (_serverPath ++ "poll") (toJSON req)
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

checkResult :: Bool -> Maybe Value -> Maybe ApiResult -> Bool
checkResult isFailure expect result =
  case result of
    Nothing -> False
    Just (ApiResult cmdRes _ _) ->
      case cmdRes of
        Object h -> if isFailure then (checkIfFailure h expect)
                    else (checkIfSuccess h expect)
        _ -> False

checkIfSuccess :: Object -> Maybe Value -> Bool
checkIfSuccess h Nothing = (HM.lookup (T.pack "status") h == (Just . String . T.pack) "success")
checkIfSuccess h (Just expect) = isSuccess && isMatch
  where isSuccess = (HM.lookup (T.pack "status") h == (Just . String . T.pack) "success")
        isMatch = (HM.lookup (T.pack "data") h == Just (toJSON expect))

checkIfFailure :: Object -> Maybe Value -> Bool
checkIfFailure h Nothing = (HM.lookup (T.pack "status") h == (Just . String . T.pack) "failure")
checkIfFailure h (Just expect) = isFailure && isMatch
  where isFailure = (HM.lookup (T.pack "status") h == (Just . String . T.pack) "failure")
        isMatch = (HM.lookup (T.pack "detail") h == Just (toJSON expect))



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
