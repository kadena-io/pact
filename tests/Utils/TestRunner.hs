{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.TestRunner
  ( ApiResultCheck(..)
  , runAll
  , flushDb
  , genKeys
  , threeStepPactCode
  , makeCheck
  , checkResults
  , checkIfSuccess
  , checkIfFailure
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

_testDir, _testLogDir, _testConfigFilePath, _testPort, _serverPath :: String
_testDir = "tests/resume/"
_testLogDir = "tests/resume/test-log/"
_testConfigFilePath = "tests/resume/test-config.yaml"

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

checkResults :: HM.HashMap RequestKey ApiResult -> [ApiResultCheck] -> Bool
checkResults resultsMap checks = foldl isExpected True checks
  where isExpected acc ApiResultCheck{..} =
          case (HM.lookup _arcReqKey resultsMap) of
            Nothing -> False
            Just (ApiResult cmdRes _ _) -> case cmdRes of
              Object h -> if _arcIsFailure then (acc && (checkIfFailure h _arcExpect))
                          else (acc && (checkIfSuccess h _arcExpect))
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

threeStepPactCode :: String -> T.Text
threeStepPactCode moduleName = T.concat [begCode, T.pack moduleName, endCode]
     where begCode = [text| (define-keyset 'k (read-keyset "admin-keyset"))
            (module|]
           endCode = [text| 'k
              (defpact tester ()
                (step
                 (let ((str1 "step 1"))
                  str1))
                (step
                 (let ((str2 "step 2"))
                  str2))
                (step
                 (let ((str3 "step 3"))
                  str3))))
            |] 

