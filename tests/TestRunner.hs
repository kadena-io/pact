{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{--module TestRunner
  () where
--}
import Pact.Server.Server
import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command

import Crypto.Random
import Crypto.Ed25519.Pure

import Data.Aeson
import Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Lens
import Network.Wreq
import System.Directory

_testDir, _testLogDir, _testConfigFilePath, testPort, _serverPath :: String
_testDir = "tests/resume/"
_testLogDir = "tests/resume/test-log/"
_testConfigFilePath = "tests/resume/test-config.yaml"

testPort = "8080"
_serverPath = "http://localhost:" ++ testPort ++ "/api/v1/"

_logFiles :: [String]
_logFiles = ["access.log","commands.sqlite","error.log","pact.sqlite"]

data TestExec = TestExec
  { _teCode :: String,
    _teData :: Value,
    _teKeyPairs :: [KeyPair],
    _teNonce :: String
  } deriving (Eq,Show) 

data TestCont = TestCont
  { _tcTxId :: Int,
    _tcStep :: Int,
    _tcRollback :: Bool,
    _tcData :: Value,
    _tcKeyPairs :: [KeyPair],
    _tcNonce :: String
  } deriving (Eq,Show)

main :: IO ()
main = do
  (priv,publ) <- genKeys
  cmd <- mkExec  "(+ 1 2)" Null Nothing
             [KeyPair priv publ] (Just "test1")
  res <- runAll [cmd]
  print "Printing result of runAll"
  print res
  print "server should be down. check"
  _ <- threadDelay (10000000)
  print "time to check ended. cleaning up now"
  flushDb

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
          ApiSuccess (PollResponses apiResults) -> return apiResults

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

genKeys :: IO ((PrivateKey, PublicKey))
genKeys = do
  g :: SystemRandom <- newGenIO
  case generateKeyPair g of
    Left _ -> error "Something went wrong in genKeys"
    Right (s,p,_) -> return (s,p)
