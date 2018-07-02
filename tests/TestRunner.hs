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
    _teKeyPair :: KeyPair,
    _teNonce :: String
  } deriving Show 

data TestResult = Error String | Success ApiResult
  deriving Show

main :: IO ()
main = do
  {--withAsync (serve _testConfigFilePath) $ \a -> do
      (priv,publ) <- genKeys
      req <- toJSON (SubmitBatch
                         [mkExec "(+ 1 2)" Null Nothing
                          [KeyPair priv publ]
                           $ Just "test1"]) 
      print "Printing response of send"
      print r
      print "Submitting Poll"
      p <- post "http://localhost:8080/api/v1/poll" (toJSON (Poll [RequestKey (_cmdHash req)]))
      print "Printing response of poll"
      print p
      print "printing result of sending command"
      _ <- threadDelay (20000000)
      print (asyncThreadId a)
      iscancelled <- poll a
      print iscancelled--}

  print "server should be down. check"
  _ <- threadDelay (10000000)
  print "time to check ended. cleaning up now"
  flushDb

{-testCmd :: IO (ApiResponse ApiResult)
testCmd = 
  withAsync (serve _testConfigFilePath) $ \a -> do
      (priv,publ) <- genKeys
      sendReq <- mkExec  "(+ 1 2)" Null Nothing
                 [KeyPair priv publ] (Just "test1")
      sendResp <- asJSON =<< post (_serverPath ++ "send") (toJSON $ SubmitBatch [sendReq])
      case (view responseBody sendResp) of
        sf@ApiFailure{..} -> return sf
        ApiSuccess{..} -> do
          let pollReq = toJSON (_apiResponse)
          pollResp <- asJSON =<< post (_serverPath ++ "poll") pollReq
          
          case (PollResponses $ view responseBody pollResp) of
            pf@ApiFailure{..} -> return pf
            ps@ApiSuccess{..} -> return ps-}

{--testExecCmd :: [TestExec] -> IO (TestResult)
testExecCmd cmd = do
  let batch = --}
    
doSend :: (ToJSON req) => req -> IO (Response (ApiResponse RequestKeys))
doSend req = do
  sendResp <- post (_serverPath ++ "send") (toJSON req)
  asJSON sendResp

doPoll :: (ToJSON req) => req -> IO (Response (ApiResponse PollResponses))
doPoll req = do
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
