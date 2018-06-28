{-# LANGUAGE ScopedTypeVariables #-}
{--module TestRunner
  () where
--}
import Pact.Server.Server
import Pact.ApiReq
import Pact.Types.API

import Crypto.Random
import Crypto.Ed25519.Pure

import Data.Aeson (toJSON, Value (Null))
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (when)
import Network.Wreq
import System.Directory

_testDir, _testLogDir, _testConfigFilePath, testPort :: String
_testDir = "tests/resume/"
_testLogDir = "tests/resume/test-log/"
_testConfigFilePath = "tests/resume/test-config.yaml"
testPort = "8080"

_logFiles :: [String]
_logFiles = ["access.log","commands.sqlite","error.log","pact.sqlite"]

main :: IO ()
main = do
  withAsync (serve _testConfigFilePath) $ \a -> do
      (priv,publ) <- genKeys
      req <- mkExec "(+ 1 2)" Null Nothing
             [KeyPair priv publ]
             $ Just "test1"
      print "Printing out request"
      print (toJSON req)
      r <- post "http://localhost:8080/api/v1/send" (toJSON (SubmitBatch [req]))
      print r
      _ <- threadDelay (20000000)
      print (asyncThreadId a)
      iscancelled <- poll a
      print iscancelled

  print "server should be down. check"
  _ <- threadDelay (10000000)
  print "time to check ended. cleaning up now"
  flushDb

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
