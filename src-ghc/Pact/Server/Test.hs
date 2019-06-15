{-# LANGUAGE LambdaCase #-}
module Pact.Server.Test
  (
  -- * Test server management
    startServer
  , startServer'
  , stopServer
  , flushDb
  -- * Crypto util
  , genKeys
  -- * Directories and constants
  , testDir
  , serverBaseUrl
  ) where
import Pact.Server.Server (serve)
import Pact.Server.API
import Pact.Types.Crypto as Crypto

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Servant.Client
import System.Directory
import qualified Control.Exception as Exception
import qualified Network.HTTP.Client as HTTP

import Pact.Types.SPV


genKeys :: IO SomeKeyPair
genKeys = genKeyPair defaultScheme

flushDb :: IO ()
flushDb = mapM_ deleteIfExists logFiles
  where deleteIfExists filename = do
          let fp = testLogDir ++ filename
          isFile <- doesFileExist fp
          when isFile $ removeFile fp

logFiles :: [String]
logFiles = ["access.log","commands.sqlite","error.log","pact.sqlite"]

testLogDir :: FilePath
testLogDir = testDir ++ "test-log/"

testDir :: FilePath
testDir = "tests/"

serverRootPath :: String
serverRootPath = "http://localhost:" ++ testPort ++ "/"

testPort :: String
testPort = "8080"

serverBaseUrl :: IO BaseUrl
serverBaseUrl = parseBaseUrl serverRootPath


startServer :: FilePath -> IO (Async ())
startServer configFile = startServer' configFile noSPVSupport

startServer' :: FilePath -> SPVSupport -> IO (Async ())
startServer' configFile spv = do
  asyncServer <- async $ serve configFile spv
  waitUntilStarted 0
  return asyncServer


waitUntilStarted :: Int -> IO ()
waitUntilStarted i | i > 10 = throwIO $ userError "waitUntilStarted: failing after 10 attempts"
waitUntilStarted i = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  baseUrl' <- serverBaseUrl
  let clientEnv = mkClientEnv mgr baseUrl'
  r <- runClientM versionClient clientEnv
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
