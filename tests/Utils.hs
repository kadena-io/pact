{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils
( testMgr
, withTestPactServer
, withTestPactServerWithSpv
, testFlags
, backCompatFlags
, nestedDefPactFlags
, testDir
) where

import Control.Concurrent (threadDelay)
import Control.Exception

import qualified Data.Text as T

import qualified Network.HTTP.Client as HTTP

import qualified Pact.JSON.Encode as J

import Servant.Client

import System.IO.Temp
import System.IO.Unsafe

-- internal modules

import Pact.Server.API
import Pact.Server.Server
import Pact.Types.Runtime
import Pact.Types.SPV

-- -------------------------------------------------------------------------- --
-- Global Test Manager

-- | Use a single global Manager throughout all tests
--
testMgr :: HTTP.Manager
testMgr = unsafePerformIO $ HTTP.newManager HTTP.defaultManagerSettings
{-# NOINLINE testMgr #-}

-- -------------------------------------------------------------------------- --
-- Constants

testDir :: FilePath
testDir = "tests/"

testFlags :: [ExecutionFlag]
testFlags = [ FlagDisablePact43, FlagDisablePact44 ]

backCompatFlags :: [ExecutionFlag]
backCompatFlags = [ FlagDisablePact40, FlagDisablePact43, FlagDisablePact44 ]

nestedDefPactFlags :: [ExecutionFlag]
nestedDefPactFlags = []

serverRoot :: Port -> String
serverRoot port = "http://localhost:" ++ show port ++ "/"

-- -------------------------------------------------------------------------- --
-- Internal Tools

waitUntilStarted :: Port -> Int -> IO ()
waitUntilStarted _ i | i > 10 = throwIO $ userError "waitUntilStarted: failing after 10 attempts"
waitUntilStarted port i = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  baseUrl' <- parseBaseUrl $ serverRoot port
  let clientEnv = mkClientEnv mgr baseUrl'
  r <- runClientM versionClient clientEnv
  case r of
    Right _ -> pure ()
    Left _ -> do
      threadDelay 100
      waitUntilStarted port (succ i)

withTestPactServerWithSpv_ :: SPVSupport -> FilePath -> (Port -> IO a) -> IO a
withTestPactServerWithSpv_ spv configFile app =
  withTestServe configFile spv $ \port -> do
    waitUntilStarted port 0
    app port

withTestConfig :: String -> [ExecutionFlag] -> (FilePath -> IO a) -> IO a
withTestConfig label flags inner =
  withSystemTempDirectory ("pact-tests." <> label) $ \dir -> do
    let confFilePath = dir <> "/" <> "test-config.yaml"
    J.encodeFile confFilePath $ J.object
      [ "port" J..= J.Aeson @Int 0 -- ignored by test server
      , "logDir" J..= J.string dir
      , "persistDir" J..= J.string dir
      , "pragmas" J..= J.Array @[T.Text] []
      , "verbose" J..= False
      , "execConfig" J..= J.Array flags
      ]
    inner confFilePath

-- -------------------------------------------------------------------------- --
-- Test Pact Server

withTestPactServer :: String -> (ClientEnv -> IO a) -> IO a
withTestPactServer label = withTestPactServerWithSpv label testFlags noSPVSupport

withTestPactServerWithSpv
  :: String
  -> [ExecutionFlag]
  -> SPVSupport
  -> (ClientEnv -> IO a)
  -> IO a
withTestPactServerWithSpv label flags spv action =
  withTestConfig label flags $ \fp ->
    withTestPactServerWithSpv_ spv fp $ \port -> do
      clientEnv <- mkClientEnv testMgr <$> parseBaseUrl (serverRoot port)
      action clientEnv

