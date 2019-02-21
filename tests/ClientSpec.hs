{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ClientSpec (spec) where

import Data.Aeson
import Data.Default (def)
import Test.Hspec
import Utils.TestRunner
import qualified Network.HTTP.Client as HTTP
import qualified Control.Exception as Exception

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)
import Pact.Server.Client
import Servant.Client

_testLogDir, _testConfigFilePath, _testPort, _serverPath :: String
_testLogDir = testDir ++ "test-log/"
_testConfigFilePath = testDir ++ "test-config.yaml"

_testPort = "8080"
_serverPath = "http://localhost:" ++ _testPort

bracket :: IO a -> IO a
bracket action = Exception.bracket (startServer _testConfigFilePath) stopServer (const action)

simpleServerCmd :: IO (Command Text)
simpleServerCmd = do
  simpleKeys <- genKeys
  mkExec  "(+ 1 2)" Null def [simpleKeys] (Just "test1")

spec :: Spec
spec = around_ bracket $ describe "Servant API client tests" $ do
  mgr <- runIO $ HTTP.newManager HTTP.defaultManagerSettings
  url <- runIO $ parseBaseUrl _serverPath
  let clientEnv = mkClientEnv mgr url
  -- it "incorrectly runs a simple command privately" $ do
  --   cmd <- simpleServerCmd
  --   res <- runClientM (private (SubmitBatch [cmd])) clientEnv
  --   let expt = Right (ApiFailure "Send private: payload must have address")
  --   res `shouldBe` expt
  it "correctly runs a simple command locally" $ do
    cmd <- simpleServerCmd
    res <- runClientM (local pactServerApiClient cmd) clientEnv
    let cmdData = (CommandSuccess . Number) 3
    res `shouldBe` (Right cmdData)
  it "correctly runs a simple command publicly and listens to the result" $ do
    cmd <- simpleServerCmd
    res <- runClientM (send pactServerApiClient (SubmitBatch [cmd])) clientEnv
    let rk = cmdToRequestKey cmd
    res `shouldBe` (Right (RequestKeys [rk]))
    res' <- runClientM (listen pactServerApiClient (ListenerRequest rk)) clientEnv
    let cmdData = (toJSON . CommandSuccess . Number) 3
    res' `shouldBe` (Right (ApiResult cmdData (Just 0) Nothing))
