{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ClientSpec (spec) where

import Data.Aeson
import Data.Aeson.Types (parse)
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
import Pact.Types.Runtime
import Pact.Types.PactValue


_testLogDir, _testConfigFilePath, _testPort, _serverPath :: String
_testLogDir = testDir ++ "test-log/"
_testConfigFilePath = testDir ++ "test-config.yaml"

_testPort = "8080"
_serverPath = "http://localhost:" ++ _testPort

bracket :: IO a -> IO a
bracket action = Exception.bracket
  (flushDb >> startServer _testConfigFilePath)
  (\a -> stopServer a >> flushDb)
  (const action)

simpleServerCmd :: IO (Command Text)
simpleServerCmd = do
  simpleKeys <- genKeys
  mkExec  "(+ 1 2)" Null def [simpleKeys] (Just "test1")

simpleServerCmdWithPactErr :: IO (Command Text)
simpleServerCmdWithPactErr = do
  simpleKeys <- genKeys
  mkExec  "(+ 1 2 3)" Null def [simpleKeys] (Just "test1")

spec :: Spec
spec = describe "Servant API client tests" $ do
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
    res <- bracket $! do
      r <- runClientM (local pactServerApiClient cmd) clientEnv
      return r
    let cmdPactResult = (toJSON . CommandSuccess . PLiteral . LDecimal) 3
    (_crResult' <$> res) `shouldBe` (Right cmdPactResult)

  it "correctly runs a simple command with pact error locally" $ do
    cmd <- simpleServerCmdWithPactErr
    res <- bracket $! do
      r <- runClientM (local pactServerApiClient cmd) clientEnv
      return r
    (_crResult' <$> res) `shouldSatisfy` (failWith ArgsError)

  it "correctly runs a simple command publicly and listens to the result" $ do
    cmd <- simpleServerCmd
    let rk = cmdToRequestKey cmd
    (res,res') <- bracket $! do
      !res <- runClientM (send pactServerApiClient (SubmitBatch [cmd])) clientEnv
      !res' <- runClientM (listen pactServerApiClient (ListenerRequest rk)) clientEnv
      -- print (res,res')
      return (res,res')
    res `shouldBe` (Right (RequestKeys [rk]))
    let cmdData = (toJSON . CommandSuccess . PLiteral . LDecimal) 3
    (_arResult <$> res') `shouldBe` (Right cmdData)

  it "correctly runs a simple command with pact error publicly and listens to the result" $ do
    cmd <- simpleServerCmdWithPactErr
    let rk = cmdToRequestKey cmd
    (res,res') <- bracket $! do
      !res <- runClientM (send pactServerApiClient (SubmitBatch [cmd])) clientEnv
      !res' <- runClientM (listen pactServerApiClient (ListenerRequest rk)) clientEnv
      -- print (res,res')
      return (res,res')
    res `shouldBe` (Right (RequestKeys [rk]))
    (_arResult <$> res') `shouldSatisfy` (failWith ArgsError)


failWith :: PactErrorType -> Either ServantError Value -> Bool
failWith errType res = case res of
  Left _ -> False
  Right res' -> case parse parseJSON res' of
    Success (CommandError (PactError t _ _ _)) -> t == errType
    _ -> False
