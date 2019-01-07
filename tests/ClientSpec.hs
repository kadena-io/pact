{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module ClientSpec (spec) where

import Data.Aeson
import Data.Default (def)
import Test.Hspec
import NeatInterpolation (text)
import Utils.TestRunner
import qualified Network.HTTP.Client as HTTP
import qualified Control.Exception as Exception

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)
import Pact.Server.Client
import Servant.Client
import qualified Data.HashMap.Strict as HM

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

singleModule :: Value
singleModule = (Object (HM.fromList
  [ ("verify",(Object (HM.fromList [("namespace", Null), ("name", String "helloWorld")])))
  , ("modules",Array [Object (HM.fromList
    [("hash",String "5a05b07162915175823e44ec80e134966e12233e0ec0ce1f156241e2f64ed828be0a7c2e267784f4846046e84df80fb8e64f8530b5a6eed4952fe9e9cd4b61a1")
    ,("blessed",Array [])
    ,("keyset",String "admin-keyset")
    ,("interfaces",Array [])
    ,("name",(Object (HM.fromList [("namespace", Null), ("name", String "helloWorld")])))
    ,("code",String [text|
    (module helloWorld 'admin-keyset
      "A smart contract to greet the world."
      (defun hello (name)
        "Do the hello-world dance"
        (format "Hello {}!" [name]))
    )
    |])
    ,("meta",Object (HM.fromList
      [ ("model",Array [])
      , ("docs",String "A smart contract to greet the world.")
      ]))
    ])
  ])]))

spec :: Spec
spec = around_ bracket $ describe "Servant API client tests" $ do
  mgr <- runIO $ HTTP.newManager HTTP.defaultManagerSettings
  url <- runIO $ parseBaseUrl _serverPath
  let clientEnv = mkClientEnv mgr url
  it "correctly runs a simple command publicly" $ do
    cmd <- simpleServerCmd
    res <- runClientM (send (SubmitBatch [cmd])) clientEnv
    res `shouldBe` (Right (ApiSuccess (RequestKeys [(cmdToRequestKey cmd)])))
  -- it "incorrectly runs a simple command privately" $ do
  --   cmd <- simpleServerCmd
  --   res <- runClientM (private (SubmitBatch [cmd])) clientEnv
  --   let expt = Right (ApiFailure "Send private: payload must have address")
  --   res `shouldBe` expt
  it "correctly runs a simple command publicly and polls the result" $ do
    cmd <- simpleServerCmd
    res <- runClientM (send (SubmitBatch [cmd])) clientEnv
    let rk = cmdToRequestKey cmd
    res `shouldBe` (Right (ApiSuccess (RequestKeys [rk])))
    res' <- runClientM (poll (Poll [rk])) clientEnv
    let cmdData = (toJSON . CommandSuccess . Number) 3
    let expt = Right (ApiSuccess (PollResponses
            (HM.singleton rk (ApiResult cmdData (Just 0) Nothing))))
    res' `shouldBe` expt
  it "correctly runs a simple command publicly and listens to the result" $ do
    cmd <- simpleServerCmd
    res <- runClientM (send (SubmitBatch [cmd])) clientEnv
    let rk = cmdToRequestKey cmd
    res `shouldBe` (Right (ApiSuccess (RequestKeys [rk])))
    res' <- runClientM (listen (ListenerRequest rk)) clientEnv
    let cmdData = (toJSON . CommandSuccess . Number) 3
    res' `shouldBe` (Right (ApiSuccess (ApiResult cmdData (Just 0) Nothing)))
  it "correctly runs a simple command locally" $ do
    cmd <- simpleServerCmd
    res <- runClientM (local cmd) clientEnv
    let cmdData = (CommandSuccess . Number) 3
    res `shouldBe` (Right (ApiSuccess cmdData))
  it "correctly verifies a property" $ do
    res <- runClientM (verify singleModule) clientEnv
    let expt = Right (Object (HM.singleton "output"
            (Array ["<interactive>:3:16:Warning: Unable to resolve type\n"])))
    res `shouldBe` expt
