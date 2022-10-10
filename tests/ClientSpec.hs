{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import Data.Aeson
import Data.Default (def)
import Test.Hspec
import qualified Control.Exception as Exception

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)
import Pact.Server.API
import Pact.Server.Test
import Servant.Client
import Pact.Types.Runtime
import Pact.Types.PactValue
import System.IO.Temp

import Utils

#if ! MIN_VERSION_servant_client(0,16,0)
type ClientError = ServantError
#endif

testPort :: Int
testPort = 8080

serverPath :: String
serverPath = "http://localhost:" ++ show testPort

withTestConfig :: (FilePath -> IO a) -> IO a
withTestConfig inner = withSystemTempDirectory "pact-test-clientspec" $ \dir -> do
  let confFilePath = dir <> "/" <> "test-config.yaml"
  encodeFile confFilePath $ object
    [ "port" .= testPort
    , "logDir" .= dir
    , "persistDir" .= dir
    , "pragmas" .= ([] :: [String])
    , "verbose" .= False
    , "execConfig" .= ([ "DisablePact43", "DisablePact44" ] :: [String])
    ]
  inner confFilePath

bracket :: IO a -> IO a
bracket action = withTestConfig $ \fp ->
  Exception.bracket (startServer fp) stopServer (const action)

simpleServerCmd :: IO (Command Text)
simpleServerCmd = do
  simpleKeys <- genKeys
  mkExec  "(+ 1 2)" Null def [(simpleKeys,[])] Nothing (Just "test1")


simpleServerCmdWithPactErr :: IO (Command Text)
simpleServerCmdWithPactErr = do
  simpleKeys <- genKeys
  mkExec  "(+ 1 2 3)" Null def [(simpleKeys,[])] Nothing (Just "test1")

spec :: Spec
spec = describe "Servant API client tests" $
  before mkClient $ do
    -- it "incorrectly runs a simple command privately" $ do
    --   cmd <- simpleServerCmd
    --   res <- runClientM (private (SubmitBatch [cmd])) clientEnv
    --   let expt = Right (ApiFailure "Send private: payload must have address")
    --   res `shouldBe` expt
    it "correctly runs a simple command locally" $ \clientEnv -> do
      cmd <- simpleServerCmd
      res <- bracket $! runClientM (localClient cmd) clientEnv
      let cmdPactResult = (PactResult . Right . PLiteral . LDecimal) 3
      (_crResult <$> res) `shouldBe` Right cmdPactResult

    it "correctly runs a simple command with pact error locally" $ \clientEnv -> do
      cmd <- simpleServerCmdWithPactErr
      res <- bracket $! runClientM (localClient cmd) clientEnv
      (_crResult <$> res) `shouldSatisfy` failWith ArgsError

    it "correctly runs a simple command publicly and listens to the result" $ \clientEnv -> do
      cmd <- simpleServerCmd
      let rk = cmdToRequestKey cmd
      (res,res') <- bracket $! do
        !res <- runClientM (sendClient (SubmitBatch [cmd])) clientEnv
        !res' <- runClientM (listenClient (ListenerRequest rk)) clientEnv
        -- print (res,res')
        return (res,res')
      res `shouldBe` Right (RequestKeys [rk])
      let cmdData = (PactResult . Right . PLiteral . LDecimal) 3
      case res' of
        Left _ -> expectationFailure "client request failed"
        Right r -> case r of
          ListenTimeout _ -> expectationFailure "timeout"
          ListenResponse lr -> _crResult lr `shouldBe` cmdData

    it "correctly runs a simple command with pact error publicly and listens to the result" $ \clientEnv -> do
      cmd <- simpleServerCmdWithPactErr
      let rk = cmdToRequestKey cmd
      (res,res') <- bracket $! do
        !res <- runClientM (sendClient (SubmitBatch [cmd])) clientEnv
        !res' <- runClientM (listenClient (ListenerRequest rk)) clientEnv
        -- print (res,res')
        return (res,res')
      res `shouldBe` Right (RequestKeys [rk])
      case res' of
        Left _ -> expectationFailure "client request failed"
        Right r -> case r of
          ListenTimeout _ -> expectationFailure "timeout"
          ListenResponse lr -> Right (_crResult lr) `shouldSatisfy` failWith ArgsError
 where
  mkClient = mkClientEnv testMgr <$> parseBaseUrl serverPath


failWith :: PactErrorType -> Either ClientError PactResult -> Bool
failWith errType res = case res of
  Left _ -> False
  Right res' -> case res' of
    (PactResult (Left (PactError t _ _ _))) -> t == errType
    _ -> False
