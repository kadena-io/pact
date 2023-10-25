{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec (spec) where

import Data.Aeson
import Data.Default (def)
import Test.Hspec

import Pact.ApiReq
import Pact.Types.API
import Pact.Types.Command
import Data.Text (Text)
import Pact.Server.API
import Servant.Client
import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Crypto (genKeyPair)

import Utils

simpleServerCmd :: IO (Command Text)
simpleServerCmd = do
  simpleKeys <- genKeyPair
  mkExec  "(+ 1 2)" Null def [(simpleKeys,[])] Nothing (Just "test1")


simpleServerCmdWithPactErr :: IO (Command Text)
simpleServerCmdWithPactErr = do
  simpleKeys <- genKeyPair
  mkExec  "(+ 1 2 3)" Null def [(simpleKeys,[])] Nothing (Just "test1")

spec :: Spec
spec = describe "Servant API client tests" $ do
    -- it "incorrectly runs a simple command privately" $ do
    --   cmd <- simpleServerCmd
    --   res <- runClientM (private (SubmitBatch [cmd])) clientEnv
    --   let expt = Right (ApiFailure "Send private: payload must have address")
    --   res `shouldBe` expt

    it "correctly runs a simple command locally" $ do
      cmd <- simpleServerCmd
      res <- withTestPactServer "clientspec" $ \clientEnv -> do
        runClientM (localClient cmd) clientEnv
      let cmdPactResult = (PactResult . Right . PLiteral . LDecimal) 3
      (_crResult <$> res) `shouldBe` Right cmdPactResult

    it "correctly runs a simple command with pact error locally" $ do
      cmd <- simpleServerCmdWithPactErr
      res <- withTestPactServer "clientspec" $ \clientEnv -> do
        runClientM (localClient cmd) clientEnv
      (_crResult <$> res) `shouldSatisfy` failWith ArgsError

    it "correctly runs a simple command publicly and listens to the result" $ do
      cmd <- simpleServerCmd
      let rk = cmdToRequestKey cmd
      (res,res') <- withTestPactServer "clientspec" $ \clientEnv -> do
        !res <- runClientM (sendClient (SubmitBatch [cmd])) clientEnv
        !res' <- runClientM (listenClient (ListenerRequest rk)) clientEnv
        return (res,res')
      res `shouldBe` Right (RequestKeys [rk])
      let cmdData = (PactResult . Right . PLiteral . LDecimal) 3
      case res' of
        Left _ -> expectationFailure "client request failed"
        Right r -> case r of
          ListenTimeout _ -> expectationFailure "timeout"
          ListenResponse lr -> _crResult lr `shouldBe` cmdData

    it "correctly runs a simple command with pact error publicly and listens to the result" $ do
      cmd <- simpleServerCmdWithPactErr
      let rk = cmdToRequestKey cmd
      (res,res') <- withTestPactServer "clientspec" $ \clientEnv -> do
        !res <- runClientM (sendClient (SubmitBatch [cmd])) clientEnv
        !res' <- runClientM (listenClient (ListenerRequest rk)) clientEnv
        return (res,res')
      res `shouldBe` Right (RequestKeys [rk])
      case res' of
        Left _ -> expectationFailure "client request failed"
        Right r -> case r of
          ListenTimeout _ -> expectationFailure "timeout"
          ListenResponse lr -> Right (_crResult lr) `shouldSatisfy` failWith ArgsError

failWith :: PactErrorType -> Either ClientError PactResult -> Bool
failWith errType res = case res of
  Left _ -> False
  Right res' -> case res' of
    (PactResult (Left (PactError t _ _ _))) -> t == errType
    _ -> False
