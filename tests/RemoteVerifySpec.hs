{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests remote verification on the server side (i.e. no GHCJS involvement)
module RemoteVerifySpec (spec) where

import Test.Hspec

import Control.Lens
import Control.Monad ((>=>))
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Data.Bifunctor (first)
import Data.Either
import Data.Text (Text, unpack)

import NeatInterpolation (text)

import Servant.Client

import Pact.Analyze.Remote.Server (withTestServantServer)
import qualified Pact.Analyze.Remote.Types as Remote
import Pact.Repl
import Pact.Repl.Types
import Pact.Server.API
import Pact.Types.Runtime

import Utils

spec :: Spec
spec = do
  describe "single module"                       testSingleModule
  describe "multiple modules, sent out of order" testUnsortedModules

data TestFailure
  = ReplError String
  deriving Show

loadCode :: Text -> IO (Either TestFailure ReplState)
loadCode code = do
  replState0 <- initReplState StringEval (Just "http://localhost:3000")
  (eTerm, replState) <- runStateT (evalRepl' $ unpack code) replState0
  pure $ case eTerm of
    Left err -> Left $ ReplError err
    Right _t -> Right replState

stateModuleData :: ModuleName -> ReplState -> IO (Either String (ModuleData Ref))
stateModuleData nm replState = replLookupModule replState nm

serveAndRequest :: Remote.Request -> IO (Either ClientError Remote.Response)
serveAndRequest body =
  withTestServantServer $ \port -> do
    verifyBaseUrl <- parseBaseUrl $ "http://localhost:" ++ show port
    runClientM (verifyClient body) (mkClientEnv testMgr verifyBaseUrl)

testSingleModule :: Spec
testSingleModule = beforeAll load $ do
  it "loads locally" $
    stateModuleData "mod1" >=> (`shouldSatisfy` isRight)

  it "verifies over the network" $ \replState0 -> do
    (ModuleData mod1 _refs _) <- either error id <$> stateModuleData "mod1" replState0
    resp <- serveAndRequest $ Remote.Request [derefDef <$> mod1] "mod1"
    fmap (view Remote.responseLines) resp `shouldBe` Right []
 where
   load = either (error.show) id <$> loadCode
    [text|
      (env-exec-config ["DisablePact44"])
      (env-keys ["admin"])
      (env-data { "keyset": { "keys": ["admin"], "pred": "=" } })
      (begin-tx)

      (define-keyset 'ks (read-keyset "keyset"))
      (module mod1 'ks
        (defun f:integer ()
          @doc   "always returns 1"
          @model [(property (= result 1))]
          1))

      (commit-tx)
    |]

testUnsortedModules :: Spec
testUnsortedModules = beforeAll load $ do

  it "loads when topologically sorted locally" $
    stateModuleData "mod2" >=> (`shouldSatisfy` isRight)

  it "verifies over the network" $ \replState0  -> do
    resp <- runExceptT $ do
      ModuleData mod1 _refs _ <- ExceptT $ stateModuleData "mod1" replState0
      ModuleData mod2 _refs _ <- ExceptT $ stateModuleData "mod2" replState0
      ExceptT . fmap (first show) . serveAndRequest $
        Remote.Request [derefDef <$> mod2, derefDef <$> mod1] "mod2"
    fmap (view Remote.responseLines) resp `shouldBe` Right []
 where
  load = either (error . show) id <$> loadCode
    [text|
      (env-exec-config ["DisablePact44"])
      (env-keys ["admin"])
      (env-data { "keyset": { "keys": ["admin"], "pred": "=" } })
      (begin-tx)
      (define-keyset 'ks (read-keyset "keyset"))
      (module mod1 'ks
        (defun f:integer ()
          @doc   "always returns 1"
          @model [(property (= result 1))]
          1))
      (commit-tx)
      (begin-tx)
      (define-keyset 'ks2 (read-keyset "keyset"))
      (module mod2 'ks2
        (use mod1)
        (defun g:integer ()
          @doc   "always returns 2"
          @model [(property (= result 2))]
          2))
      (commit-tx)
    |]
