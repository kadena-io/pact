{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GoldenSpec

  (spec)

  where

import Control.Lens
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Either
import System.Directory

import Test.Hspec
import Test.Hspec.Golden


import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Names
import Pact.Types.Persistence
import Pact.Parse
import Pact.Interpreter
import Pact.Types.Runtime
import Pact.Types.Command
import Pact.Types.RPC
import Pact.Types.Logger
import Pact.Gas
import Pact.Types.SPV
import Pact.Server.PactService

spec :: Spec
spec = do
  describe "goldenAccounts" $
    goldenModule "accounts-module" "golden/golden.accounts.repl" "accounts"
    [("successCR",acctsSuccessCR)]
  describe "goldenAutoCap" $
    goldenModule "autocap-module" "golden/golden.autocap.repl" "auto-caps-mod" []

goldenModule
  :: String -> FilePath -> ModuleName -> [(String, String -> ReplState -> Spec)] -> Spec
goldenModule tn fp mn tests = after_ (cleanupActual tn (map fst tests)) $ do
  (r,s) <- runIO $ execScript' Quiet fp
  it ("loads " ++ fp) $ r `shouldSatisfy` isRight
  mr <- runIO $ replLookupModule s mn
  case mr of
    Left e -> it "module load failed" $ expectationFailure e
    Right m -> case traverse (traverse toPersistDirect) m of
      Left e -> it "failed to convert to PersistDirect" $ expectationFailure (show e)
      Right m' -> do
        it "matches golden" $ golden tn m'
        (`mapM_` tests) $ \(n,f) -> do
          describe n $ f (subTestName tn n) s

subTestName :: String -> String -> String
subTestName tn n = tn ++ "-" ++ n

acctsSuccessCR :: String -> ReplState -> Spec
acctsSuccessCR tn s = do
  let dbEnv = PactDbEnv (view (rEnv . eePactDb) s) (view (rEnv . eePactDbVar) s)
      cmd = Command payload [] initialHash
      payload = Payload exec "" pubMeta [] Nothing
      pubMeta = def
      parsedCode = either error id $ parsePact "1"
      exec = Exec $ ExecMsg parsedCode Null
  r <- runIO $ applyCmd (newLogger neverLog "") Nothing dbEnv (constGasModel 0) 0 0 "" noSPVSupport Local cmd (ProcSucc cmd)
  it "matches golden" $ golden tn r


cleanupActual :: String -> [String] -> IO ()
cleanupActual testname subs = do
  go testname
  mapM_ (\n -> go (subTestName testname n)) subs
  where
    go tn = catch (removeFile $ "golden/" ++ tn ++ "/actual")
            (\(_ :: SomeException) -> return ())


golden :: (Show a,FromJSON a,ToJSON a) => String -> a -> Golden a
golden name obj = Golden
  { output = obj
  , encodePretty = elide . show
  , writeToFile = jsonEncode
  , readFromFile = jsonDecode
  , testName = name
  , directory = "golden"
  }
  where
    elide s | length s < 256 = s
            | otherwise = take 256 s ++ "..."
    jsonEncode fp = BL.writeFile fp . encode
    jsonDecode fp = do
      r <- eitherDecode <$> BL.readFile fp
      case r of
        Left e -> throwIO $ userError $ "golden decode failed: " ++ show e
        Right v -> return v
