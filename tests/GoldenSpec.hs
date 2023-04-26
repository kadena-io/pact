{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GoldenSpec

  (
      spec
    , cleanupActual

  )

  where

import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Text (Text)
import System.Directory

import Test.Hspec
import Test.Hspec.Golden


import Pact.Gas
import Pact.Interpreter
import Pact.Parse
import Pact.Repl
import Pact.Repl.Types
import Pact.Server.PactService
import Pact.Types.Command
import Pact.Types.Logger
import Pact.Types.Names
import Pact.Types.Persistence
import Pact.Types.RPC
import Pact.Types.Runtime
import Pact.Types.SPV
import Pact.JSON.Legacy.Value

spec :: Spec
spec = do
  describe "goldenAccounts" $
    goldenModule [FlagDisableInlineMemCheck, FlagDisablePact43, FlagDisablePact44, FlagDisablePact47] "accounts-module" "golden/golden.accounts.repl" "accounts"
    [("successCR",acctsSuccessCR)
    ,("failureCR",acctsFailureCR)
    ,("eventCR",eventCR)
    ,("crossChainSendCR",crossChainSendCR False)
    ,("crossChainSendCRBackCompat",crossChainSendCR True)
    ]
  describe "goldenAutoCap" $
    goldenModule [FlagDisableInlineMemCheck, FlagDisablePact43, FlagDisablePact44, FlagDisablePact47] "autocap-module" "golden/golden.autocap.repl" "auto-caps-mod" []
  describe "goldenLambdas" $
    goldenModule [FlagDisableInlineMemCheck, FlagDisablePact43, FlagDisablePact44, FlagDisablePact47] "lambda-module" "golden/golden.lams.repl" "lams-test" []
  describe "goldenModuleMemcheck" $
    goldenModule [FlagDisablePact43, FlagDisablePact44, FlagDisablePact47] "goldenModuleMemCheck" "golden/golden.memcheck.repl" "memcheck" []
  describe "goldenFullyQuals" $
    goldenModule [] "goldenFullyQuals" "golden/golden.fqns.repl" "fqns" []
  describe "goldenNamespaced keysets" $
    goldenModule [] "goldenNamespacedKeysets" "golden/golden.nks.repl" "free.nks" []
  describe "golden root ns module upgrade" $
    goldenModule [] "goldenRootNamespace" "golden/golden.rootnamespace.repl" "nsupgrade" []

goldenModule
    :: [ExecutionFlag]
    -> String
    -> FilePath
    -> ModuleName
    -> [(String, String -> SpecWith ReplState)]
    -> Spec
goldenModule flags tn fp mn tests = after_ (cleanupActual tn (map fst tests)) $ do
  beforeAll loadModule $ do
    beforeAllWith lookupModule $ do
      it (fp <> " loaded module") $ \case
        Left e -> expectationFailure $ "module load failed: " <> e
        Right m -> case traverse (traverse toPersistDirect) m of
          Left e -> expectationFailure $ "failed to convert to PersistDirect: " <> show e
          Right _ -> return ()
      it (fp <> " matches golden") $ \case
        Left e -> error $ "module load failed: " <> e
        Right m -> case traverse (traverse toPersistDirect) m of
          Left e -> error $ "failed to convert to PersistDirect: " <> show e
          Right m' -> golden tn m'
    forM_ tests $ \(n,f) -> describe n $ do
      f (subTestName tn n)
 where
  ec = mkExecutionConfig flags

  loadModule :: IO ReplState
  loadModule = do
    (_,s) <- execScriptF' Quiet fp (set (rEnv . eeExecutionConfig) ec . set (rEnv . eeInRepl) False)
    return s

  lookupModule s = replLookupModule s mn

subTestName :: String -> String -> String
subTestName tn n = tn ++ "-" ++ n

acctsSuccessCR :: String -> SpecWith ReplState
acctsSuccessCR tn = doCRTest tn "1"

-- Needs disablePact44 here, accts failure cr
-- results in `interactive:0:0` which is an info that has been stripped
acctsFailureCR :: String -> SpecWith ReplState
acctsFailureCR tn = doCRTest' (mkExecutionConfig [FlagDisablePact44, FlagDisablePact47]) tn "(accounts.transfer \"a\" \"b\" 1.0 true)"

eventCR :: String -> SpecWith ReplState
eventCR tn = doCRTest' (mkExecutionConfig [FlagDisableInlineMemCheck, FlagDisablePact43, FlagDisablePact47]) tn
    "(module events-test G \
    \  (defcap G () true) \
    \  (defcap CAP (name:string amount:decimal) @managed \
    \    true) \
    \  (defun f (name:string amount:decimal) \
    \    (install-capability (CAP name amount)) \
    \    (with-capability (CAP name amount) 1))) \
    \ (events-test.f \"Alice\" 10.1)"

crossChainSendCR :: Bool -> String -> SpecWith ReplState
crossChainSendCR backCompat tn = doCRTest' (ec backCompat) tn
    "(module xchain G (defcap G () true) \
    \ (defpact p (a:integer) \
    \  (step (yield { 'a: a } \"1\")) \
    \  (step (resume { 'a:=a } a)))) \
    \(xchain.p 3)"
  where
    ec True = mkExecutionConfig [FlagDisablePact40, FlagDisableInlineMemCheck, FlagDisablePact43, FlagDisablePact47]
    ec False = mkExecutionConfig [FlagDisableInlineMemCheck, FlagDisablePact43, FlagDisablePact43, FlagDisablePact47]

doCRTest :: String -> Text -> SpecWith ReplState
doCRTest = doCRTest' def

doCRTest' :: ExecutionConfig -> String -> Text -> SpecWith ReplState
doCRTest' ec tn code = beforeAllWith initRes $

    -- StackFrame and Info have pathological instances which impacts failure JSON
    -- out of CommandResult. Therefore this golden does not ensure equality of
    -- de-serialized CRs, but instead that
    -- a) ToJSON instances are backward-compat
    -- b) "Output roundtrip": A de-serialized CR will result in the same ToJSON output.
    -- NOTE: an implication of "output roundtrip" is, on a golden failure, expected
    -- and actual are reversed, as the golden is read with 'readFromFile' which roundtrips.
    it "matches golden encoded" $ \r -> Golden
      { output = encode r
      , encodePretty = show
      , writeToFile = BL.writeFile
      , readFromFile = readOutputRoundtrip
      , testName = tn
      , directory = "golden"
      , failFirstTime = False
      }
 where
  initRes s = do
    let dbEnv = PactDbEnv (view (rEnv . eePactDb) s) (view (rEnv . eePactDbVar) s)
        cmd = Command payload [] initialHash
        payload = Payload exec "" pubMeta [] Nothing
        pubMeta = def
        parsedCode = either error id $ parsePact code
        exec = Exec $ ExecMsg parsedCode (toLegacyJson Null)
    applyCmd (newLogger neverLog "") Nothing dbEnv (constGasModel 0) 0 0 "" noSPVSupport ec Local cmd (ProcSucc cmd)

  -- hacks 'readFromFile' to run the golden value through the roundtrip.
  readOutputRoundtrip = fmap (tryEncode . eitherDecode) . BL.readFile
  tryEncode :: Either String (CommandResult Hash) -> BL.ByteString
  tryEncode (Left e) = error e
  tryEncode (Right cr) = encode cr

cleanupActual :: String -> [String] -> IO ()
cleanupActual testname subs = do
  go testname
  mapM_ (go . subTestName testname) subs
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
  , failFirstTime = False
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
