{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GoldenSpec

  (
      spec
    , golden
    , cleanupActual

  )

  where

import Control.Exception
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default
import Data.Either
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

spec :: Spec
spec = do
  describe "goldenAccounts" $
    goldenModule "accounts-module" "golden/golden.accounts.repl" "accounts"
    [("successCR",acctsSuccessCR)
    ,("failureCR",acctsFailureCR)
    ,("eventCR",eventCR)
    ,("crossChainSendCR",crossChainSendCR False)
    ,("crossChainSendCRBackCompat",crossChainSendCR True)
    ]
  describe "goldenAutoCap" $
    goldenModule "autocap-module" "golden/golden.autocap.repl" "auto-caps-mod" []
  describe "goldenLambdas" $
    goldenModule "lambda-module" "golden/golden.lams.repl" "lams-test" []

goldenModule
  :: String -> FilePath -> ModuleName -> [(String, String -> ReplState -> Spec)] -> Spec
goldenModule tn fp mn tests = after_ (cleanupActual tn (map fst tests)) $ do
  let ec = mkExecutionConfig [FlagDisableInlineMemCheck]
  (r,s) <- runIO $ execScriptF' Quiet fp (\st -> st & rEnv . eeExecutionConfig .~ ec)
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
acctsSuccessCR tn s = doCRTest tn s "1"

acctsFailureCR :: String -> ReplState -> Spec
acctsFailureCR tn s = doCRTest tn s "(accounts.transfer \"a\" \"b\" 1.0 true)"

eventCR :: String -> ReplState -> Spec
eventCR tn s = doCRTest' (mkExecutionConfig [FlagDisableInlineMemCheck]) tn s $
    "(module events-test G \
    \  (defcap G () true) \
    \  (defcap CAP (name:string amount:decimal) @managed \
    \    true) \
    \  (defun f (name:string amount:decimal) \
    \    (install-capability (CAP name amount)) \
    \    (with-capability (CAP name amount) 1))) \
    \ (events-test.f \"Alice\" 10.1)"

crossChainSendCR :: Bool -> String -> ReplState -> Spec
crossChainSendCR backCompat tn s = doCRTest' (ec backCompat) tn s $
    "(module xchain G (defcap G () true) \
    \ (defpact p (a:integer) \
    \  (step (yield { 'a: a } \"1\")) \
    \  (step (resume { 'a:=a } a)))) \
    \(xchain.p 3)"
  where
    ec True = mkExecutionConfig [FlagDisablePact40, FlagDisableInlineMemCheck]
    ec False = mkExecutionConfig [FlagDisableInlineMemCheck]

doCRTest :: String -> ReplState -> Text -> Spec
doCRTest tn s code = doCRTest' def tn s code

doCRTest' :: ExecutionConfig -> String -> ReplState -> Text -> Spec
doCRTest' ec tn s code = do
  let dbEnv = PactDbEnv (view (rEnv . eePactDb) s) (view (rEnv . eePactDbVar) s)
      cmd = Command payload [] initialHash
      payload = Payload exec "" pubMeta [] Nothing
      pubMeta = def
      parsedCode = either error id $ parsePact code
      exec = Exec $ ExecMsg parsedCode Null
  r <- runIO $ applyCmd (newLogger neverLog "") Nothing dbEnv (constGasModel 0) 0 0 ""
       noSPVSupport ec Local cmd (ProcSucc cmd)
  -- StackFrame and Info have pathological instances which impacts failure JSON
  -- out of CommandResult. Therefore this golden does not ensure equality of
  -- de-serialized CRs, but instead that
  -- a) ToJSON instances are backward-compat
  -- b) "Output roundtrip": A de-serialized CR will result in the same ToJSON output.
  -- NOTE: an implication of "output roundtrip" is, on a golden failure, expected
  -- and actual are reversed, as the golden is read with 'readFromFile' which roundtrips.
  it "matches golden encoded" $ Golden
    { output = encode r
    , encodePretty = show
    , writeToFile = BL.writeFile
    , readFromFile = readOutputRoundtrip
    , testName = tn
    , directory = "golden"
    , failFirstTime = False
    }
  where
    -- hacks 'readFromFile' to run the golden value through the roundtrip.
    readOutputRoundtrip = fmap (tryEncode . eitherDecode) . BL.readFile
    tryEncode :: Either String (CommandResult Hash) -> BL.ByteString
    tryEncode (Left e) = error e
    tryEncode (Right cr) = encode cr


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
