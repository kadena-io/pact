{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module TypecheckSpec (spec) where

import Prelude hiding (take)

import Test.Hspec

import Control.Lens
import Control.Monad

import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Foldable
import Data.Text (Text, take, unpack)
import qualified Data.Set as Set

import Pact.Typechecker hiding (debug)
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Runtime
import Pact.Types.Typecheck
import Pact.Runtime.Utils

import Pact.Types.Pretty
import qualified Data.Text as T
import qualified Pact.Analyze.Check as Check

spec :: Spec
spec = do
  checkModule "tests/pact/caps.repl" "caps"
  checkModule "examples/cp/cp.repl" "cp"
  checkModule "tests/pact/yield.repl" "yieldtest"
  checkFun "examples/accounts/accounts.repl" "accounts" "transfer"
  checkFuns
  verifyModule "examples/cp/cp.repl" "cp"
  verifyModule "examples/verified-accounts/accounts.repl" "accounts"

type TCResult = (TopLevel Node,TcState)
type TCResultCheck a = TCResult -> ([a] -> [a] -> Expectation) -> Expectation

checkUnresolvedTys :: TCResultCheck (Type UserType)
checkUnresolvedTys (tl,_) test = getUnresolvedTys tl `test` []

getUnresolvedTys :: TopLevel Node -> [Type UserType]
getUnresolvedTys tl = filter isUnresolvedTy (map _aTy (toList tl))

checkFailures :: TCResultCheck Failure
checkFailures (_,s) test = toList (_tcFailures s) `test` []

topLevelTypechecks :: Text -> SpecWith TCResult
topLevelTypechecks n = it (unpack n ++ " typechecks") $ \r ->
  checkUnresolvedTys r shouldBe

topLevelNoFailures :: Text -> SpecWith TCResult
topLevelNoFailures n = it (unpack n ++ " has no failures") $ \r ->
  checkFailures r shouldBe

topLevelFails :: Text -> SpecWith TCResult
topLevelFails n = it (unpack n ++ " should fail") $ \r ->
  r `checkFailures` shouldNotBe

topLevelChecks :: Text -> SpecWith TCResult
topLevelChecks n = topLevelTypechecks n  >> topLevelNoFailures n

checkModule :: FilePath -> ModuleName -> Spec
checkModule fp mn = describe (fp ++ ": " ++ moduleName mn ++ " typechecks") $ do
  beforeAll (inferModule False fp mn) $ do
    it (moduleName mn ++ ": module has no failures") $ \(_, fs) ->
      map prettyFail fs `shouldBe` []
    it (moduleName mn ++ ": all toplevels typecheck") $ \(tls, _) ->
      concatMap getUnresolvedTys tls `shouldBe` []

moduleName :: ModuleName -> String
moduleName = unpack . asString

-- | Check that this module has no verification failures
verifyModule :: FilePath -> ModuleName -> Spec
verifyModule fp mn = it (fp ++ ": " ++ moduleName mn ++ " verifies") $ do
  (resultTm, replState) <- execScript' Quiet fp
  either (die def) (const (pure ())) resultTm
  eModule <- replLookupModule replState mn
  modul <- case eModule of
    Left e      -> die def $ "Module not found: " ++ show (fp,mn,e)
    Right modul -> pure modul
  mModules <- replGetModules replState
  checkResult <- case mModules of
    Left err           -> die def (show err)
    Right (modules, _) -> Check.verifyModule Nothing def (inlineModuleData <$> modules) (inlineModuleData modul)
  let ros = Check.renderVerifiedModule checkResult
  when (any ((== OutputFailure) . _roType) ros) $
    expectationFailure $ T.unpack $
      "Verification errors found: " <> T.intercalate "\n" (map renderCompactText ros)

prettyFail :: Failure -> String
prettyFail (Failure TcId{..} msg) = renderInfo _tiInfo ++ ": " ++ msg


checkFun :: FilePath -> ModuleName -> Text -> Spec
checkFun fp mn fn =
  beforeAll (inferFun False fp mn fn) $
    topLevelChecks (asString mn <> "." <> fn)


checkFuns :: Spec
checkFuns = describe "pact typecheck" $ do
  let mn = "tests/pact/tc.repl"
  -- runIO is needed here to construct the test tree
  (ModuleData _ m _) <- runIO $ inlineModuleData <$> loadModule mn "tctest"
  forM_ (HM.toList m) $ \(fn,ref) -> do
    let doTc = beforeAll (runTC 0 False (typecheckTopLevel ref))
        n = asString mn <> "." <> fn
    when (take 3 fn == "tc-") $
      doTc $ do
        topLevelChecks n
        customFunChecks n
    when (take 6 fn == "fails-") $
      doTc $ do
        topLevelTypechecks n
        topLevelFails n

customFunChecks :: Text -> SpecWith TCResult
customFunChecks name = case name of
  "tests/pact/tc.repl.tc-update-partial" -> do
    -- TODO top levels don't get inferred return type, so we have to dig in here
    it (unpack name ++ ":specializes partial type") $ \(tl, _) -> do
      shouldBe
        (preview (tlFun . fBody . _head . aNode . aTy . tySchemaPartial) tl)
        (Just $ PartialSchema $ Set.singleton "name")
  _ -> return ()

loadModule :: FilePath -> ModuleName -> IO (ModuleData Ref)
loadModule fp mn = do
  (r,s) <- execScript' Quiet fp
  either (die def) (const (return ())) r
  replLookupModule s mn >>= \mr -> case mr of
    Right m -> return m
    Left e -> die def $ "Module not found: " ++ show (fp,mn,e)

loadFun :: FilePath -> ModuleName -> Text -> IO Ref
loadFun fp mn fn = loadModule fp mn >>= \(inlineModuleData -> ModuleData _ m _) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f


inferFun :: Bool -> FilePath -> ModuleName -> Text -> IO TCResult
inferFun dbg fp mn fn = loadFun fp mn fn >>= \r -> runTC 0 dbg (typecheckTopLevel r)


inferModule :: Bool -> FilePath -> ModuleName -> IO ([TopLevel Node],[Failure])
inferModule debug fp mn = do
  md <- inlineModuleData <$> loadModule fp mn
  typecheckModule debug def md




-- _pretty =<< _inferIssue
_inferIssue :: IO TCResult
_inferIssue = inferFun True "examples/cp/cp.repl" "cp" "issue"

-- _pretty =<< _inferTransfer
_inferTransfer :: IO TCResult
_inferTransfer = inferFun True "examples/accounts/accounts.repl" "accounts" "transfer"

_inferTestModule :: IO ([TopLevel Node],[Failure])
_inferTestModule = inferModule True "tests/pact/tc.repl" "tctest"

_inferTestFun :: Text -> IO TCResult
_inferTestFun = inferFun True "tests/pact/tc.repl" "tctest"

_inferAccounts :: IO ([TopLevel Node],[Failure])
_inferAccounts = inferModule False "examples/accounts/accounts.repl" "accounts"

_inferCP :: IO ([TopLevel Node],[Failure])
_inferCP = inferModule False "examples/cp/cp.repl" "cp"

-- | prettify output of 'inferFun' runs
_pretty :: TCResult -> IO ()
_pretty (f,tc) = putDoc (pretty tc <> hardline <> hardline <> pretty f <> hardline)
