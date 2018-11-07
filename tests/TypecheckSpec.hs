{-# LANGUAGE OverloadedStrings #-}
module TypecheckSpec (spec) where

import Test.Hspec
import Pact.Typechecker hiding (debug)
import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Runtime
import Pact.Types.Typecheck
import Data.Default
import Control.Lens
import qualified Data.HashMap.Strict as HM
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Control.Monad
import Data.Foldable
import qualified Data.Text as T

spec :: Spec
spec = do
  void $ runIO $ inferModule False "tests/pact/tc.repl" "tctest"
  void $ runIO $ inferModule False "examples/cp/cp.repl" "cp"
  void $ runIO $ inferModule False "examples/accounts/accounts.repl" "accounts"
  checkFuns

type TCResult = (TopLevel Node,TcState)
type TCResultCheck a = TCResult -> ([a] -> [a] -> Expectation) -> Expectation

checkUnresolvedTys :: TCResultCheck (Type UserType)
checkUnresolvedTys (tl,_) test = filter isUnresolvedTy (map _aTy (toList tl)) `test` []

checkFailures :: TCResultCheck Failure
checkFailures (_,s) test = toList (_tcFailures s) `test` []

topLevelTypechecks :: Text -> TCResult -> Spec
topLevelTypechecks n r = it (unpack n ++ " typechecks") $ checkUnresolvedTys r shouldBe

topLevelNoFailures :: Text -> TCResult -> Spec
topLevelNoFailures n r =
  it (unpack n ++ " has no failures") $ checkFailures r shouldBe

topLevelFails :: Text -> TCResult -> Spec
topLevelFails n r =
  it (unpack n ++ " should fail") $ checkFailures r shouldNotBe

topLevelChecks :: Text -> TCResult -> Spec
topLevelChecks n r = topLevelTypechecks n r >> topLevelNoFailures n r

checkFun :: FilePath -> ModuleName -> Text -> Spec
checkFun fp mn fn = do
  r <- runIO $ inferFun False fp mn fn
  topLevelChecks (asString mn <> "." <> fn) r

checkFuns :: Spec
checkFuns = describe "pact typecheck" $ do
  let mn = "tests/pact/tc.repl"
  (ModuleData _ m) <- runIO $ loadModule mn "tctest"
  forM_ (HM.toList m) $ \(fn,ref) -> do
    let doTc = runIO $ runTC 0 False (typecheckTopLevel ref)
        n = asString mn <> "." <> fn
    when (T.take 3 fn == "tc-") $
      doTc >>= topLevelChecks n
    when (T.take 6 fn == "fails-") $
      doTc >>= \r -> do
        topLevelTypechecks n r
        topLevelFails n r

  checkFun "examples/cp/cp.repl" "cp" "issue"
  checkFun "examples/accounts/accounts.repl" "accounts" "transfer"

loadModule :: FilePath -> ModuleName -> IO ModuleData
loadModule fp mn = do
  (r,s) <- execScript' Quiet fp
  either (die def) (const (return ())) r
  case view (rEnv . eeRefStore . rsModules . at mn) s of
    Just m -> return m
    Nothing -> die def $ "Module not found: " ++ show (fp,mn)

loadFun :: FilePath -> ModuleName -> Text -> IO Ref
loadFun fp mn fn = loadModule fp mn >>= \(ModuleData _ m) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f


inferFun :: Bool -> FilePath -> ModuleName -> Text -> IO TCResult
inferFun dbg fp mn fn = loadFun fp mn fn >>= \r -> runTC 0 dbg (typecheckTopLevel r)


inferModule :: Bool -> FilePath -> ModuleName -> IO [TopLevel Node]
inferModule debug fp mn = do
  md <- loadModule fp mn
  fst <$> typecheckModule debug md




-- _pretty =<< _inferIssue
_inferIssue :: IO TCResult
_inferIssue = inferFun True "examples/cp/cp.repl" "cp" "issue"

-- _pretty =<< _inferTransfer
_inferTransfer :: IO TCResult
_inferTransfer = inferFun True "examples/accounts/accounts.repl" "accounts" "transfer"

_inferTestModule :: IO [TopLevel Node]
_inferTestModule = inferModule True "tests/pact/tc.repl" "tctest"

_inferTestFun :: Text -> IO TCResult
_inferTestFun = inferFun True "tests/pact/tc.repl" "tctest"

_inferAccounts :: IO [TopLevel Node]
_inferAccounts = inferModule False "examples/accounts/accounts.repl" "accounts"

_inferCP :: IO [TopLevel Node]
_inferCP = inferModule False "examples/cp/cp.repl" "cp"

-- | prettify output of 'inferFun' runs
_pretty :: TCResult -> IO ()
_pretty (f,tc) = PP.putDoc (PP.pretty tc <> PP.hardline <> PP.hardline <> PP.pretty f <> PP.hardline)
