{-# LANGUAGE OverloadedStrings #-}
module TypecheckSpec where

import Test.Hspec
import Pact.Typechecker
import Pact.Repl
import Pact.Types.Runtime
import Pact.Types.Typecheck
import Data.Default
import Control.Lens
import qualified Data.HashMap.Strict as HM
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(<$$>),(<>))
import Control.Monad
import Data.Foldable

spec :: Spec
spec = do
  void $ runIO $ inferModule "tests/pact/tc.repl" "tctest"
  void $ runIO $ inferModule "examples/cp/cp.repl" "cp"
  void $ runIO $ inferModule "examples/accounts/accounts.repl" "accounts"
  checkFuns

topLevelChecks :: Text -> (TopLevel Node,TcState) -> Spec
topLevelChecks n (tl,s) = do
  it (unpack n ++ " typechecks") $
    filter isUnresolvedTy (map _aTy (toList tl)) `shouldBe` []
  it (unpack n ++ " has no failures") $ toList (_tcFailures s) `shouldBe` []

checkFun :: FilePath -> ModuleName -> Text -> Spec
checkFun fp mn fn = do
  r <- runIO $ inferFun False fp mn fn
  topLevelChecks (asString mn <> "." <> fn) r

checkFuns :: Spec
checkFuns = describe "tc.pact typecheck" $ do
  checkFun "tests/pact/tc.repl" "tctest" "add-person"
  checkFun "tests/pact/tc.repl" "tctest" "update-age"
  checkFun "tests/pact/tc.repl" "tctest" "unconsumed-app-typevar"
  checkFun "tests/pact/tc.repl" "tctest" "will-overload-conflict"
  checkFun "tests/pact/tc.repl" "tctest" "at-typed-object"
  checkFun "tests/pact/tc.repl" "tctest" "at-typed-list"
  checkFun "tests/pact/tc.repl" "tctest" "adults"
  checkFun "tests/pact/tc.repl" "tctest" "filter-map"
  checkFun "tests/pact/tc.repl" "tctest" "map1"
  checkFun "tests/pact/tc.repl" "tctest" "fold1"
  checkFun "examples/cp/cp.repl" "cp" "issue"
  checkFun "examples/accounts/accounts.repl" "accounts" "transfer"

loadModule :: FilePath -> ModuleName -> IO ModuleData
loadModule fp mn = do
  (r,s) <- execScript' (Script fp) fp
  either (die def) (const (return ())) r
  case view (rEnv . eeRefStore . rsModules . at mn) s of
    Just m -> return m
    Nothing -> die def $ "Module not found: " ++ show (fp,mn)

loadFun :: FilePath -> ModuleName -> Text -> IO Ref
loadFun fp mn fn = loadModule fp mn >>= \(_,m) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f


inferFun :: Bool -> FilePath -> ModuleName -> Text -> IO (TopLevel Node, TcState)
inferFun dbg fp mn fn = loadFun fp mn fn >>= \r -> runTC 0 dbg (typecheckTopLevel r)


inferModule :: FilePath -> ModuleName -> IO [TopLevel Node]
inferModule fp mn = do
  md <- loadModule fp mn
  (tls,fails) <- typecheckModule False md
  forM_ fails print
  return tls




-- _pretty =<< _inferIssue
_inferIssue :: IO (TopLevel Node, TcState)
_inferIssue = inferFun True "examples/cp/cp.repl" "cp" "issue"

-- _pretty =<< _inferTransfer
_inferTransfer :: IO (TopLevel Node, TcState)
_inferTransfer = inferFun True "examples/accounts/accounts.repl" "accounts" "transfer"

_inferTestModule :: IO [TopLevel Node]
_inferTestModule = inferModule "tests/pact/tc.repl" "tctest"

_inferAccounts :: IO [TopLevel Node]
_inferAccounts = inferModule "examples/accounts/accounts.repl" "accounts"

_inferCP :: IO [TopLevel Node]
_inferCP = inferModule "examples/cp/cp.repl" "cp"

-- | prettify output of 'inferFun' runs
_pretty :: (TopLevel Node, TcState) -> IO ()
_pretty (f,tc) = putDoc (pretty tc <> hardline <> hardline <> pretty f <> hardline)
