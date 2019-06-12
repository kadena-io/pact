{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Pact.Types.Pretty

spec :: Spec
spec = do
  checkModule "tests/pact/caps.repl" "caps"
  checkModule "examples/cp/cp.repl" "cp"
  checkModule "tests/pact/yield.repl" "yieldtest"
  checkFun "examples/accounts/accounts.repl" "accounts" "transfer"
  checkFuns

type TCResult = (TopLevel Node,TcState)
type TCResultCheck a = TCResult -> ([a] -> [a] -> Expectation) -> Expectation

checkUnresolvedTys :: TCResultCheck (Type UserType)
checkUnresolvedTys (tl,_) test = getUnresolvedTys tl `test` []

getUnresolvedTys :: TopLevel Node -> [Type UserType]
getUnresolvedTys tl = filter isUnresolvedTy (map _aTy (toList tl))

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

checkModule :: FilePath -> ModuleName -> Spec
checkModule fp mn = describe (fp ++ ": " ++ show mn ++ " typechecks") $ do
  (tls,fs) <- runIO $ inferModule False fp mn
  it (show mn ++ ": module has no failures") $ map prettyFail fs `shouldBe` []
  it (show mn ++ ": all toplevels typecheck") $ concatMap getUnresolvedTys tls `shouldBe` []

prettyFail :: Failure -> String
prettyFail (Failure TcId{..} msg) = renderInfo _tiInfo ++ ": " ++ msg


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
    when (take 3 fn == "tc-") $
      doTc >>= \r -> do
      topLevelChecks n r
      customFunChecks n r
    when (take 6 fn == "fails-") $
      doTc >>= \r -> do
        topLevelTypechecks n r
        topLevelFails n r


customFunChecks :: Text -> TCResult -> Spec
customFunChecks name (tl,_) = case name of
  "tests/pact/tc.repl.tc-update-partial" -> do
    -- TODO top levels don't get inferred return type, so we have to dig in here
    it (show name ++ ":specializes partial type") $
      preview (tlFun . fBody . _head . aNode . aTy . tySchemaPartial) tl
        `shouldBe`
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
loadFun fp mn fn = loadModule fp mn >>= \(ModuleData _ m) -> case HM.lookup fn m of
  Nothing -> die def $ "Function not found: " ++ show (fp,mn,fn)
  Just f -> return f


inferFun :: Bool -> FilePath -> ModuleName -> Text -> IO TCResult
inferFun dbg fp mn fn = loadFun fp mn fn >>= \r -> runTC 0 dbg (typecheckTopLevel r)


inferModule :: Bool -> FilePath -> ModuleName -> IO ([TopLevel Node],[Failure])
inferModule debug fp mn = do
  md <- loadModule fp mn
  typecheckModule debug md




-- _pretty =<< _inferIssue
_inferIssue :: IO TCResult
_inferIssue = inferFun True "examples/cp/cp.repl" "cp" "issue"

-- _pretty =<< _inferTransfer
_inferTransfer :: IO TCResault
_inferTransfer = inferFun True "examples/accounts/accounts.repl" "accounts" "transfer"

_inferTestModule :: IO ([TopLevel Node],[Failure])
_inferTestModule = inferModule True "tests/pact/tc.repl" "tctest"

_inferTestFun :: Text -> IO TCResult
_inferTestFun = inferFun True "tests/pact/tc.repl" "tctest"

_inferAccounts :: IO ([TopLevel Node],[Failure])
_inferAccounts = inferModule False "examples/accounts/accounts.repl" "accounts"

_inferCP :: IO ([TopLevel Node],[Failure])
_inferCP = inferModule False "examples/cp/cp.repl" "cp"

_inferYield :: IO ([TopLevel Node], [Failure])
_inferYield = inferModule True "tests/pact/yield.repl" "yieldtest"

-- | prettify output of 'inferFun' runs
_pretty :: TCResult -> IO ()
_pretty (f,tc) = putDoc (pretty tc <> hardline <> hardline <> pretty f <> hardline)
