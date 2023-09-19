{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module PactTestsSpec (spec) where

import Test.Hspec

import Control.Concurrent
import Control.Monad (forM_)
import Control.Monad.State.Strict
import Control.Lens
import Data.Text(Text)
import Data.Either (isLeft, isRight)
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Data.Set as S

import Pact.Repl
import Pact.Repl.Lib
import Pact.Repl.Types
import Pact.Types.Logger
import Pact.Types.Runtime
import Pact.Persist.SQLite as SQLite
import Pact.Interpreter
import Pact.Parse (parsePact, legacyParsePact)

import System.Directory
import System.FilePath

spec :: Spec
spec = do
  tests <- runIO findTests
  pactTests tests
  badTests
  accountsTest
  cpTest
  verifiedAccountsTest
  prodParserTests tests
  legacyProdParserTests tests
  versionedNativesTests


pactTests :: [FilePath] -> Spec
pactTests tests = describe "pact tests" $ mapM_ runScript tests

badTests :: Spec
badTests = do
  describe "bad pact tests" $ do
    badFs <- runIO $ findTests' ("tests" </> "pact" </> "bad")
    forM_ badFs runBadScript

accountsTest :: Spec
accountsTest = describe "accounts regression" $ runScript ("examples" </> "accounts" </> "accounts.repl")

cpTest :: Spec
cpTest = describe "cp regression" $ runScript ("examples" </> "cp" </> "cp.repl")

verifiedAccountsTest :: Spec
verifiedAccountsTest = describe "verified accounts regression" $
  runScript ("examples" </> "verified-accounts" </> "accounts.repl")


findTests :: IO [FilePath]
findTests = findTests' $ "tests" </> "pact"

findTests' :: FilePath -> IO [FilePath]
findTests' tdir = (map (tdir </>) . filter ((== ".repl") . reverse . take 5 . reverse)) <$> getDirectoryContents tdir

runScript :: String -> SpecWith ()
runScript fp = it fp $ do
  (PactDbEnv _ pdb) <- mkSQLiteEnv (newLogger neverLog "") False (SQLiteConfig "" []) neverLog
  ls <- initLibState' (LibDb pdb) Nothing
  rs <- initReplState' ls Quiet
  (r, ReplState{..}) <- execScriptState' fp rs id
  case r of
    Left e -> expectationFailure e
    Right _ -> do
      LibState{..} <- readMVar $ _eePactDbVar _rEnv
      forM_ _rlsTests $ \TestResult {..} -> case trFailure of
        Nothing -> return ()
        Just (i,e) -> expectationFailure $ renderInfo (_faInfo i) ++ ": " ++ unpack e

runBadScript :: String -> SpecWith ()
runBadScript fp = describe ("bad-" ++ fp) $ do
  beforeAll prep $ do
    it "has error in badErrors" $ \(_, expectedError) ->
        expectedError `shouldSatisfy` isJust
    it "failed as expected" $ \(r, expectedError) ->
      r `shouldSatisfy` isCorrectError expectedError
 where
  prep = do
   (r,ReplState{}) <- execScript' Quiet fp
   return (r, M.lookup fp badErrors)

isCorrectError :: Maybe String -> Either String (Term Name) -> Bool
isCorrectError Nothing _ = False
isCorrectError _ Right {} = False
isCorrectError (Just err) (Left e) = err `isInfixOf` e

-- | Pair file path with snippet of error string
badErrors :: M.Map FilePath String
badErrors = M.fromList
  [(pfx "bad-import-deftable.repl",
    "invalid import")
  ,(pfx "bad-import-defcap.repl",
    "cannot import capabilities")
  ,(pfx "bad-pact.repl",
    "rollbacks aren't allowed on the last step")
  ,(pfx "bad-parens.repl",
    "error: expected")
  ,(pfx "bad-import-unimported-reference.repl",
    "Expected def")
  ,(pfx "bad-root-namespace.repl",
    "Definitions in default namespace are not authorized")
  ,(pfx "bad-root-namespace-44.repl",
    "Definitions in default namespace are not authorized")
  ,(pfx "bad-root-namespace-upgrade.repl",
    "Definitions in default namespace are not authorized")
  ,(pfx "bad-dupe-def.repl"
   ,"definition name conflict")
  ,(pfx "bad-modules-disabled.repl"
   ,"Module/interface install not supported")
  ,(pfx "bad-ns-def.repl"
   ,"invalid namespace name format")
  ,(pfx "bad-defcap-explicit-mgr-auto-impl.repl"
   ,"Defmeta mismatch with I: found @managed, expected @managed b")
  ,(pfx "bad-namespace-upgrade.repl"
   ,"autonomous")
  ,(pfx "bad-modrefs.repl"
   ,"Expected: qualified module name reference")
  ,(pfx "bad-modrefs-empty.repl"
   ,"Unexpected end of input")
  ,(pfx "bad-repl-native.repl"
   ,"module restore failed: Native lookup failed")
  ,(pfx "bad-import-wrong-hash.repl"
   ,"does not match specified hash")
  ,(pfx "bad-module-enforce-ns-user.repl"
   ,"Keyset failure")
  ,(pfx "bad-iface-enforce-ns-user.repl"
   ,"Keyset failure")
  ,(pfx "bad-term-in-list.repl"
   ,"Expected: value level form")
  ,(pfx "bad-ns-entry-module.repl"
   ,"Keyset failure")
  ,(pfx "bad-ns-entry-iface.repl"
   ,"Keyset failure")
  ,(pfx "bad-magic-module-keyset-install.repl"
   ,"Keyset failure (keys-all): 'ns.magic")
  ,(pfx "bad-magic-module-keyset-upgrade.repl"
   ,"Keyset failure (keys-all): 'ns.magic")
  ,(pfx "bad-gov-cap-acquire.repl"
   ,"Keyset failure (keys-all): [gov]")

  ]
  where
    pfx = ("tests/pact/bad/" ++)

-- ghci utility to load a string and get the refmap
_evalRefMap
  :: String -> IO (HM.HashMap ModuleName (ModuleData Ref, Bool))
_evalRefMap cmd = fmap (_rsLoadedModules . _evalRefs . _rEvalState . snd)
  (initReplState Quiet Nothing >>= runStateT (evalRepl' cmd))

-- -------------------------------------------------------------------------- --
-- Production Parser Tests

prodParserTests :: [FilePath] -> Spec
prodParserTests tests =
  describe "test production parser" $
    forM_ tests $ \fp ->
        checkProdParser (notElem fp badParserTests) fp
 where
  badParserTests =
   [ "tests/pact/bad/bad-parens.repl"
   ]

legacyProdParserTests :: [FilePath] -> Spec
legacyProdParserTests =
  describe "test legacy production parser" . mapM_ (checkLegacyProdParser True)

checkProdParser :: Bool -> String -> SpecWith ()
checkProdParser expectSuccess fp = describe fp $ do
  if expectSuccess
    then it "parsing succeeds" $ do
      pc <- parse
      pc `shouldSatisfy` isRight
    else it "parsing fails as expected" $ do
      pc <- parse
      pc `shouldSatisfy` isLeft
 where
  parse = parsePact <$> T.readFile fp

checkLegacyProdParser :: Bool -> String -> SpecWith ()
checkLegacyProdParser expectSuccess fp = describe fp $ do
  if expectSuccess
    then it "parsing succeeds" $ do
      pc <- parse
      pc `shouldSatisfy` isRight
    else it "parsing fails as expected" $ do
      pc <- parse
      pc `shouldSatisfy` isLeft
 where
  parse = legacyParsePact <$> T.readFile fp

-- Versioned natives tests

versionedNativesTest :: ExecutionFlag -> [Text] -> SpecWith ()
versionedNativesTest flag natives = do
  let msg = "Successfully disables " <> show flag <> " natives"
  it msg $ do
    let rs = versionedNativesRefStore (mkExecutionConfig [flag])
        rs' = versionedNativesRefStore (mkExecutionConfig [])
        nativesDisabled = S.fromList natives
    rs `shouldSatisfy` views rsNatives (S.disjoint nativesDisabled . S.fromList  . HM.keys)
    rs' `shouldSatisfy` views rsNatives (S.isSubsetOf nativesDisabled . S.fromList  . HM.keys)

versionedNativesTests :: SpecWith ()
versionedNativesTests = describe "versionedNativesTests" $ do
  versionedNativesTest FlagDisablePact40 pact40Natives
  versionedNativesTest FlagDisablePact420 pact420Natives
  versionedNativesTest FlagDisablePact43 pact43Natives
  versionedNativesTest FlagDisablePact431 pact431Natives
  versionedNativesTest FlagDisablePact46 pact46Natives
  versionedNativesTest FlagDisablePact47 pact47Natives
