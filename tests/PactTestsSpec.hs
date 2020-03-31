{-# LANGUAGE RecordWildCards #-}
module PactTestsSpec (spec) where


import Test.Hspec

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Data.Text (unpack)

import Pact.Repl
import Pact.Repl.Types
import Pact.Types.Runtime

import System.Directory
import System.FilePath
import Control.Monad
import Control.Concurrent

spec :: Spec
spec = do
  pactTests
  badTests
  accountsTest
  cpTest
  verifiedAccountsTest


pactTests :: Spec
pactTests = do
  describe "pact tests" $ do
    fs <- runIO findTests
    forM_ fs runScript

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
runScript fp = describe fp $ do
  (r,ReplState{..}) <- runIO $ execScript' Quiet fp
  case r of
    Left e -> it ("failed to load " ++ fp) $ expectationFailure e
    Right _ -> do
      LibState{..} <- runIO $ readMVar $ _eePactDbVar _rEnv
      forM_ _rlsTests $ \TestResult {..} -> it (unpack trName) $ case trFailure of
        Nothing -> return ()
        Just (i,e) -> expectationFailure $ renderInfo (_faInfo i) ++ ": " ++ unpack e

runBadScript :: String -> SpecWith ()
runBadScript fp = describe ("bad-" ++ fp) $ do
  (r,ReplState{..}) <- runIO $ execScript' Quiet fp
  let expectedError = M.lookup fp badErrors
  it "has error in badErrors" $ expectedError `shouldSatisfy` isJust
  it ("failed as expected: " ++ show expectedError) $
    r `shouldSatisfy` isCorrectError expectedError

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
    "Cannot resolve")
  ,(pfx "bad-root-namespace.repl",
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
  ]
  where
    pfx = ("tests/pact/bad/" ++)
