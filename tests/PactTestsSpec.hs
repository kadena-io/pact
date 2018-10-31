{-# LANGUAGE RecordWildCards #-}
module PactTestsSpec (spec) where


import Test.Hspec

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
  accountsTest
  cpTest
  verifiedAccountsTest


pactTests :: Spec
pactTests = describe "pact tests" $ do
  fs <- runIO findTests
  forM_ fs runScript

accountsTest :: Spec
accountsTest = describe "accounts regression" $ runScript ("examples" </> "accounts" </> "accounts.repl")

cpTest :: Spec
cpTest = describe "cp regression" $ runScript ("examples" </> "cp" </> "cp.repl")

verifiedAccountsTest :: Spec
verifiedAccountsTest = describe "verified accounts regression" $
  runScript ("examples" </> "verified-accounts" </> "accounts.repl")


findTests :: IO [FilePath]
findTests = (map (tdir </>) . filter ((== ".repl") . reverse . take 5 . reverse)) <$> getDirectoryContents tdir
            where tdir = "tests" </> "pact"


runScript :: String -> SpecWith ()
runScript fp = describe fp $ do
  (r,ReplState{..}) <- runIO $ execScript' (Script False fp) fp
  case r of
    Left e -> fail e
    Right _ -> do
      LibState{..} <- runIO $ readMVar $ _eePactDbVar _rEnv
      forM_ _rlsTests $ \TestResult {..} -> it (unpack trName) $ case trFailure of
        Nothing -> return ()
        Just (i,e) -> expectationFailure $ renderInfo (_faInfo i) ++ ": " ++ unpack e
