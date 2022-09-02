{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module PactTestsSpec (spec) where


import Test.Hspec

import Control.Concurrent
import Control.Monad.State.Strict

import Data.Decimal
import Data.Either (isLeft, isRight)
import Data.List
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (unpack)
import qualified Data.Text.IO as T
import Test.QuickCheck
import Test.QuickCheck.Monadic


import Pact.Repl
import Pact.Repl.Lib
import Pact.Repl.Types
import Pact.Types.Logger
import Pact.Types.Runtime
import Pact.Persist.SQLite as SQLite
import Pact.Interpreter
import Pact.Parse (parsePact, legacyParsePact)
import qualified Pact.Native.Trans.Musl as M
import qualified Pact.Native.Trans.Types as T
import qualified Pact.Native.Trans.TOps as T


import System.Directory
import System.FilePath

spec :: Spec
spec = do
  pactTests
  badTests
  accountsTest
  cpTest
  verifiedAccountsTest
  prodParserTests
  legacyProdParserTests
  floatingPointTests


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
  (r,ReplState{..}) <- runIO $ do
    (PactDbEnv _ pdb) <- mkSQLiteEnv (newLogger neverLog "") False (SQLiteConfig "" []) neverLog
    ls <- initLibState' (LibDb pdb) Nothing
    rs <- initReplState' ls Quiet
    execScriptState' fp rs id
  case r of
    Left e -> it ("failed to load " ++ fp) $ expectationFailure e
    Right _ -> do
      LibState{..} <- runIO $ readMVar $ _eePactDbVar _rEnv
      forM_ _rlsTests $ \TestResult {..} -> it (unpack trName) $ case trFailure of
        Nothing -> return ()
        Just (i,e) -> expectationFailure $ renderInfo (_faInfo i) ++ ": " ++ unpack e

runBadScript :: String -> SpecWith ()
runBadScript fp = describe ("bad-" ++ fp) $ do
  (r,ReplState{}) <- runIO $ execScript' Quiet fp
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

prodParserTests :: Spec
prodParserTests =
  describe "test production parser" $ do
    fs <- runIO findTests
    forM_ fs $ \fp ->
        checkProdParser (notElem fp badParserTests) fp
 where
  badParserTests =
   [ "tests/pact/bad/bad-parens.repl"
   ]

legacyProdParserTests :: Spec
legacyProdParserTests =
  describe "test legacy production parser" $ do
    fs <- runIO findTests
    forM_ fs (checkLegacyProdParser True)

checkProdParser :: Bool -> String -> SpecWith ()
checkProdParser expectSuccess fp = describe fp $ do
  source <- runIO $ T.readFile fp
  let pc = parsePact source
  if expectSuccess
    then it "parsing succeeds" $ pc `shouldSatisfy` isRight
    else it "parsing fails as expected" $ pc `shouldSatisfy` isLeft

checkLegacyProdParser :: Bool -> String -> SpecWith ()
checkLegacyProdParser expectSuccess fp = describe fp $ do
  source <- runIO $ T.readFile fp
  let pc = legacyParsePact source
  if expectSuccess
    then it "parsing succeeds" $ pc `shouldSatisfy` isRight
    else it "parsing fails as expected" $ pc `shouldSatisfy` isLeft

dec2F :: Decimal -> Double
dec2F = fromRational . toRational

f2Dec :: Double -> Decimal
f2Dec = fromRational . toRational

floatingPointTestsArity1
  :: String
  -> (Double -> Double)
  -> (Double -> Double)
  -> (Decimal -> T.TransResult Decimal)
  -> Spec
floatingPointTestsArity1 lbl f_double f_musl f_mpfr =
  describe ("floating point tests: " ++ lbl) $ runIO $ do
    quickCheck $ withMaxSuccess 1_000_000 $ \x -> monadicIO $ do
      let dble = f_double (dec2F x)
      unless (isNaN dble || isInfinite dble) $ do
        let musl = f_musl (dec2F x)
        unless (isNaN musl || isInfinite musl) $ do
          let mpfr = f_mpfr x
          unless (T.TransNumber (f2Dec musl) == mpfr) $ liftIO $ do
            putStrLn $ "x = " ++ show x
            putStrLn $ "dble = " ++ show (f2Dec dble)
            putStrLn $ "musl = " ++ show (f2Dec musl)
            putStrLn $ "mpfr = " ++ show mpfr
            error "Results fail to match"

floatingPointTestsArity2
  :: String
  -> (Double -> Double -> Double)
  -> (Double -> Double -> Double)
  -> (Decimal -> Decimal -> T.TransResult Decimal)
  -> Spec
floatingPointTestsArity2 lbl f_double f_musl f_mpfr =
  describe ("floating point tests: " ++ lbl) $ runIO $ do
    -- From the MPFR 4.1.0 documentation: ...with a precision of 53 bits and
    -- in any of the four standard rounding modes, MPFR is able to exactly
    -- reproduce all computations with double-precision machine floating-point
    -- numbers (e.g., double type in C, with a C implementation that
    -- rigorously follows Annex F of the ISO C99 standard and FP_CONTRACT
    -- pragma set to OFF) on the four arithmetic operations and the square
    -- root, except the default exponent range is much wider and subnormal
    -- numbers are not implemented (but can be emulated).
    quickCheck $ withMaxSuccess 1_000_000 $ \x y -> monadicIO $ do
      let dble = f_double (dec2F x) (dec2F y)
      unless (isNaN dble || isInfinite dble) $ do
        let musl = f_musl (dec2F x) (dec2F y)
        unless (isNaN musl || isInfinite musl) $ do
          let mpfr = f_mpfr x y
          unless (T.TransNumber (f2Dec musl) == mpfr) $ liftIO $ do
            putStrLn $ "x = " ++ show x
            putStrLn $ "y = " ++ show y
            putStrLn $ "dble = " ++ show (f2Dec dble)
            putStrLn $ "musl = " ++ show (f2Dec musl)
            putStrLn $ "mpfr = " ++ show mpfr
            error "Results fail to match"

floatingPointTests :: Spec
floatingPointTests = do
  floatingPointTestsArity1 "exp" exp M.trans_exp T.mpfr_exp
  floatingPointTestsArity1 "ln" Prelude.log M.trans_ln T.mpfr_ln
  floatingPointTestsArity2 "logBase" logBase M.trans_log T.mpfr_log
  floatingPointTestsArity2 "pow" (**) M.trans_pow T.mpfr_pow
  floatingPointTestsArity1 "sqrt" sqrt M.trans_sqrt T.mpfr_sqrt
