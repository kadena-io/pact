{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}


module Pact.GasModel.Types
  ( GasTest(..)
  , GasSetup(..)
  , GasTestResult(..)

  , getDescription

  , GasUnitTests(..)
  , concatGasUnitTests
  , runGasUnitTests

  , NoopNFData(..)

  , defGasUnitTest
  , defGasUnitTests
  , defPactExpGasTest
  , defPactExpGasTests

  , createGasUnitTests

  , setState
  , setEnv


  ) where

import Control.Concurrent (readMVar)
import Control.DeepSeq (NFData(..))
import Control.Exception (onException)
import Control.Lens hiding ((.=),DefName)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import NeatInterpolation (text)
import System.Directory (removeFile)


import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base16 as B16
import qualified Data.HashMap.Strict as HM
import qualified Pact.Persist.SQLite as PSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import Pact.Eval (eval)
import Pact.Gas.Table
import Pact.GasModel.Utils
import Pact.Interpreter
import Pact.MockDb
import Pact.PersistPactDb (DbEnv(..))
import Pact.Types.Lang
import Pact.Types.Logger (neverLog, Loggers(..))
import Pact.Types.Runtime
import Pact.Types.SQLite (SQLiteConfig(..),fastNoJournalPragmas)
import Pact.Types.SPV


type SQLiteDb = DbEnv PSL.SQLite

data GasSetup e = GasSetup
  { _gasSetupEnv :: !(IO (EvalEnv e))
  , _gasSetupState :: !(IO EvalState)
  , gasSetupBackendType :: !T.Text
  , gasSetupCleanup :: !((EvalEnv e, EvalState) -> IO ())
  }
-- only makes lenses for fields prefixed with underscore
makeLenses ''GasSetup


data GasTest = GasTest
  { _gasTestFunctionName :: NativeDefName
  , _gasTestExpression :: !PactExpression
  , _gasTestSetups :: !(GasSetup SQLiteDb, GasSetup ())
  }


data GasTestResult a = GasTestResult
 { _gasTestResultFunctionName :: !NativeDefName
 , _gasTestResultDesciption :: !T.Text
 , _gasTestResultSqliteDb :: !a
 , _gasTestResultMockDb :: !a
 }


getDescription :: PactExpression -> GasSetup e -> T.Text
getDescription expr testSetup =
  (gasSetupBackendType testSetup) <> "/" <> exprDesc
  where exprDesc = fromMaybe
                   (_pactExpressionFull expr)
                   (_pactExpressionAbridged expr)


newtype GasUnitTests = GasUnitTests { gasUnitTests :: [GasTest] }
    deriving Semigroup

concatGasUnitTests :: [GasUnitTests] -> GasUnitTests
concatGasUnitTests = GasUnitTests . concat . map gasUnitTests


runGasUnitTests
  :: GasUnitTests
  -> (PactExpression -> GasSetup SQLiteDb -> IO a)
  -> (PactExpression -> GasSetup () -> IO a)
  -> IO [GasTestResult a]
runGasUnitTests (GasUnitTests tests) sqliteFun mockFun = do
  mapM run tests
  where
    description (PactExpression full abrid) =
      fromMaybe full abrid
    run (GasTest funName expr (sqlitedb, mockdb)) = do
      resSqlite <- sqliteFun expr sqlitedb
      resMock <- mockFun expr mockdb
      return $
        GasTestResult funName (description expr) resSqlite resMock



-- | Newtype to provide a noop NFData instance.
-- Intended for use in criterion's 'envWithCleanup'
-- which wants environment values to be NFData.
newtype NoopNFData a = NoopNFData a
  deriving (Show)
instance NFData (NoopNFData a) where
  rnf _ = ()

defGasUnitTest
  :: PactExpression
  -> NativeDefName
  -> GasUnitTests
defGasUnitTest pe = defGasUnitTests [pe]

defGasUnitTests
  :: [PactExpression]
  -> NativeDefName
  -> GasUnitTests
defGasUnitTests pactExprs funName =
  GasUnitTests $ map (\e -> defGasTest e funName) pactExprs

defPactExpGasTest :: T.Text -> NativeDefName -> GasUnitTests
defPactExpGasTest code = defPactExpGasTests [code]

defPactExpGasTests :: [T.Text] -> NativeDefName -> GasUnitTests
defPactExpGasTests codes = defGasUnitTests (map defPactExpression codes)

createGasUnitTests
  :: (GasSetup SQLiteDb -> GasSetup SQLiteDb)
  -> (GasSetup () -> GasSetup ())
  -> [PactExpression]
  -> NativeDefName
  -> GasUnitTests
createGasUnitTests sqliteUpdate mockUpdate pactExprs funName =
  GasUnitTests $ map createTest pactExprs
  where
    createTest expr =
      GasTest funName expr
      (sqliteUpdate (defSqliteGasSetup funName),
       mockUpdate defMockGasSetup)


-- | Default Gas Tests
defGasTest :: PactExpression -> NativeDefName -> GasTest
defGasTest expr funName =
  GasTest
  funName
  expr
  (defSqliteGasSetup funName, defMockGasSetup)

defSqliteGasSetup :: NativeDefName -> GasSetup SQLiteDb
defSqliteGasSetup n =
  GasSetup
  (defSqliteBackend encName)
  defEvalState
  "SQLiteDb"
  (sqliteSetupCleanup encName)
 where
  encName = B8.unpack $ B16.encode $ T.encodeUtf8 $ asString n

defMockGasSetup :: GasSetup ()
defMockGasSetup =
  GasSetup
  defMockBackend
  defEvalState
  "MockDb"
  mockSetupCleanup


-- | Helper functions for manipulating Gas Tests
setState :: (EvalState -> EvalState) -> GasSetup e -> GasSetup e
setState f setup = setState'
  where
    newState = fmap f $ view gasSetupState setup
    setState' = set gasSetupState newState setup

setEnv :: (EvalEnv e -> EvalEnv e) -> GasSetup e -> GasSetup e
setEnv f setup = setEnv'
  where
    newEnv = fmap f $ view gasSetupEnv setup
    setEnv' = set gasSetupEnv newEnv setup


-- | Default EvalState
--   Caches sample keyset and account module
defEvalState :: IO EvalState
defEvalState = do
  stateWithModule <- getLoadedState (accountsModule acctModuleName)
  let loaded = HM.singleton sampleLoadedKeysetName
               (Direct $ TGuard (GKeySet sampleKeyset) def, Nothing)
  return
      $ set (evalRefs . rsLoaded) loaded stateWithModule

getLoadedState
  :: T.Text
  -> IO (EvalState)
getLoadedState code = do
  terms <- compileCode code
  pureDb <- mkPureEnv neverLog
  initSchema pureDb
  env <- defEvalEnv pureDb
  (_, newState) <- runEval def env $ mapM eval terms
  return newState



-- | Default EvalEnv
--   Default backends for gas testing have the following loaded:
--   * Sample accounts module and table
--   * An account "someId" with 0.0 as its balance
--   * Sample keyset and namespace

defEvalEnv :: PactDbEnv e -> IO (EvalEnv e)
defEvalEnv db = do
  setupEvalEnv db entity Transactional (initMsgData pactInitialHash) (versionedNativesRefStore noPact44EC)
    prodGasModel permissiveNamespacePolicy noSPVSupport def noPact44EC
  where entity = Just $ EntityName "entity"
        prodGasModel = GasEnv 10000000 0.01 $ tableGasModel defaultGasConfig
        noPact44EC = mkExecutionConfig [FlagDisablePact44]

-- MockDb
--
defMockBackend :: IO (EvalEnv ())
defMockBackend = do
  db <- mkMockEnv defMockDb
  defEvalEnv db

defMockDb :: MockDb
defMockDb = mockdb
  where
    mockdb = def { mockRead = MockRead rowRead }

    rowRead :: Domain k v -> k -> Method () (Maybe v)
    rowRead UserTables {} _ = rc (Just acctRow)
    rowRead KeySets (KeySetName ks _n)
      | ks == sampleLoadedKeysetName = rc (Just sampleKeyset)
      | otherwise = rc Nothing
    rowRead Namespaces (NamespaceName n)
      | n == sampleNamespaceName = rc (Just sampleNamespace)
      | otherwise = rc Nothing
    rowRead _ _ = rc Nothing

-- SQLite Db
--
sqliteFile :: Maybe String -> String
sqliteFile s = "gasmodel" <> maybe "" ('.':) s <> ".sqlite"

defSqliteBackend :: String -> IO (EvalEnv SQLiteDb)
defSqliteBackend name = do
  sqliteDb <- mkSQLiteEnv (newLogger neverLog "")
              True (SQLiteConfig dbFileName fastNoJournalPragmas) neverLog
  initSchema sqliteDb
  state <- defEvalState
  env <- defEvalEnv sqliteDb
  let setupExprs =
        (accountsModule acctModuleName) <>
        [text| (create-table $acctModuleNameText.accounts)
               (insert $acctModuleNameText.accounts
                  "someId"
                  { "balance": 0.0 })

               (define-keyset "$sampleLoadedKeysetName" $sampleLoadedKeysetName)
               (define-namespace "$sampleNamespaceName" $sampleLoadedKeysetName $sampleLoadedKeysetName)
        |]
  setupTerms <- compileCode setupExprs
  (res,_) <- runEval' state env $ mapM eval setupTerms
  _ <- onException (eitherDie "Sqlite setup expressions" res) (sqliteSetupCleanup name (env,state))
  return env
 where
  dbFileName = sqliteFile $ Just name


-- | Default GasSetup cleanup
mockSetupCleanup :: (EvalEnv (), EvalState) -> IO ()
mockSetupCleanup (_, _) = return ()

sqliteSetupCleanup :: String -> (EvalEnv SQLiteDb, EvalState) -> IO ()
sqliteSetupCleanup name (env, _) = do
  c <- readMVar $ _eePactDbVar env
  _ <- PSL.closeSQLite $ _db c
  removeFile dbFileName
 where
  dbFileName = sqliteFile $ Just name
