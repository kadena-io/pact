{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}


module Pact.GasModel.Types
  ( GasTest(..)
  , GasSetup(..)
  , GasTestResult(..)

  , getDescription

  , GasUnitTests(..)
  , concatGasUnitTests
  , mapOverGasUnitTests

  , NoopNFData(..)

  , defGasUnitTests
  , createGasUnitTests

  , setState
  , setEnv


  ) where

import Control.Concurrent (readMVar)
import Control.DeepSeq (NFData(..))
import Control.Exception (onException)
import Control.Lens hiding ((.=),DefName)
import Data.Default (def)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import NeatInterpolation (trimming)
import System.Directory (removeFile)


import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty  as NEL
import qualified Pact.Persist.SQLite as PSL
import qualified Data.Text as T


import Pact.Eval (eval)
import Pact.Gas
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


newtype GasUnitTests = GasUnitTests (NEL.NonEmpty GasTest)
instance Semigroup GasUnitTests where
  (GasUnitTests g) <> (GasUnitTests g') =
    GasUnitTests (g <> g')

concatGasUnitTests :: NEL.NonEmpty GasUnitTests -> GasUnitTests
concatGasUnitTests listOfTests =
  foldl' (<>) baseCase rest
  where
    baseCase = NEL.head listOfTests
    rest = NEL.tail listOfTests



mapOverGasUnitTests
  :: GasUnitTests
  -> (PactExpression -> GasSetup SQLiteDb -> IO f)
  -> (PactExpression -> GasSetup () -> IO f)
  -> IO [GasTestResult f]
mapOverGasUnitTests (GasUnitTests tests) sqliteFun mockFun = do
  mapM run (NEL.toList tests)
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



defGasUnitTests
  :: NEL.NonEmpty PactExpression
  -> NativeDefName
  -> GasUnitTests
defGasUnitTests pactExprs funName =
  GasUnitTests $ NEL.map (\e -> defGasTest e funName) pactExprs


createGasUnitTests
  :: (GasSetup SQLiteDb -> GasSetup SQLiteDb)
  -> (GasSetup () -> GasSetup ())
  -> NEL.NonEmpty PactExpression
  -> NativeDefName
  -> GasUnitTests
createGasUnitTests sqliteUpdate mockUpdate pactExprs funName =
  GasUnitTests $ NEL.map createTest pactExprs
  where
    createTest expr =
      GasTest funName expr
      (sqliteUpdate defSqliteGasSetup,
       mockUpdate defMockGasSetup)


-- | Default Gas Tests
defGasTest :: PactExpression -> NativeDefName -> GasTest
defGasTest expr funName =
  GasTest
  funName
  expr
  (defSqliteGasSetup, defMockGasSetup)

defSqliteGasSetup :: GasSetup SQLiteDb
defSqliteGasSetup =
  GasSetup
  defSqliteBackend
  defEvalState
  "SQLiteDb"
  sqliteSetupCleanup

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
  let loaded = HM.singleton (Name $ BareName sampleLoadedKeysetName def)
               (Direct $ TGuard (GKeySet sampleKeyset) def)
  return $ set (evalRefs . rsLoaded) loaded stateWithModule

getLoadedState
  :: T.Text
  -> IO (EvalState)
getLoadedState code = do
  terms <- compileCode code
  pureDb <- mkPureEnv neverLog
  initSchema pureDb
  let env = defEvalEnv pureDb
  (_, newState) <- runEval def env $ mapM eval terms
  return newState



-- | Default EvalEnv
--   Default backends for gas testing have the following loaded:
--   * Sample accounts module and table
--   * An account "someId" with 0.0 as its balance
--   * Sample keyset and namespace

defEvalEnv :: PactDbEnv e -> EvalEnv e
defEvalEnv db =
  setupEvalEnv db entity Transactional (initMsgData pactInitialHash)
  initRefStore freeGasEnv permissiveNamespacePolicy noSPVSupport def def
  where entity = Just $ EntityName "entity"

-- MockDb
--
defMockBackend :: IO (EvalEnv ())
defMockBackend = do
  db <- mkMockEnv defMockDb
  return $ defEvalEnv db

defMockDb :: MockDb
defMockDb = mockdb
  where
    mockdb = def { mockRead = MockRead rowRead }

    rowRead :: Domain k v -> k -> Method () (Maybe v)
    rowRead UserTables {} _ = rc (Just acctRow)
    rowRead KeySets (KeySetName ks)
      | ks == sampleLoadedKeysetName = rc (Just sampleKeyset)
      | otherwise = rc Nothing
    rowRead Namespaces (NamespaceName n)
      | n == sampleNamespaceName = rc (Just sampleNamespace)
      | otherwise = rc Nothing
    rowRead _ _ = rc Nothing

-- SQLite Db
--
sqliteFile :: String
sqliteFile = "gasmodel.sqlite"

defSqliteBackend :: IO (EvalEnv SQLiteDb)
defSqliteBackend = do
  sqliteDb <- mkSQLiteEnv (newLogger neverLog "")
              True (SQLiteConfig sqliteFile fastNoJournalPragmas) neverLog
  initSchema sqliteDb
  state <- defEvalState
  let env = defEvalEnv sqliteDb
      setupExprs =
        (accountsModule acctModuleName) <>
        [trimming| (create-table $acctModuleNameText.accounts)
               (insert $acctModuleNameText.accounts
                  "someId"
                  { "balance": 0.0 })
               (define-keyset "$sampleLoadedKeysetName" $sampleLoadedKeysetName)
               (define-namespace "$sampleNamespaceName" $sampleLoadedKeysetName $sampleLoadedKeysetName)
        |]
  setupTerms <- compileCode setupExprs
  (res,_) <- runEval' state env $ mapM eval setupTerms
  _ <- onException (eitherDie "Sqlite setup expressions" res) (sqliteSetupCleanup (env,state))
  return env


-- | Default GasSetup cleanup
mockSetupCleanup :: (EvalEnv (), EvalState) -> IO ()
mockSetupCleanup (_, _) = return ()

sqliteSetupCleanup :: (EvalEnv SQLiteDb, EvalState) -> IO ()
sqliteSetupCleanup (env, _) = do
  c <- readMVar $ _eePactDbVar env
  _ <- PSL.closeSQLite $ _db c
  removeFile sqliteFile
