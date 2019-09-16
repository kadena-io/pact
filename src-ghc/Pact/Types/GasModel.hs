{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}


module Pact.Types.GasModel
  ( GasTest(..)
  , gasTestExpression
  , gasTestDescription
  , gasTestSetup
  , gasTestSetupCleanup

  , GasUnitTests(..)
  , concatGasUnitTests

  , NoopNFData(..)
  
  , defGasUnitTests
  , createGasUnitTests

  , setState
  , setEnv
  
  
  ) where

import Control.Concurrent         (readMVar)
import Control.Lens               hiding ((.=),DefName)
import Control.DeepSeq            (NFData(..))
import Data.Default               (def)
import Data.List                  (foldl')
import NeatInterpolation          (text)
import System.Directory           (removeFile)


import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.List.NonEmpty  as NEL
import qualified Pact.Persist.SQLite as PSL


-- Internal exports
--
import Pact.GasModel.Utils
import Pact.MockDb
import Pact.Types.Logger          (neverLog, Loggers(..))
import Pact.Types.SQLite          (SQLiteConfig(..),fastNoJournalPragmas)

import Pact.PersistPactDb         (DbEnv(..))
import Pact.Eval                  (eval)

import Pact.Gas
import Pact.Interpreter
import Pact.Types.Lang
import Pact.Types.Runtime
import Pact.Types.SPV


type SQLiteDb = DbEnv PSL.SQLite
type GasSetup e = (EvalEnv e, EvalState)
type SQLiteGasTests = NEL.NonEmpty (GasTest SQLiteDb)
type MockGasTests = NEL.NonEmpty (GasTest ())

data GasTest e = GasTest
  { _gasTestExpression :: !PactExpression
  , _gasTestDescription :: !T.Text
  , _gasTestSetup :: !(IO (GasSetup e))
  , _gasTestSetupCleanup :: !((GasSetup e) -> IO ())
  }
makeLenses ''GasTest


data GasUnitTests = GasUnitTests
  { _gasUnitTestsSqlite :: SQLiteGasTests
  , _gasUnitTestsMock :: MockGasTests
  }
instance Semigroup (GasUnitTests) where
  g <> g' = GasUnitTests
            (_gasUnitTestsSqlite g <> _gasUnitTestsSqlite g')
            (_gasUnitTestsMock g <> _gasUnitTestsMock g')


concatGasUnitTests :: NEL.NonEmpty GasUnitTests -> GasUnitTests
concatGasUnitTests listOfTests =
  foldl' (<>) baseCase rest
  where
    baseCase = NEL.head listOfTests
    rest = NEL.tail listOfTests



-- | Newtype to provide a noop NFData instance.
-- Intended for use in criterion's 'envWithCleanup'
-- which wants environment values to be NFData.
newtype NoopNFData a = NoopNFData a
  deriving (Show)
instance NFData (NoopNFData a) where
  rnf _ = ()



defGasUnitTests
  :: NEL.NonEmpty (T.Text, PactExpression)
  -> GasUnitTests
defGasUnitTests pactExprs = GasUnitTests sqliteTests mockTests
  where
    mockTests = NEL.map defMockGasTest pactExprs
    sqliteTests = NEL.map defSqliteGasTest pactExprs


createGasUnitTests
  :: (GasTest SQLiteDb -> GasTest SQLiteDb)
  -> (GasTest () -> GasTest ())
  -> NEL.NonEmpty (T.Text, PactExpression)
  -> GasUnitTests
createGasUnitTests sqliteUpdate mockUpdate pactExprs =
  GasUnitTests sqliteTests mockTests
  where
    mockTests = NEL.map (mockUpdate . defMockGasTest) pactExprs
    sqliteTests = NEL.map (sqliteUpdate . defSqliteGasTest) pactExprs


-- | Default Gas Tests
defMockGasTest :: (T.Text, PactExpression) -> GasTest ()
defMockGasTest (desc, expr) =
  GasTest
  expr
  desc
  (createSetup defMockBackend defEvalState)
  mockSetupCleanup

defSqliteGasTest :: (T.Text, PactExpression) -> GasTest SQLiteDb
defSqliteGasTest (desc, expr) =
  GasTest
  expr
  desc
  (createSetup defSqliteBackend defEvalState)
  sqliteSetupCleanup

createSetup :: IO (EvalEnv e) -> IO EvalState -> IO (GasSetup e)
createSetup env state = do
  e <- env
  s <- state
  return (e,s)


-- | Helper functions for manipulating Gas Tests 
setState :: (EvalState -> EvalState) -> GasTest e -> GasTest e
setState f test = setState'
  where
    newState = fmap (\(e,s) -> (e, f s))
               $ view gasTestSetup test
    setState' = set gasTestSetup newState test

setEnv :: (EvalEnv e -> EvalEnv e) -> GasTest e -> GasTest e
setEnv f test = setEnv'
  where
    newEnv = fmap (\(e,s) -> (f e, s)) $ view gasTestSetup test
    setEnv' = set gasTestSetup newEnv test


-- | Default EvalState
--   Caches sample keyset and account module
defEvalState :: IO EvalState
defEvalState = do
  stateWithModule <- getLoadedState (accountsModule acctModuleName)
  let loaded = HM.singleton (Name sampleLoadedKeysetName def)
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

sqliteFile :: String
sqliteFile = "log/bench.sqlite"


defEvalEnv :: PactDbEnv e -> EvalEnv e
defEvalEnv db =
  setupEvalEnv db entity Transactional (initMsgData pactInitialHash)
  initRefStore freeGasEnv permissiveNamespacePolicy noSPVSupport def
  where entity = Just $ EntityName "entity"

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

defSqliteBackend :: IO (EvalEnv SQLiteDb)
defSqliteBackend = do
  sqliteDb <- mkSQLiteEnv (newLogger neverLog "")
              True (SQLiteConfig sqliteFile fastNoJournalPragmas) neverLog
  initSchema sqliteDb
  state <- defEvalState
  let env = defEvalEnv sqliteDb
      setupExprs =
        (accountsModule acctModuleName) <>
        [text| (create-table $acctModuleNameText.accounts)
               (insert $acctModuleNameText.accounts
                     "someId"
                     { "balance": 0.0 })
               (define-keyset "$sampleLoadedKeysetName" $sampleLoadedKeysetName)
               (define-namespace "$sampleNamespaceName" $sampleLoadedKeysetName)
        |]
  setupTerms <- compileCode setupExprs
  _ <- runEval state env $ mapM eval setupTerms
  return env


-- | Default GasSetup cleanup
mockSetupCleanup :: GasSetup () -> IO ()
mockSetupCleanup (_, _) = return ()

sqliteSetupCleanup :: GasSetup SQLiteDb -> IO ()
sqliteSetupCleanup (env, _) = do
  c <- readMVar $ _eePactDbVar env
  _ <- PSL.closeSQLite $ _db c
  removeFile sqliteFile
