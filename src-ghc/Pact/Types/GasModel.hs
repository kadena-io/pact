{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}


module Pact.Types.GasModel
  ( GasTest(..) 
  , SomeGasTest(..)
  , GasUnitTests(..)

  , NoopNFData(..)
  
  , unitTests
  , allNatives
  , untestedNatives
  
  , compileCode
  , printResult
  , eitherDie

  , unitTestFromDef
  
  ) where

import Bound                      (abstract, Scope)
import Control.Concurrent         (readMVar)
import Control.Lens               hiding ((.=),DefName)
import Control.DeepSeq            (NFData(..))
import Control.Exception          (throwIO)
import Data.Aeson                 (toJSON, ToJSON(..))
import Data.Default               (def)
import Data.List                  (foldl')
import NeatInterpolation          (text)
import Data.List.NonEmpty         (NonEmpty(..))
import System.Directory           (removeFile)


import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Foldable       as F
import qualified Data.Set            as S
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.List.NonEmpty  as NEL
import qualified Pact.Persist.SQLite as PSL


-- Internal exports
--
import Pact.Native
import Pact.Types.Native
import Pact.Compile               (compileExps, mkTextInfo)
import Pact.MockDb
import Pact.Types.PactValue       (toPactValueLenient, PactValue(..))
import Pact.Types.Logger          (neverLog, Loggers(..))
import Pact.Types.SQLite          (SQLiteConfig(..))

import Pact.PersistPactDb         (DbEnv(..))
import Pact.Persist.Pure          (PureDb)
import Pact.Eval                  (eval)

import Pact.Gas
import Pact.Interpreter
import Pact.Parse
import Pact.Types.Command
import Pact.Types.Lang
import Pact.Types.Runtime
import Pact.Types.SPV


type SQLiteDb = DbEnv PSL.SQLite
type GasSetup e = (EvalEnv e, EvalState)

data GasTestBackend = GasMockDb | GasSQLiteDb

data GasTest e = GasTest
  { _gasTestExpression :: !T.Text
  , _gasTestDescription :: !T.Text
  , _gasTestEvalEnvUpdate :: !(EvalEnv e -> EvalEnv e)
  , _gasTestEvalStateUpdate :: !(EvalState -> EvalState)
  , _gasTestSetup :: !(IO (GasSetup e))
  , _gasTestSetupCleanup :: !((GasSetup e) -> IO ())
  }
makeLenses ''GasTest


data SomeGasTest
  = forall e. (Show e, Eq e) => SomeGasTest !(GasTest e)

newtype GasUnitTests
  = GasUnitTests (NEL.NonEmpty SomeGasTest)


-- | Newtype to provide a noop NFData instance.
-- Intended for use in criterion's 'envWithCleanup'
-- which wants environment values to be NFData.
newtype NoopNFData a = NoopNFData a
  deriving (Show)
instance NFData (NoopNFData a) where
  rnf _ = ()


defMockGasTest :: T.Text -> GasTest ()
defMockGasTest expr =
  GasTest
  expr
  expr
  id
  id
  defMockSetup
  mockSetupCleanup

defSqliteGasTest :: T.Text -> GasTest SQLiteDb
defSqliteGasTest expr =
  GasTest
  expr
  expr
  id
  id
  defSqliteSetup
  sqliteSetupCleanup


defGasUnitTests
  :: GasTestBackend
  -> NEL.NonEmpty T.Text
  -> GasUnitTests
defGasUnitTests backendType pactExprs = case backendType of
  GasMockDb -> GasUnitTests
               $ NEL.map (SomeGasTest . defMockGasTest) pactExprs
  GasSQLiteDb -> GasUnitTests
                 $ NEL.map (SomeGasTest . defSqliteGasTest) pactExprs


-- | Sample pact code and helper functions/values for testing
acctModuleName :: ModuleName
acctModuleName = ModuleName "accounts" def

accountsModule :: ModuleName -> T.Text
accountsModule moduleName = [text|
     (module $moduleNameText GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]
  where moduleNameText = asString moduleName

acctRow :: ObjectMap PactValue
acctRow = ObjectMap $ M.fromList
          [("balance", PLiteral (LDecimal 100.0))]


samplePubKeys :: [PublicKey]
samplePubKeys = [PublicKey "something"]


sampleKeyset :: KeySet
sampleKeyset = KeySet samplePubKeys (Name "keys-all" def)


sampleNamespaceName :: T.Text
sampleNamespaceName = "my-namespace"


sampleNamespace :: Namespace
sampleNamespace = Namespace
                  (NamespaceName sampleNamespaceName)
                  (GKeySet sampleKeyset)


-- | Helper functions for manipulating EvalState 
setState :: (EvalState -> EvalState) -> GasTest e -> GasTest e
setState f test = setState'
  where
    newState = fmap (\(e,s) -> (e, f s))
               $ view gasTestSetup test
    setState' = set gasTestSetup newState test


defEvalState :: EvalState
defEvalState = def


getLoadedState
  :: T.Text
  -> (EvalEnv (DbEnv PureDb) -> EvalEnv (DbEnv PureDb))
  -> EvalState
  -> IO (EvalState)
getLoadedState code updateEnv state = do
  terms <- compileCode code
  pureDb <- mkPureEnv neverLog
  initSchema pureDb
  let env = updateEnv $ defEvalEnv pureDb
  (_, newState) <- runEval state env $ mapM eval terms
  return newState


defEvalStateCaching :: IO EvalState
defEvalStateCaching = do
  getLoadedState (accountsModule acctModuleName) id defEvalState



-- | Helper functions for manipulating EvalEnv
setEnv :: (EvalEnv e -> EvalEnv e) -> GasTest e -> GasTest e
setEnv f test = setEnv'
  where
    newEnv = fmap (\(e,s) -> (f e, s)) $ view gasTestSetup test
    setEnv' = set gasTestSetup newEnv test


defEvalEnv :: PactDbEnv e -> EvalEnv e
defEvalEnv db =
  setupEvalEnv db entity Transactional (initMsgData pactInitialHash)
  initRefStore freeGasEnv permissiveNamespacePolicy noSPVSupport def
  where entity = Just $ EntityName "entity"


defMockDb :: MockDb
defMockDb = mockdb
  where
    mockdb = def { mockRead = MockRead rowRead
                 , mockKeys = MockKeys keysRead
                 , mockTxIds = MockTxIds txIdsRead
                 , mockGetTxLog = MockGetTxLog getTxLogRead
                 }
    
    rowRead :: Domain k v -> k -> Method () (Maybe v)
    rowRead UserTables {} _ = rc (Just acctRow)
    rowRead KeySets _ = rc (Just sampleKeyset)
    rowRead _ _ = rc Nothing

    txIdsRead :: TableName -> TxId -> Method () [TxId]
    txIdsRead _ i = rc [i]

    getTxLogRead :: Domain k v -> TxId -> Method () [TxLog v]
    getTxLogRead UserTables {} _ = rc [TxLog "accounts.accounts" "some-id" acctRow]
    getTxLogRead _ _ = rc []

    keysRead :: Domain k v -> Method () [k]
    keysRead UserTables {} = rc ["some-id"]
    keysRead _ = rc []


defMockSetup :: IO (GasSetup ())
defMockSetup = do
  state <- defEvalStateCaching
  createMockSetup defMockDb state  


createMockSetup
  :: MockDb
  -> EvalState
  -> IO (GasSetup ())
createMockSetup mdb state = do
  db <- mkMockEnv mdb
  return $ (defEvalEnv db, state)


mockSetupCleanup :: GasSetup () -> IO ()
mockSetupCleanup (_, _) = return ()


sqliteFile :: T.Text
sqliteFile = "log/bench.sqlite"

defSqliteSetup :: IO (GasSetup SQLiteDb)
defSqliteSetup = do
  sqliteDb <- mkSQLiteEnv
              (newLogger neverLog "")
              True
              (SQLiteConfig sqliteFile [])
              neverLog
  initSchema sqliteDb
  state <- defEvalStateCaching
  return $ (defEvalEnv sqliteDb, state)


sqliteSetupCleanup :: GasTest (SQLiteDb) -> IO ()
sqliteSetupCleanup (env, _) = do
  c <- readMVar $ _eePactDb env
  _ <- PSL.closeSQLite $ _db c
  removeFile sqliteFile


-- TODO get rid?
-- | Mock module name for testing
defModuleName :: ModuleName
defModuleName = ModuleName "some-module" Nothing

-- | Mock module hash for testing
defModuleHash :: ModuleHash
defModuleHash = ModuleHash $ pactHash ""

-- | Mock stack frame with mock module name
defStackFrame :: StackFrame
defStackFrame =
  StackFrame "" def
  (Just ((FunApp def ""
           (Just defModuleName) Defun (funTypes $ FunType [] TyAny) Nothing)
        ,[])
  )

-- | Mock module data with mock module name for testing
defModuleData :: ModuleData Ref
defModuleData = ModuleData modDef refMap
  where refMap = HM.empty
        ref = Direct $ TVar (Name "" def) def
        fst' :: Ref -> Maybe Int
        fst' = const Nothing
        scd' :: Term Ref
        scd' = TVar ref def
        
        scopeOfRef :: Scope Int Term Ref
        scopeOfRef = abstract fst' scd'
        
        defOfRef = Def (DefName "") defModuleName Defun (FunType [] TyAny) scopeOfRef def def
        modDef = MDModule mod'
        gov = Governance $ Right defOfRef
        mod' = Module defModuleName gov def (Code "") defModuleHash HS.empty [] []



-- | Helper functions for manipulating Text and other util functions
compileCode :: T.Text -> IO [Term Name]
compileCode m = do
  parsedCode <- parseCode m
  throwEither $ compileExps
                (mkTextInfo $ _pcCode parsedCode)
                (_pcExps parsedCode)

parseCode :: T.Text -> IO ParsedCode
parseCode m = do
  ParsedCode m <$> eitherDie m (parseExprs m)


printResult :: Either PactError [Term Name] -> IO ()
printResult res = case res of
  Left err -> print $ show err
  Right ts -> print $ show $ toJSON $ map (toJSON . toPactValueLenient) ts

eitherDie :: (Show b) => T.Text -> Either b a -> IO a
eitherDie annot
  = either (throwIO . userError . ((show annot ++ " : ") ++) . show) (return $!)


type PactExpression = T.Text

data MockPactType =
    MockObject (HM.HashMap T.Text Integer)
  | MockBinding (HM.HashMap T.Text Integer)
  | MockList [MockPactType]
  | MockBool Bool
  | MockInt Integer
  | MockString T.Text
  | MockExpr T.Text
  deriving (Show)

toText :: MockPactType -> PactExpression
toText (MockObject m) = toPactMap m
toText (MockBinding m) = toPactBinding m
toText (MockList li) = "[ " <> T.unwords (map toText li) <> " ]"
toText (MockBool True) = "true"
toText (MockBool False) = "false"
toText (MockInt i) = intToStr i
toText (MockString t) = escapeText t
toText (MockExpr t) = t

intToStr :: Integer -> T.Text
intToStr = T.pack . show

escapeText :: T.Text -> PactExpression
escapeText n = "\"" <> n <> "\""

toPactMap
  :: HM.HashMap T.Text Integer
  -> PactExpression
toPactMap m = "{ " <> allKeys <> " }"
  where
    allKeys = T.intercalate ", " $ map colonFormat (HM.toList m)
    colonFormat (key, val) = escapeText key <> ": " <> toText (MockInt val)

toPactBinding
  :: HM.HashMap T.Text Integer
  -> PactExpression
toPactBinding  m = "{ " <> allKeys <> " }"
  where
    allKeys = T.intercalate ", " $ map bindingFormat (HM.toList m)
    bindingFormat (key, _) = escapeText key <> " := " <> key

toPactKeyset :: T.Text -> T.Text -> Maybe T.Text -> A.Value
toPactKeyset ksName ksValue predicate =
  A.object [ksName A..= A.object ["keys" A..= [ksValue], "pred" A..= pred']]
  where pred' = maybe ">" id predicate



-- | Sample Pact literals for testing different sizes of lists/strings/integers
sizes :: NEL.NonEmpty Integer
sizes = --TODO write correct sizes (100000, 100, 10)
    10 :|
  [ 5,
    1
  ]


sizesExpr :: NEL.NonEmpty PactExpression
sizesExpr = NEL.map (toText . MockInt) sizes


-- | List of integers of varying sizes
intLists :: NEL.NonEmpty (NEL.NonEmpty Integer)
intLists = NEL.map (\n -> 1 :| [2..n]) sizes


-- | example: [ "[1 2 3 4]" ]
intListsExpr :: NEL.NonEmpty PactExpression
intListsExpr = NEL.map makeExpr intLists
  where
    makeExpr li = toText $ MockList
                  $ map MockInt (NEL.toList li)


-- | List of strings of varying sizes
strLists :: NEL.NonEmpty (NEL.NonEmpty T.Text)
strLists = NEL.map (NEL.map (("a" <>) . intToStr)) intLists


-- | example: [ "[ \"a1\" \"a2\" \"a3\" \"a4\" ]" ]
escapedStrListsExpr :: NEL.NonEmpty PactExpression
escapedStrListsExpr = NEL.map makeExpr strLists
  where
    makeExpr li = toText $ MockList
                  $ map MockString (NEL.toList li)


-- | Maps of varying sizes. The keys are strings and the values are integers.
strKeyIntValMaps :: NEL.NonEmpty (HM.HashMap T.Text Integer)
strKeyIntValMaps = NEL.map toMap allLists
  where allLists = NEL.zip strLists intLists
        toMap (kList, vList) =
          HM.fromList $ zip (NEL.toList kList) (NEL.toList vList)


-- | example: "{ \"a5\": 5, \"a3\": 3 }"
strKeyIntValMapsExpr :: NEL.NonEmpty PactExpression
strKeyIntValMapsExpr = NEL.map (toText . MockObject) strKeyIntValMaps


-- | example: "{ \"a5\" := a5, \"a3\" := a3 }"
strKeyIntValBindingsExpr :: NEL.NonEmpty PactExpression
strKeyIntValBindingsExpr = NEL.map (toText . MockBinding) strKeyIntValMaps


-- | Strings of varying sizes
strings :: NEL.NonEmpty T.Text
strings = NEL.map (T.concat . NEL.toList) strLists


-- | example: "\"a1a2a3a4\""
escapedStringsExpr :: NEL.NonEmpty PactExpression
escapedStringsExpr = NEL.map (toText . MockString) strings


-- | Gas benchmark tests for Pact native functions
allNatives :: [NativeDef]
allNatives = concatMap snd natives

untestedNatives :: GasTestBackend -> [NativeDefName]
untestedNatives backendType = foldl' check [] allNatives
  where
    check li (nativeName,_,_) = case (HM.lookup nativeName (unitTests backendType)) of
      Nothing -> nativeName : li
      Just _ -> li


unitTests :: GasTestBackend -> HM.HashMap NativeDefName GasUnitTests
unitTests backendType = HM.fromList $ foldl' getUnitTest [] allNatives 
  where
    getUnitTest li (nativeName,_,_) =
      case unitTestFromDef nativeName backendType of
        Nothing -> li
        Just ts -> (nativeName, ts) : li
    


unitTestFromDef :: NativeDefName -> GasTestBackend -> Maybe GasUnitTests
unitTestFromDef nativeName = case (asString nativeName) of
  -- | General native functions
  "at"                   -> Just atTests
  "bind"                 -> Just bindTests
  "chain-data"           -> Just chainDataTests
  "compose"              -> Just composeTests
  "constantly"           -> Just constantlyTests
  "contains"             -> Just containsTests
  "define-namespace"     -> Just defineNamespaceTests
  "drop"                 -> Just dropTests
  "enforce"              -> Just enforceTests
  "enforce-one"          -> Just enforceOneTests
  "enforce-pact-version" -> Just enforcePactVersionTests
  "filter"               -> Just filterTests
  "fold"                 -> Just foldTests
  "format"               -> Just formatTests
  "hash"                 -> Just hashTests
  "identity"             -> Just identityTests
  "if"                   -> Just ifTests
  "int-to-str"           -> Just intToStrTests
  "length"               -> Just lengthTests
  "list-modules"         -> Just listModulesTests
  "make-list"            -> Just makeListTests
  "map"                  -> Just mapTests
  "namespace"            -> Just namespaceTests
  "pact-id"              -> Just pactIdTests
  "pact-version"         -> Just pactVersionTests
  "read-decimal"         -> Just readDecimalTests
  "read-integer"         -> Just readIntegerTests
  "read-msg"             -> Just readMsgTests
  "remove"               -> Just removeTests
  "resume"               -> Just resumeTests
  "reverse"              -> Just reverseTests
  "sort"                 -> Just sortTests
  "str-to-int"           -> Just strToIntTests
  "take"                 -> Just takeTests
  "try"                  -> Just tryTests
  "tx-hash"              -> Just txHashTests
  "typeof"               -> Just typeOfTests
  "where"                -> Just whereTests
  "yield"                -> Just yieldTests

  -- | Operators native functions
  "!="      -> Just notEqualOptTests
  "&"       -> Just bitwiseOptTests
  "*"       -> Just multOptTests
  "+"       -> Just addOptTests
  "-"       -> Just subOptTests
  "/"       -> Just divOptTests
  "<"       -> Just lessThanOptTests
  "<="      -> Just lessThanEqualOptTests
  "="       -> Just equalOptTests
  ">"       -> Just greaterThanOptTests
  ">="      -> Just greaterThanEqOptTests
  "^"       -> Just raiseOptTests
  "abs"     -> Just absOptTests
  "and"     -> Just andOptTests
  "and?"    -> Just andFuncOptTests
  "ceiling" -> Just ceilingOptTests
  "exp"     -> Just expOptTests
  "floor"   -> Just floorOptTests
  "ln"      -> Just lnOptTests
  "log"     -> Just logOptTests
  "mod"     -> Just modOptTests
  "not"     -> Just notOptTests
  "not?"    -> Just notFuncOptTests
  "or"      -> Just orOptTests
  "or?"     -> Just orFuncOptTests
  "round"   -> Just roundOptTests
  "shift"   -> Just shiftOptTests
  "sqrt"    -> Just sqrtOptTests
  "xor"     -> Just xorOptTests
  "|"       -> Just bitwiseOrOptTests
  "~"       -> Just reverseBitsOptTests

  -- | Time native functions
  "add-time"    -> Just addTimeTests
  "days"        -> Just daysTests
  "diff-time"   -> Just diffTimeTests
  "format-time" -> Just formatTimeTests
  "hours"       -> Just hoursTests
  "minutes"     -> Just minutesTests
  "parse-time"  -> Just parseTimeTests
  "time"        -> Just timeTests

  -- | Commitments native functions
  "decrypt-cc20p1305" -> Just decryptCc20p1305Tests
  "validate-keypair"  -> Just validateKeypairTests

  -- | Keyset native functions
  "define-keyset"  -> Just defineKeysetTests
  "enforce-keyset" -> Just enforceKeysetTests
  "keys-2"         -> Just keys2Tests
  "keys-all"       -> Just keysAllTests
  "keys-any"       -> Just keysAnyTests
  "read-keyset"    -> Just readKeysetTests

  -- | Database nataive functions
  "create-table"      -> Just createTableTests
  "describe-keyset"   -> Just describeKeysetTests
  "describe-module"   -> Just describeModuleTests
  "describe-table"    -> Just describeTableTests
  "insert"            -> Just insertTests
  "keylog"            -> Just keylogTests
  "keys"              -> Just keysTests
  "read"              -> Just readTests
  "select"            -> Just selectTests
  "txids"             -> Just txidsTests
  "txlog"             -> Just txlogTests
  "update"            -> Just updateTests
  "with-default-read" -> Just withDefaultReadTests
  "with-read"         -> Just withReadTests
  "write"             -> Just writeTests

  _ -> Nothing


-- | Database native function tests
--   NOTE: Using MockDb means that database insert/write/update always succeed
txlogTests :: GasTestBackend -> GasUnitTests
txlogTests backendType = GasUnitTests tests
  where
    keyLogExpr = [text| (txlog $moduleName.accounts 1) |]

    acctRow = ObjectMap $ M.fromList [("balance", PLiteral (LDecimal 100.0))]
    getTxLogRead :: Domain k v -> TxId -> Method () [TxLog v]
    getTxLogRead UserTables {} _ = rc [TxLog "accounts.accounts" "some-id" acctRow]
    getTxLogRead _ _ = rc []

    mockDb = def { mockGetTxLog = MockGetTxLog getTxLogRead }
    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv mockDb state

    test = GasTest keyLogExpr keyLogExpr env mockEnvCleanup
    tests = SomeGasTest defMockGasTest :| []  


txidsTests :: GasUnitTests
txidsTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    txidsExpr = [text| (txids $moduleName.accounts 1) |]

    txIdsRead :: TableName -> TxId -> Method () [TxId]
    txIdsRead _ i = rc [i]

    mockDb = def { mockTxIds = MockTxIds txIdsRead }
    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv mockDb state

    test = GasTest txidsExpr txidsExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


keylogTests :: GasUnitTests
keylogTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    keyLogExpr = [text| (keylog $moduleName.accounts "some-id" 1) |]

    txIdsRead :: TableName -> TxId -> Method () [TxId]
    txIdsRead _ i = rc [i]

    acctRow = ObjectMap $ M.fromList [("balance", PLiteral (LDecimal 100.0))]
    getTxLogRead :: Domain k v -> TxId -> Method () [TxLog v]
    getTxLogRead UserTables {} _ = rc [TxLog "accounts.accounts" "some-id" acctRow]
    getTxLogRead _ _ = rc []

    mockDb = def { mockTxIds = MockTxIds txIdsRead,
                   mockGetTxLog = MockGetTxLog getTxLogRead
                 }
    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv mockDb state

    test = GasTest keyLogExpr keyLogExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


keysTests :: GasUnitTests
keysTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    keysExpr = [text| (keys $moduleName.accounts) |]
    
    keysRead :: Domain k v -> Method () [k]
    keysRead UserTables {} = rc ["some-id"]
    keysRead _ = rc []

    mockDb = def { mockKeys = MockKeys keysRead }
    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv mockDb state

    test = GasTest keysExpr keysExpr env mockEnvCleanup
    tests = SomeGasTest test :| []



selectTests :: GasUnitTests
selectTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    selectExpr = [text| (select $moduleName.accounts (where "balance" (constantly true)) ) |]

    acctRow = ObjectMap $ M.fromList [("balance", PLiteral (LDecimal 100.0))]
    rowRead :: Domain k v -> k -> Method () (Maybe v)
    rowRead UserTables {} _ = rc (Just acctRow)
    rowRead _ _ = rc Nothing

    keysRead :: Domain k v -> Method () [k]
    keysRead UserTables {} = rc ["some-id"]
    keysRead _ = rc []

    mockDb = def { mockRead = MockRead rowRead,
                   mockKeys = MockKeys keysRead
                 }
    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv mockDb state

    test = GasTest selectExpr selectExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


withReadTests :: GasUnitTests
withReadTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    withReadExpr =
      [text| (with-read
                $moduleName.accounts
                "some-id"
                { "balance":= bal }
                bal
             )
      |]

    acctRow = ObjectMap $ M.fromList [("balance", PLiteral (LDecimal 100.0))]
    rowRead :: Domain k v -> k -> Method () (Maybe v)
    rowRead UserTables {} _ = rc (Just acctRow)
    rowRead _ _ = rc Nothing
    mockDb = def { mockRead = MockRead rowRead }

    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv mockDb state

    test = GasTest withReadExpr
                   (withReadExpr <> " where row IS found.")
                   env
                   mockEnvCleanup
    tests = SomeGasTest test :| []


withDefaultReadTests :: GasUnitTests
withDefaultReadTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    withDefReadExpr =
      [text| (with-default-read
                $moduleName.accounts
                "some-id"
                { "balance": 1.0 }
                { "balance":= bal }
                bal
             )
      |]

    acctRow = ObjectMap $ M.fromList [("balance", PLiteral (LDecimal 100.0))]
    rowRead :: Domain k v -> k -> Method () (Maybe v)
    rowRead UserTables {} _ = rc (Just acctRow)
    rowRead _ _ = rc Nothing
    mockDb = def { mockRead = MockRead rowRead }

    envEmptyRead = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv def state

    envWithRow = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv mockDb state

    testUsingDef =
      GasTest withDefReadExpr
              (withDefReadExpr <> " where NO row found.")
              envEmptyRead
              mockEnvCleanup
    testNotUsingDef =
      GasTest withDefReadExpr
              (withDefReadExpr <> " where row IS found.")
              envWithRow
              mockEnvCleanup
    tests = SomeGasTest testUsingDef :|
            [SomeGasTest testNotUsingDef]


readTests :: GasUnitTests
readTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    readExpr = [text| (read $moduleName.accounts "someId") |]

    acctRow = ObjectMap $ M.fromList [("balance", PLiteral (LDecimal 100.0))]
    rowRead :: Domain k v -> k -> Method () (Maybe v)
    rowRead UserTables {} _ = rc (Just acctRow)
    rowRead _ _ = rc Nothing
    mockDb = def { mockRead = MockRead rowRead }

    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv mockDb state

    test = GasTest readExpr readExpr env mockEnvCleanup
    tests = SomeGasTest test :| []

writeTests :: GasUnitTests
writeTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    writeExpr = [text| (write $moduleName.accounts "someId" { "balance": 0.0 }) |]

    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv def state

    test = GasTest writeExpr writeExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


updateTests :: GasUnitTests
updateTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    updateExpr = [text| (update $moduleName.accounts "someId" { "balance": 0.0 }) |]

    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv def state

    test = GasTest updateExpr updateExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


insertTests :: GasUnitTests
insertTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    insertExpr = [text| (insert $moduleName.accounts "someId" { "balance": 0.0 })|]
    
    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv def state

    test = GasTest insertExpr insertExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


describeTableTests :: GasUnitTests
describeTableTests = GasUnitTests tests
  where
    describeTableExpr = [text| (describe-table $moduleName.accounts) |]

    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv def state


    test = GasTest describeTableExpr describeTableExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


describeModuleTests :: GasUnitTests
describeModuleTests = GasUnitTests tests
  where
    describeModuleExpr = [text| (describe-module "$moduleName") |]

    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv def state
      
    test = GasTest describeModuleExpr describeModuleExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


describeKeysetTests :: GasUnitTests
describeKeysetTests = GasUnitTests tests
  where
    describeKeysetExpr = [text| (describe-keyset "some-keyset") |]
    
    someKeyset = KeySet [PublicKey "something"] (Name "keys-all" def)
    ksRead :: Domain k v -> k -> Method () (Maybe v)
    ksRead KeySets _ = rc (Just someKeyset)
    ksRead _ _ = rc Nothing

    mockdb = def {mockRead = MockRead ksRead }
    testEnv = createMockEnv mockdb defEvalState

    test = GasTest describeKeysetExpr describeKeysetExpr testEnv mockEnvCleanup
    tests = SomeGasTest test :| []
    

createTableTests :: GasUnitTests
createTableTests = GasUnitTests tests
  where
    moduleName = "accounts"
    moduleCode m = [text|
     (module $m GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (deftable accounts:{account})
     ) |]

    env = do
      state <- getLoadedState (moduleCode moduleName) id defEvalState
      createMockEnv def state
      
    createTableExpr = [text| (create-table $moduleName.accounts) |]
    test = GasTest createTableExpr createTableExpr env mockEnvCleanup
    tests = SomeGasTest test :| []


-- | Keyset native function tests
defineKeysetTests :: GasUnitTests
defineKeysetTests = GasUnitTests tests
  where
    tests = simpleDefTest :| [rotateKeysetTest]

    simpleDefTest = SomeGasTest $ updateStateWithKeysetLoaded (defGasTest simpleDefExpr)
      where
        simpleDefExpr =
          [text| (define-keyset 'my-pact-keyset some-loaded-keyset) |]
        loadedKeyset = KeySet [PublicKey "something"] (Name "keys-all" def)
        loaded = HM.singleton (Name "some-loaded-keyset" def)
                 (Direct $ TGuard (GKeySet loadedKeyset) def)
        updateStateWithKeysetLoaded = setState $ set (evalRefs . rsLoaded) loaded

    rotateKeysetTest = SomeGasTest $ rotateTest
      where
        name = "my-pact-keyset"
        rotateExpr = [text| (define-keyset '$name some-loaded-keyset) |]
        loaded = HM.singleton (Name "some-loaded-keyset" def)
                 (Direct $ TGuard (GKeySet oldKeyset) def)
        stateWithKeysetLoaded = set (evalRefs . rsLoaded) loaded defEvalState

        oldPublicKeys = [PublicKey "something"]
        oldKeyset = KeySet oldPublicKeys (Name "keys-all" def)
        updateEnvMsgSig = setEnv (set eeMsgSigs (S.fromList oldPublicKeys))

        ksRead :: Domain k v -> k -> Method () (Maybe v)
        ksRead KeySets _ = rc (Just oldKeyset)
        ksRead _ _ = rc Nothing

        mockdb = def { mockRead = MockRead ksRead }
        testEnv = createMockEnv mockdb stateWithKeysetLoaded

        gasTest = GasTest rotateExpr
                  (rotateExpr <> ": Defining keyset with the same name as one already defined.")
                  testEnv
                  mockEnvCleanup

        rotateTest = updateEnvMsgSig gasTest


enforceKeysetTests :: GasUnitTests
enforceKeysetTests = GasUnitTests tests
  where
    enforceKeysetExpr = [text| (enforce-keyset 'my-keyset) |]

    pubKeys = [PublicKey "something"]
    keyset = KeySet pubKeys (Name "keys-all" def)
    updateEnvWithSig = setEnv $ set eeMsgSigs (S.fromList pubKeys)
    
    ksRead :: Domain k v -> k -> Method () (Maybe v)
    ksRead KeySets _ = rc (Just keyset)
    ksRead _ _ = rc Nothing

    mockdb = def { mockRead = MockRead ksRead }
    testEnv = createMockEnv mockdb defEvalState

    gasTest = updateEnvWithSig $
              GasTest enforceKeysetExpr
                      enforceKeysetExpr
                      testEnv
                      mockEnvCleanup
    tests = SomeGasTest gasTest :| []
    

readKeysetTests :: GasUnitTests
readKeysetTests = GasUnitTests tests
  where
    readKeysetExpr = [text| (read-keyset 'my-keyset) |]
    dataWithKeyset = toPactKeyset "my-keyset" "something" Nothing
    test = setEnv (set eeMsgBody dataWithKeyset) (defGasTest readKeysetExpr)

    tests = SomeGasTest test :| []


keysAnyTests :: GasUnitTests
keysAnyTests = defGasUnitTests allExprs
  where
    allExprs = [text|(keys-any 10 1)|] :| []


keysAllTests :: GasUnitTests
keysAllTests = defGasUnitTests allExprs
  where
    allExprs = [text|(keys-all 3 3)|] :| []


keys2Tests :: GasUnitTests
keys2Tests = defGasUnitTests allExprs
  where
    allExprs = [text|(keys-2 3 1)|] :| []



-- | Commitments native function tests
decryptCc20p1305Tests :: GasUnitTests
decryptCc20p1305Tests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (decrypt-cc20p1305
              "Zi1REj5-iA"
              "AAAAAAECAwQFBgcI"
              "YWFk"
              "FYP6lG7xq7aExvoaHIH8Jg"
              "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
              "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
      |] :| []

validateKeypairTests :: GasUnitTests
validateKeypairTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (validate-keypair
             "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
             "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a") |]
        :| []


-- | Time native function tests
addTimeTests :: GasUnitTests
addTimeTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (add-time (time "2016-07-22T12:00:00Z") 15) |] :| []


daysTests :: GasUnitTests
daysTests = defGasUnitTests allExprs
  where
    daysExpr n =
      [text| (days $n) |]
    allExprs = NEL.map daysExpr sizesExpr


diffTimeTests :: GasUnitTests
diffTimeTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (diff-time (time "2016-07-22T12:00:00Z")
                        (time "2018-07-22T12:00:00Z"))
      |] :| []


formatTimeTests :: GasUnitTests
formatTimeTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (format-time "%F" (time "2016-07-22T12:00:00Z")) |] :|
      [[text| (format-time "%Y-%m-%dT%H:%M:%S%N" (time "2016-07-23T13:30:45Z")) |]]


hoursTests :: GasUnitTests
hoursTests = defGasUnitTests allExprs
  where
    hoursExpr n =
      [text| (hours $n) |]
    allExprs = NEL.map hoursExpr sizesExpr


minutesTests :: GasUnitTests
minutesTests = defGasUnitTests allExprs
  where
    minutesExpr n =
      [text| (minutes $n) |]
    allExprs = NEL.map minutesExpr sizesExpr


parseTimeTests :: GasUnitTests
parseTimeTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (parse-time "%F" "2016-07-22") |] :|
      [[text| (parse-time "%Y-%m-%dT%H:%M:%S%N" "2016-07-23T13:30:45+00:00") |]]


timeTests :: GasUnitTests
timeTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (time "2016-07-22T12:00:00Z") |] :| []


-- | Operators native function tests
reverseBitsOptTests :: GasUnitTests
reverseBitsOptTests = defGasUnitTests allExprs
  where
    reverseBitsExpr x = [text| (~ $x) |]

    allExprs = NEL.map reverseBitsExpr sizesExpr


bitwiseOrOptTests :: GasUnitTests
bitwiseOrOptTests = defGasUnitTests allExprs
  where
    bitwiseOrExpr x = [text| (| 2 $x) |]

    allExprs = NEL.map bitwiseOrExpr sizesExpr


xorOptTests :: GasUnitTests
xorOptTests = defGasUnitTests allExprs
  where
    xorExpr x = [text| (xor 2 $x) |]

    allExprs = NEL.map xorExpr sizesExpr


sqrtOptTests :: GasUnitTests
sqrtOptTests = defGasUnitTests allExprs
  where
    sqrtExpr x = [text| (sqrt $x) |]
    sqrtDecimalExpr x = [text| (sqrt $x.1) |]

    allExprs = NEL.map sqrtExpr sizesExpr
      <> NEL.map sqrtDecimalExpr sizesExpr


shiftOptTests :: GasUnitTests
shiftOptTests = defGasUnitTests allExprs
  where
    shiftExpr x =
      [text| (shift 2 $x) |]
    shiftNegExpr x =
      [text| (shift -2 $x) |]

    allExprs = NEL.map shiftExpr sizesExpr
      <> NEL.map shiftNegExpr sizesExpr


roundOptTests :: GasUnitTests
roundOptTests = defGasUnitTests allExprs
  where
    roundExpr x =
      [text| (round $x.12345) |]
    roundPrecExpr x =
      [text| (round $x.12345 4) |]

    allExprs = NEL.map roundExpr sizesExpr
      <> NEL.map roundPrecExpr sizesExpr


orFuncOptTests :: GasUnitTests
orFuncOptTests = defGasUnitTests allExprs
  where
    orFuncExpr = [text| (or? (identity) (identity) true) |]

    allExprs = orFuncExpr :| []


orOptTests :: GasUnitTests
orOptTests = defGasUnitTests allExprs
  where
    orExpr = [text| (or false false) |]

    allExprs = orExpr :| []


notFuncOptTests :: GasUnitTests
notFuncOptTests = defGasUnitTests allExprs
  where
    notFuncExpr = [text| (not? (identity) true) |]

    allExprs = notFuncExpr :| []


notOptTests :: GasUnitTests
notOptTests = defGasUnitTests allExprs
  where
    notExpr = [text| (not true) |]

    allExprs = notExpr :| []


modOptTests :: GasUnitTests
modOptTests = defGasUnitTests allExprs
  where
    modExpr x =
      [text| (mod $x 2) |]

    allExprs = NEL.map modExpr sizesExpr



logOptTests :: GasUnitTests
logOptTests = defGasUnitTests allExprs
  where
    logExpr y =
      [text| (log 2 $y) |]
    logDecimalExpr y =
      [text| (log 2 $y.1) |]

    allExprs = NEL.map logExpr sizesExpr
      <> NEL.map logDecimalExpr sizesExpr



lnOptTests :: GasUnitTests
lnOptTests = defGasUnitTests allExprs
  where
    lnExpr x =
      [text| (ln $x) |]
    lnDecimalExpr x =
      [text| (ln $x.1) |]

    allExprs = NEL.map lnExpr sizesExpr
      <> NEL.map lnDecimalExpr sizesExpr


floorOptTests :: GasUnitTests
floorOptTests = defGasUnitTests allExprs
  where
    floorExpr x =
      [text| (floor $x.12345) |]
    floorPrecExpr x =
      [text| (floor $x.12345 4) |]

    allExprs = NEL.map floorExpr sizesExpr
      <> NEL.map floorPrecExpr sizesExpr


expOptTests :: GasUnitTests
expOptTests = defGasUnitTests allExprs
  where
    expExprSmall =
      [text| (exp 1) |]
    expExprMed =
      [text| (exp 10) |]
    expExprLarge =
      [text| (exp 100) |]

    allExprs = expExprSmall :| [expExprMed, expExprLarge]


ceilingOptTests :: GasUnitTests
ceilingOptTests = defGasUnitTests allExprs
  where
    ceilingExpr x =
      [text| (ceiling $x.12345) |]
    ceilingPrecExpr x =
      [text| (ceiling $x.12345 4) |]

    allExprs = NEL.map ceilingExpr sizesExpr
      <> NEL.map ceilingPrecExpr sizesExpr
        

andFuncOptTests :: GasUnitTests
andFuncOptTests = defGasUnitTests allExprs
  where
    andFuncExpr =
      [text| (and? (identity) (identity) true) |]

    allExprs = andFuncExpr :| []


andOptTests :: GasUnitTests
andOptTests = defGasUnitTests allExprs
  where
    andExpr =
      [text| (and false true) |]

    allExprs = andExpr :| []


absOptTests :: GasUnitTests
absOptTests = defGasUnitTests allExprs
  where
    absExpr x =
      [text| (abs -$x) |]
    absDecimalExpr x =
      [text| (abs -$x.0) |]

    allExprs = NEL.map absExpr sizesExpr
      <> NEL.map absDecimalExpr sizesExpr

        
raiseOptTests :: GasUnitTests
raiseOptTests = defGasUnitTests allExprs
  where
    raiseExpr y = 
      [text| (^ 2 $y) |]
    raiseDecimalExpr y =
      [text| (^ 2.1 $y.1) |]
    raiseBothExpr y =
      [text| (^ 2.1 $y) |]

    allExprs = NEL.map raiseExpr sizesExpr
      <> NEL.map raiseDecimalExpr sizesExpr
      <> NEL.map raiseBothExpr sizesExpr


greaterThanEqOptTests :: GasUnitTests
greaterThanEqOptTests = defGasUnitTests allExprs
  where
    greaterEqExpr x =
      [text| (>= $x $x) |]
    greaterEqDecimalExpr x =
      [text| (>= $x.0 $x.0) |]
    greaterEqTimeExpr =
      [text| (>= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
        :| []
   
    allExprs = NEL.map greaterEqExpr sizesExpr
      <> NEL.map greaterEqDecimalExpr sizesExpr
      <> NEL.map greaterEqExpr escapedStringsExpr
      <> greaterEqTimeExpr


greaterThanOptTests :: GasUnitTests
greaterThanOptTests = defGasUnitTests allExprs
  where
    greaterExpr x =
      [text| (> $x $x) |]
    greaterDecimalExpr x =
      [text| (> $x.0 $x.0) |]
    greaterTimeExpr =
      [text| (> (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
        :| []
   
    allExprs = NEL.map greaterExpr sizesExpr
      <> NEL.map greaterDecimalExpr sizesExpr
      <> NEL.map greaterExpr escapedStringsExpr
      <> greaterTimeExpr
    


equalOptTests :: GasUnitTests
equalOptTests = defGasUnitTests allExprs
  where
    eqExpr x =
      [text| (= $x $x) |]
    eqDecimalExpr x =
      [text| (= $x.0 $x.0) |]
    eqTimeExpr =
      [text| (= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
        :| []

    allExprs = NEL.map eqExpr sizesExpr
      <> NEL.map eqDecimalExpr sizesExpr
      <> NEL.map eqExpr escapedStringsExpr
      <> NEL.map eqExpr strKeyIntValMapsExpr
      <> NEL.map eqExpr intListsExpr
      <> eqTimeExpr


lessThanEqualOptTests :: GasUnitTests
lessThanEqualOptTests = defGasUnitTests allExprs
  where
    lessEqExpr x =
      [text| (<= $x $x) |]
    lessEqDecimalExpr x =
      [text| (<= $x.0 $x.0) |]
    lessEqTimeExpr =
      [text| (<= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
        :| []
   
    allExprs = NEL.map lessEqExpr sizesExpr
      <> NEL.map lessEqDecimalExpr sizesExpr
      <> NEL.map lessEqExpr escapedStringsExpr
      <> lessEqTimeExpr


lessThanOptTests :: GasUnitTests
lessThanOptTests = defGasUnitTests allExprs
  where
    lessExpr x =
      [text| (< $x $x) |]
    lessDecimalExpr x =
      [text| (< $x.0 $x.0) |]
    lessTimeExpr =
      [text| (< (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
        :| []
   
    allExprs = NEL.map lessExpr sizesExpr
      <> NEL.map lessDecimalExpr sizesExpr
      <> NEL.map lessExpr escapedStringsExpr
      <> lessTimeExpr


divOptTests :: GasUnitTests
divOptTests = defGasUnitTests allExprs
  where
    divExpr x =
      [text| (/ $x $x) |]
    divDecimalExpr x =
      [text| (/ $x.0 $x.0) |]
    divBothExpr x =
      [text| (/ $x.0 $x) |]
   
    allExprs = NEL.map divExpr sizesExpr
      <> NEL.map divDecimalExpr sizesExpr
      <> NEL.map divBothExpr sizesExpr


subOptTests :: GasUnitTests
subOptTests = defGasUnitTests allExprs
  where
    subExpr x =
      [text| (+ $x $x) |]
    subDecimalExpr x =
      [text| (+ $x.0 $x.0) |]
    subBothExpr x =
      [text| (+ $x.0 $x) |]
    subOneExpr x =
      [text| (- $x) |]
    subOneDecimalExpr x =
      [text| (- $x.0) |]
   
    allExprs = NEL.map subExpr sizesExpr
      <> NEL.map subDecimalExpr sizesExpr
      <> NEL.map subBothExpr sizesExpr
      <> NEL.map subOneExpr sizesExpr
      <> NEL.map subOneDecimalExpr sizesExpr


addOptTests :: GasUnitTests
addOptTests = defGasUnitTests allExprs
  where
    addExpr x =
      [text| (+ $x $x) |]
    addDecimalExpr x =
      [text| (+ $x.0 $x.0) |]
    addBothExpr x =
      [text| (+ $x.0 $x) |]

    allExprs = NEL.map addExpr sizesExpr
      <> NEL.map addDecimalExpr sizesExpr
      <> NEL.map addBothExpr sizesExpr
      <> NEL.map addExpr escapedStringsExpr
      <> NEL.map addExpr strKeyIntValMapsExpr


multOptTests :: GasUnitTests
multOptTests = defGasUnitTests allExprs
  where
    multIntExpr x =
      [text| (* $x $x) |]
    multDecimalExpr x =
      [text| (* $x.0 $x.0) |]
    multBothExpr x =
      [text| (* $x.0 $x) |]

    allExprs = NEL.map multIntExpr sizesExpr
      <> NEL.map multDecimalExpr sizesExpr
      <> NEL.map multBothExpr sizesExpr


bitwiseOptTests :: GasUnitTests
bitwiseOptTests = defGasUnitTests allExprs
  where
    bitwiseExpr x =
      [text| (& $x $x) |]

    allExprs = NEL.map bitwiseExpr sizesExpr


notEqualOptTests :: GasUnitTests
notEqualOptTests = defGasUnitTests allExprs
  where
    notEqualExpr x =
      [text| (!= $x $x) |]
    notEqualDecimalExpr x =
      [text| (!= $x.0 $x.0) |]
        
    allExprs = NEL.map notEqualExpr sizesExpr
      <> NEL.map notEqualExpr escapedStringsExpr
      <> NEL.map notEqualDecimalExpr sizesExpr
      <> NEL.map notEqualExpr intListsExpr
      <> NEL.map notEqualExpr strKeyIntValMapsExpr



-- | General native function tests
whereTests :: GasUnitTests
whereTests = defGasUnitTests allExprs
  where
    whereExpr obj =
      [text| (where "a1" (constantly true) $obj) |]

    allExprs = NEL.map whereExpr strKeyIntValMapsExpr


typeOfTests :: GasUnitTests
typeOfTests = defGasUnitTests allExprs
  where
    typeOfExpr t =
      [text| (typeof $t) |]

    allExprs = NEL.map typeOfExpr strKeyIntValMapsExpr
      <> NEL.map typeOfExpr escapedStringsExpr
      <> NEL.map typeOfExpr intListsExpr
      <> NEL.map (typeOfExpr . toText . MockInt) sizes


txHashTests :: GasUnitTests
txHashTests = defGasUnitTests allExprs
  where
    allExprs = [text| (tx-hash) |] :| []


tryTests :: GasUnitTests
tryTests = defGasUnitTests allExprs
  where
    tryPassExpr =
      [text| (try true (enforce true "this will definitely pass")) |]
    tryFailExpr =
      [text| (try true (enforce false "this will definitely fail")) |]

    allExprs = tryPassExpr :| [ tryFailExpr ]


takeTests :: GasUnitTests
takeTests = defGasUnitTests allExprs
  where
    takeFirstExpr t =
      [text| (take 1 $t) |]
    takeLastExpr t =
      [text| (take -1 $t) |]
    takeKeysExpr (keyList, obj) =
      [text| (take $keyList $obj) |]
    takeSingleKeyExpr obj =
      [text| (take ["a1"] $obj) |]
    
    keysToTakeArgs = NEL.zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         NEL.map takeFirstExpr intListsExpr
      <> NEL.map takeLastExpr intListsExpr
      <> NEL.map takeKeysExpr keysToTakeArgs
      <> NEL.map takeSingleKeyExpr strKeyIntValMapsExpr


strToIntTests :: GasUnitTests
strToIntTests = defGasUnitTests allExprs
  where
    str2intExpr valInt =
      [text| (str-to-int $valStr) |]
        where valStr = escapeText $ intToStr valInt
    str2intLongHex = [text| (str-to-int 16 "186A0") |]
    str2intMedHex = [text| (str-to-int 16 "64") |]
    str2intSmallHex = [text| (str-to-int 16 "a") |]

    str2intLongBinary = [text| (str-to-int 2 "11000011010100000") |]
    str2intMedBinary = [text| (str-to-int 2 "1100100") |]
    str2intSmallBinary = [text| (str-to-int 2 "1010") |]
      
    str2intLongBase64 = [text| (str-to-int 64 "AYag") |]
    str2intMedBase64 = [text| (str-to-int 64 "ZA") |]
    str2intSmallBase64 = [text| (str-to-int 64 "Cg") |]
    
    allExprs = NEL.map str2intExpr sizes
      <> (str2intLongHex :|
          [ str2intMedHex,
            str2intSmallHex,
            
            str2intLongBinary,
            str2intMedBinary,
            str2intSmallBinary,
            
            str2intLongBase64,
            str2intMedBase64,
            str2intSmallBase64
          ])


sortTests :: GasUnitTests
sortTests = defGasUnitTests allExprs
  where
    sortListExpr li =
      [text| (sort $li) |]

    reversedListExpr = NEL.map makeExpr intLists
    makeExpr li = toText $ MockList
                  $ map MockInt (reverse $ NEL.toList li)
        
    allExprs = NEL.map sortListExpr reversedListExpr


reverseTests :: GasUnitTests
reverseTests = defGasUnitTests allExprs
  where
    reverseExpr li =
      [text| (reverse $li) |]

    allExprs = NEL.map reverseExpr intListsExpr


removeTests :: GasUnitTests
removeTests = defGasUnitTests allExprs
  where
    removeExpr obj =
      [text| (remove "a1" $obj) |]

    allExprs = NEL.map removeExpr strKeyIntValMapsExpr


pactIdTests :: GasUnitTests
pactIdTests = GasUnitTests tests
  where
    pactIdExpr = [text|(pact-id)|]
    
    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name "some-defpact-func" def) [])
    updateState = setState (set evalPactExec mockPactExec)
    test = updateState $ defGasTest pactIdExpr
    tests = SomeGasTest test :| []


yieldTests :: GasUnitTests
yieldTests = GasUnitTests tests
  where
    yieldExprWithTargetChain obj = [text| (yield $obj "some-chain-id") |]
    yieldExpr obj = [text| (yield $obj) |]

    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name "some-defpact-func" def) [])

    mockModules = HM.fromList [(defModuleName, defModuleData)]
    mockStackframe = [defStackFrame]
    updateStateWithStackFrame = setState (set evalCallStack mockStackframe)
    updateStateWithPactExec = setState (set evalPactExec mockPactExec)

    setInitialState = const (initStateModules mockModules)
    mkGasTestWithModule e = setState setInitialState
                            $ GasTest e e
                              createDefMockEnv
                              mockEnvCleanup
    
    tests =    NEL.map (SomeGasTest .
                        updateStateWithStackFrame .
                        updateStateWithPactExec .
                        mkGasTestWithModule .
                        yieldExprWithTargetChain) strKeyIntValMapsExpr
            <> NEL.map (SomeGasTest .
                        updateStateWithPactExec .
                        defGasTest .
                        yieldExpr) strKeyIntValMapsExpr


resumeTests :: GasUnitTests
resumeTests = GasUnitTests tests
  where
    resumeExpr binding = [text|(resume $binding a1)|]

    yieldData m = (ObjectMap . M.fromList . toPactValueInt . HM.toList) m
       where toPactValueInt = map (\(t,v) -> (FieldKey t, PLiteral $ LInteger v))

    chainId = ChainId "some-chain-id"
    provenance = Just $ Provenance chainId defModuleHash
    toYield m = Yield (yieldData m)
    
    toPactStep y = Just $ PactStep 2 False (PactId "") (Just y)

    args = NEL.zip strKeyIntValMaps strKeyIntValBindingsExpr

    testWithSPV (m, bindExpr) = setupTestWithSPV gasTestWithModule
      where updateEnvWithProvidence = setEnv (set eePactStep (toPactStep $ toYield m provenance))
            updateEnvWithChainId = setEnv (set (eePublicData . pdPublicMeta . pmChainId) chainId)
            updateStateWithStackFrame = setState (set evalCallStack [defStackFrame])
            updateGasTestDesc = set gasTestDescription $ (resumeExpr bindExpr) <> " with provenance"
            
            mockModules = HM.fromList [(defModuleName, defModuleData)]
            setInitialState = const (initStateModules mockModules)
            gasTestWithModule = setState setInitialState
                                $ GasTest
                                  (resumeExpr bindExpr)
                                  (resumeExpr bindExpr)
                                  createDefMockEnv
                                  mockEnvCleanup
            setupTestWithSPV = SomeGasTest .
                               updateGasTestDesc .
                               updateEnvWithProvidence .
                               updateEnvWithChainId .
                               updateStateWithStackFrame

    testSimple (m, bindExpr) = setupTestSimple $ defGasTest (resumeExpr bindExpr)
      where updateEnvWithYield = setEnv (set eePactStep (toPactStep $ toYield m Nothing))
            setupTestSimple = SomeGasTest . updateEnvWithYield

    tests =    NEL.map testWithSPV args
            <> NEL.map testSimple args


pactVersionTests :: GasUnitTests
pactVersionTests = defGasUnitTests allExprs
  where
    versionExpr =
      [text| (pact-version) |]

    allExprs = versionExpr :| []


readMsgTests :: GasUnitTests
readMsgTests = GasUnitTests tests
  where
    readMsgExpr = [text|(read-msg)|]
    setupTest g m = (updateDesc . updateEnv) g
      where updateEnv = setEnv (set eeMsgBody $ toJSON m)
            updateDesc = set gasTestDescription
                             (readMsgExpr <> " when "
                              <> (T.pack $ show $ toJSON m))
    tests' = NEL.map (setupTest $ defGasTest readMsgExpr) strKeyIntValMapsExpr
    tests = NEL.map SomeGasTest tests'


readIntegerTests :: GasUnitTests
readIntegerTests = GasUnitTests tests
  where
    readIntExpr = [text|(read-integer "amount")|]
    formatToIntVal i = A.object ["amount" A..= i]
    setupTest g i = (updateDesc . updateEnv) g
      where updateEnv = setEnv (set eeMsgBody $ formatToIntVal i)
            updateDesc = set gasTestDescription
                             (readIntExpr <> " when "
                              <> (T.pack $ show $ formatToIntVal i))
    tests' = NEL.map (setupTest $ defGasTest readIntExpr) sizesExpr
    tests = NEL.map SomeGasTest tests'


readDecimalTests :: GasUnitTests
readDecimalTests = GasUnitTests tests
  where
    readDecExpr = [text|(read-decimal "amount")|]
    formatToDecVal d = A.object ["amount" A..= decText]
      where d' = d <> "1"
            decText = [text|0.$d'|]
    setupTest g d = (updateDesc . updateEnv) g
      where updateEnv = setEnv (set eeMsgBody $ formatToDecVal d)
            updateDesc = set gasTestDescription
                             (readDecExpr <> " when "
                              <> (T.pack $ show $ formatToDecVal d))
    tests' = NEL.map (setupTest $ defGasTest readDecExpr) sizesExpr
    tests = NEL.map SomeGasTest tests'


mapTests :: GasUnitTests
mapTests = defGasUnitTests allExprs
  where
    mapExpr li =
      [text| (map (identity) $li) |]

    allExprs = NEL.map mapExpr intListsExpr


makeListTests :: GasUnitTests
makeListTests = defGasUnitTests allExprs
  where
    makeListExpr len =
      [text| (make-list $len true) |]

    allExprs = NEL.map (makeListExpr . intToStr) sizes


listModulesTests :: GasUnitTests
listModulesTests = defGasUnitTests allExprs
  where
    listModulesExpr =
      [text| (list-modules) |]

    allExprs = listModulesExpr :| []


lengthTests :: GasUnitTests
lengthTests = defGasUnitTests allExprs
  where
    lengthExpr t =
      [text| (length $t) |]

    allExprs =
         NEL.map lengthExpr intListsExpr
      <> NEL.map lengthExpr escapedStringsExpr
      <> NEL.map lengthExpr strKeyIntValMapsExpr


intToStrTests :: GasUnitTests
intToStrTests = defGasUnitTests allExprs
  where
    int2strExpr (valInt,baseInt) =
      [text| (int-to-str $base $val) |]
      where base = intToStr baseInt
            val = intToStr valInt

    baseList :: NonEmpty Integer
    baseList = 64 :| [2..16]
    -- TODO is foldr1 the best thing to do here
    -- | Test every base conversion against three different number sizes
    args = F.foldr1 (<>) $ NEL.map (\n -> NEL.map (\b -> (n,b)) baseList) sizes
    
    allExprs = NEL.map int2strExpr args


ifTests :: GasUnitTests
ifTests = defGasUnitTests allExprs
  where
    ifExpr =
      [text| (if true "then-clause" "else-clause") |] :| []

    allExprs = ifExpr


identityTests :: GasUnitTests
identityTests = defGasUnitTests allExprs
  where
    identityExpr val =
      [text| (identity $val) |]

    allExprs = NEL.map identityExpr intListsExpr


hashTests :: GasUnitTests
hashTests = defGasUnitTests allExprs
  where
    hashExpr val =
      [text| (hash $val) |]

    allExprs =
         NEL.map hashExpr escapedStringsExpr
      <> NEL.map hashExpr strKeyIntValMapsExpr

formatTests :: GasUnitTests
formatTests = defGasUnitTests allExprs
  where
    formatExpr (str,li) =
      [text| (format "$str" $li )|]

    formatStrs =
      NEL.map
      (\n -> T.unwords $ replicate (fromIntegral n) "{}")
      sizes
    strListArgs = NEL.zip formatStrs escapedStrListsExpr
    intListArgs = NEL.zip formatStrs intListsExpr

    allExprs =
         NEL.map formatExpr strListArgs
      <> NEL.map formatExpr intListArgs


foldTests :: GasUnitTests
foldTests = defGasUnitTests allExprs
  where
    foldExpr li =
      [text| (fold (constantly 0) 1 $li) |]
    allExprs = NEL.map foldExpr intListsExpr


filterTests :: GasUnitTests
filterTests = defGasUnitTests allExprs
  where
    filterExpr li =
      [text| (filter (constantly true) $li)
      |]
    allExprs = NEL.map filterExpr intListsExpr


enforceOneTests :: GasUnitTests
enforceOneTests = defGasUnitTests allExprs
  where
    enforceOneExpr tests =
      [text| (enforce-one "some-error-message" $tests) |]

    enforcePass = MockExpr
      [text| (enforce true "this-should-always-succeed") |]

    enforceFail = MockExpr
      [text| (enforce false "skip me") |]

    -- | Lists of failing enforce statements with a passing one at the end
    --   example: [ [ (enforce-statement) (enforce-statement) ] ]
    listOfEnforcesList
      = NEL.map
        (\n -> (replicate (fromIntegral n) enforceFail) <> [enforcePass])
        sizes
    listOfEnforcesListExpr
      = NEL.map (toText . MockList) listOfEnforcesList

    allExprs
      = NEL.map enforceOneExpr listOfEnforcesListExpr


enforcePactVersionTests :: GasUnitTests
enforcePactVersionTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (enforce-pact-version "3.0")|] :| []


-- TODO unable to currently test when enforce's
--      predicate function returns false.
enforceTests :: GasUnitTests
enforceTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (enforce true "some-error-message")|] :| []


dropTests :: GasUnitTests
dropTests = defGasUnitTests allExprs
  where
    dropFirstExpr t =
      [text| (drop 1 $t) |]
    dropLastExpr t =
      [text| (drop -1 $t) |]
    dropKeysExpr (keyList, obj) =
      [text| (drop $keyList $obj) |]
    dropSingleKeyExpr obj =
      [text| (drop ["a1"] $obj) |]
    
    keysToDropArgs = NEL.zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         NEL.map dropFirstExpr intListsExpr
      <> NEL.map dropLastExpr intListsExpr
      <> NEL.map dropKeysExpr keysToDropArgs
      <> NEL.map dropSingleKeyExpr strKeyIntValMapsExpr


namespaceTests :: GasUnitTests
namespaceTests = GasUnitTests tests
  where
    tests = simpleTest :| []
    simpleTest = SomeGasTest setNamespaceTest
      where
        name = "my-namespace"
        expr = [text| (namespace '$name) |]
        
        pubKeys = [PublicKey "something"]
        keyset = KeySet pubKeys (Name "keys-all" def)
        oldNamespace = Namespace (NamespaceName name) (GKeySet keyset)
        updateMsgSig = set eeMsgSigs (S.fromList pubKeys)

        nsRead :: Domain k v -> k -> Method () (Maybe v)
        nsRead Namespaces _ = rc (Just oldNamespace)
        nsRead _ _ = rc Nothing

        mockdb = def { mockRead = MockRead nsRead }
        testEnv = createMockEnv mockdb defEvalState

        setNamespaceTest =
          setEnv updateMsgSig $
            GasTest expr
                    expr
                    testEnv
                    mockEnvCleanup
        

defineNamespaceTests :: GasUnitTests
defineNamespaceTests = GasUnitTests tests
  where
    tests = simpleDefTest :| [rotateNamespaceTest]
    
    simpleDefTest = SomeGasTest $ 
      updateStateWithKeysetLoaded (defGasTest simpleDefExpr)
      where
        simpleDefExpr =
          [text| (define-namespace 'my-namespace some-loaded-keyset) |]
        loadedKeyset = KeySet [PublicKey "something"] (Name "keys-all" def)
        loaded = HM.singleton (Name "some-loaded-keyset" def)
                 (Direct $ TGuard (GKeySet loadedKeyset) def)
        updateStateWithKeysetLoaded = setState $ set (evalRefs . rsLoaded) loaded

    rotateNamespaceTest = SomeGasTest $ rotateTest
      where
        name = "my-namespace"
        rotateExpr = [text| (define-namespace '$name some-loaded-keyset) |]
        loadedKeyset = KeySet [PublicKey "something"] (Name "keys-all" def)
        loaded = HM.singleton (Name "some-loaded-keyset" def)
                 (Direct $ TGuard (GKeySet loadedKeyset) def)
        stateWithKeysetLoaded = set (evalRefs . rsLoaded) loaded defEvalState

        oldPublicKeys = [PublicKey "something"]
        oldKeyset = KeySet oldPublicKeys (Name "keys-all" def) 
        oldNamespace = Namespace (NamespaceName name) (GKeySet oldKeyset)
        updateMsgSig = setEnv $ set eeMsgSigs (S.fromList oldPublicKeys)

        nsRead :: Domain k v -> k -> Method () (Maybe v)
        nsRead Namespaces _ = rc (Just oldNamespace)
        nsRead _ _ = rc Nothing

        mockdb = def {mockRead = MockRead nsRead }
        testEnv = createMockEnv mockdb stateWithKeysetLoaded

        rotateTest = updateMsgSig $
            GasTest rotateExpr
                    (rotateExpr <> ": Defining namespace with the same name as one already defined.")
                    testEnv
                    mockEnvCleanup


containsTests :: GasUnitTests
containsTests = defGasUnitTests allExprs
  where
    containsListExpr (val,li) =
      [text| (contains $val $li) |]

    containsObjExpr obj =
      [text| (contains "a1" $obj) |]

    containsStrExpr (val, str) =
      [text| (contains "a$val" $str) |]

    valuesToSearch = NEL.map (toText . MockInt) sizes
    listArgs = NEL.zip valuesToSearch intListsExpr
    strArgs = NEL.zip valuesToSearch escapedStringsExpr

    allExprs =
      -- | When testing that a list contains a value
         NEL.map containsListExpr listArgs
      -- | When testing that an object has a key entry
      <> NEL.map containsObjExpr strKeyIntValMapsExpr
      -- | When testing that a string contains a value
      <> NEL.map containsStrExpr strArgs


constantlyTests :: GasUnitTests
constantlyTests = defGasUnitTests allExprs
  where
    singleIgnoreExpr = 
      [text| (constantly 0 "firstIgnore") |]
    doubleIgnoreExpr = 
      [text| (constantly 0 "firstIgnore" "secondIgnore") |]
    tripleIgnoreExpr = 
      [text| (constantly 0 "firstIgnore" "secondIgnore" "thirdIgnore") |]
    allExprs =
      singleIgnoreExpr :| [doubleIgnoreExpr, tripleIgnoreExpr]


composeTests :: GasUnitTests
composeTests = defGasUnitTests allExprs
  where
    composeExpr =
      [text| (compose (+ 0) (+ 0) 0) |]
    allExprs = composeExpr :| []


chainDataTests :: GasUnitTests
chainDataTests = defGasUnitTests allExprs
  where
    allExprs = [text| (chain-data) |] :| []


atTests :: GasUnitTests
atTests = defGasUnitTests allExprs
  where
    atListExpr (idx, li) =
      [text| (at $idx $li) |]

    atObjExpr obj =
      [text| (at "a1" $obj) |]

    listIndices = NEL.map (toText . MockInt . pred) sizes
    listArgs = NEL.zip listIndices escapedStrListsExpr

    allExprs = NEL.map atListExpr listArgs
      <> NEL.map atObjExpr strKeyIntValMapsExpr

bindTests :: GasUnitTests
bindTests = defGasUnitTests allExprs
  where
    bindExpr (obj,binding) =
      [text| (bind $obj $binding a1) |]

    args = NEL.zip
           strKeyIntValMapsExpr
           strKeyIntValBindingsExpr

    allExprs = NEL.map bindExpr args

  {--
All native functions
"NativeDefName \"decrypt-cc20p1305\""
"NativeDefName \"validate-keypair\""
"NativeDefName \"verify-spv\""
"NativeDefName \"keyset-ref-guard\""
"NativeDefName \"create-module-guard\""
"NativeDefName \"create-pact-guard\""
"NativeDefName \"create-user-guard\""
"NativeDefName \"compose-capability\""
"NativeDefName \"require-capability\""
"NativeDefName \"enforce-guard\""
"NativeDefName \"with-capability\""
"NativeDefName \"keys-2\""
"NativeDefName \"keys-any\""
"NativeDefName \"keys-all\""
"NativeDefName \"enforce-keyset\""
"NativeDefName \"define-keyset\""
"NativeDefName \"read-keyset\""
"NativeDefName \"shift\""
"NativeDefName \"~\""
"NativeDefName \"xor\""
"NativeDefName \"|\""
"NativeDefName \"&\""
"NativeDefName \"floor\""
"NativeDefName \"ceiling\""
"NativeDefName \"round\""
"NativeDefName \"abs\""
"NativeDefName \"exp\""
"NativeDefName \"ln\""
"NativeDefName \"sqrt\""
"NativeDefName \"mod\""
"NativeDefName \"log\""
"NativeDefName \"^\""
"NativeDefName \"/\""
"NativeDefName \"*\""
"NativeDefName \"-\""
"NativeDefName \"+\""
"NativeDefName \"!=\""
"NativeDefName \"=\""
"NativeDefName \"<=\""
"NativeDefName \">=\""
"NativeDefName \"<\""
"NativeDefName \">\""
"NativeDefName \"not\""
"NativeDefName \"and\""
"NativeDefName \"or\""
"NativeDefName \"not?\""
"NativeDefName \"and?\""
"NativeDefName \"or?\""
"NativeDefName \"format-time\""
"NativeDefName \"days\""
"NativeDefName \"hours\""
"NativeDefName \"minutes\""
"NativeDefName \"diff-time\""
"NativeDefName \"add-time\""
"NativeDefName \"parse-time\""
"NativeDefName \"time\""
"NativeDefName \"describe-module\""
"NativeDefName \"describe-keyset\""
"NativeDefName \"describe-table\""
"NativeDefName \"keylog\""
"NativeDefName \"txlog\""
"NativeDefName \"update\""
"NativeDefName \"insert\""
"NativeDefName \"write\""
"NativeDefName \"txids\""
"NativeDefName \"keys\""
"NativeDefName \"select\""
"NativeDefName \"read\""
"NativeDefName \"with-default-read\""
"NativeDefName \"with-read\""
"NativeDefName \"create-table\""
"NativeDefName \"public-chain-data\""
"NativeDefName \"chain-data\""
"NativeDefName \"namespace\""
"NativeDefName \"define-namespace\""
"NativeDefName \"hash\""
"NativeDefName \"int-to-str\""
"NativeDefName \"str-to-int\""
"NativeDefName \"identity\""
"NativeDefName \"constantly\""
"NativeDefName \"contains\""
"NativeDefName \"enforce-pact-version\""
"NativeDefName \"pact-version\""
"NativeDefName \"resume\""
"NativeDefName \"yield\""
"NativeDefName \"list-modules\""
"NativeDefName \"typeof\""
"NativeDefName \"bind\""
"NativeDefName \"tx-hash\""
"NativeDefName \"read-msg\""
"NativeDefName \"read-integer\""
"NativeDefName \"read-decimal\""
"NativeDefName \"pact-id\""
"NativeDefName \"format\""
"NativeDefName \"enforce-one\""
"NativeDefName \"enforce\""
"NativeDefName \"at\""
"NativeDefName \"remove\""
"NativeDefName \"drop\""
"NativeDefName \"take\""
"NativeDefName \"length\""
"NativeDefName \"compose\""
"NativeDefName \"where\""
"NativeDefName \"sort\""
"NativeDefName \"filter\""
"NativeDefName \"reverse\""
"NativeDefName \"make-list\""
"NativeDefName \"list\""
"NativeDefName \"fold\""
"NativeDefName \"map\""
"NativeDefName \"if\""
--}
