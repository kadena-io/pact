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
import Data.Bool                  (bool)
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
type SQLiteGasTests = NEL.NonEmpty (GasTest SQLiteDb)
type MockGasTests = NEL.NonEmpty (GasTest ())
type PactExpression = T.Text

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


-- | Sample pact code and helper functions/values for testing
acctModuleName :: ModuleName
acctModuleName = ModuleName "accounts" def

acctModuleNameText :: T.Text
acctModuleNameText = asString acctModuleName

accountsModule :: ModuleName -> T.Text
accountsModule moduleName = [text|
     (module $moduleNameText GOV

       (defcap GOV ()
         true)

       (defschema account
         balance:decimal
       )

       (defun test-with-cap-func ()
       @doc "Function to test the `with-capability` function"
         (with-capability (GOV) "")
       )

       (defun enforce-true ()
       @doc "Function to test the `create-user-guard` function"
         (enforce true "")
       )

       (deftable accounts:{account})
       
       ; table for testing `create-table`
       (deftable accounts-for-testing-table-creation:{account})
     ) |]
  where moduleNameText = asString moduleName

acctRow :: ObjectMap PactValue
acctRow = ObjectMap $ M.fromList
          [("balance", PLiteral (LDecimal 100.0))]

sampleLoadedKeysetName :: T.Text
sampleLoadedKeysetName = "some-loaded-keyset"

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


-- the default state caches sample keyset and account module
defEvalState :: IO EvalState
defEvalState = do
  stateWithModule <- getLoadedState (accountsModule acctModuleName)
  let loaded = HM.singleton (Name sampleLoadedKeysetName def)
               (Direct $ TGuard (GKeySet sampleKeyset) def)
  return $ set (evalRefs . rsLoaded) loaded stateWithModule



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

-----------------------
-----------------------
-- | Default backends for gas testing have the following loaded:
--   * Sample accounts module and table
--   * An account "someId" with 0.0 as its balance
--   * Sample keyset and namespace

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
              True (SQLiteConfig sqliteFile []) neverLog
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

-----------------------
-----------------------

mockSetupCleanup :: GasSetup () -> IO ()
mockSetupCleanup (_, _) = return ()

sqliteFile :: String
sqliteFile = "log/bench.sqlite"

sqliteSetupCleanup :: GasSetup SQLiteDb -> IO ()
sqliteSetupCleanup (env, _) = do
  c <- readMVar $ _eePactDbVar env
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

dupe :: a -> (a,a)
dupe a = (a,a)

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
sizes :: NEL.NonEmpty (T.Text, Integer)
sizes =
    ("long", 10000) :|
  [ ("med", 100),
    ("small", 10)
  ]


sizesExpr :: NEL.NonEmpty (T.Text, PactExpression)
sizesExpr = NEL.map format sizes
  where
    format (desc, i) = (desc <> "Number",
                        toText (MockInt i))


-- | List of integers of varying sizes
intLists :: NEL.NonEmpty (T.Text, NEL.NonEmpty Integer)
intLists = NEL.map format sizes
  where
    format (desc, i) = (desc, (1 :| [2..i]))


-- | example: [ "[1 2 3 4]" ]
intListsExpr :: NEL.NonEmpty (T.Text, PactExpression)
intListsExpr = NEL.map format intLists
  where
    format (desc, li) = (desc <> "NumberList",
                         makeExpr li)
    makeExpr li =
      toText $ MockList $ map MockInt (NEL.toList li)


-- | List of strings of varying sizes
strLists :: NEL.NonEmpty (T.Text, NEL.NonEmpty T.Text)
strLists = NEL.map format intLists
  where
    format (desc, li) = (desc,
                         (NEL.map (("a" <>) . intToStr) li))


-- | example: [ "[ \"a1\" \"a2\" \"a3\" \"a4\" ]" ]
escapedStrListsExpr :: NEL.NonEmpty (T.Text, PactExpression)
escapedStrListsExpr = NEL.map format strLists
  where
    format (desc, li) = (desc <> "EscapedStrList",
                         makeExpr li)
    makeExpr li =
      toText $ MockList $ map MockString (NEL.toList li)


-- | Maps of varying sizes. The keys are strings and the values are integers.
strKeyIntValMaps :: NEL.NonEmpty (T.Text, (HM.HashMap T.Text Integer))
strKeyIntValMaps = NEL.map toMap allLists
  where allLists = NEL.zip strLists intLists
        toMap ((kDesc, kList), (_, vList)) = (kDesc, m)
          where
            m = HM.fromList
                $ zip (NEL.toList kList) (NEL.toList vList)


-- | example: "{ \"a5\": 5, \"a3\": 3 }"
strKeyIntValMapsExpr :: NEL.NonEmpty (T.Text, PactExpression)
strKeyIntValMapsExpr = NEL.map format strKeyIntValMaps
  where
    format (desc, m) = (desc <> "OjectMap", toText (MockObject m))


-- | example: "{ \"a5\" := a5, \"a3\" := a3 }"
strKeyIntValBindingsExpr :: NEL.NonEmpty (T.Text, PactExpression)
strKeyIntValBindingsExpr = NEL.map format strKeyIntValMaps
  where
    format (desc, m) = (desc <> "Binding", toText (MockBinding m))


-- | Strings of varying sizes
strings :: NEL.NonEmpty (T.Text, T.Text)
strings = NEL.map format sizes
  where
    toString i = T.replicate (fromIntegral i) "a"
    format (desc, i) = (desc, toString i)
                        

-- | example: "\"aaaaa\""
escapedStringsExpr :: NEL.NonEmpty (T.Text, PactExpression)
escapedStringsExpr = NEL.map format strings
  where
    format (desc, s) = (desc <> "String",
                        toText (MockString s))



-- | Gas benchmark tests for Pact native functions
allNatives :: [NativeDef]
allNatives = concatMap snd natives

untestedNatives :: [NativeDefName]
untestedNatives = foldl' check [] allNatives
  where
    check li (nativeName,_,_) = case (HM.lookup nativeName unitTests) of
      Nothing -> nativeName : li
      Just _ -> li


unitTests :: HM.HashMap NativeDefName GasUnitTests
unitTests = HM.fromList $ foldl' getUnitTest [] allNatives 
  where
    getUnitTest li (nativeName,_,_) =
      case unitTestFromDef nativeName of
        Nothing -> li
        Just ts -> (nativeName, ts) : li
    


unitTestFromDef :: NativeDefName -> Maybe GasUnitTests
unitTestFromDef nativeName = tests
  where
    tests = case (asString nativeName) of
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
      "read-string"          -> Just readStringTests
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

      -- | Database native functions
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

      -- | Capabilities native functions
      "compose-capability"  -> Just composeCapabilityTests
      "create-module-guard" -> Just createModuleGuardTests
      "create-pact-guard"   -> Just createPactGuardTests
      "create-user-guard"   -> Just createUserGuardTests
      "enforce-guard"       -> Just enforceGuardTests
      "keyset-ref-guard"    -> Just keysetRefGuardTests
      "require-capability"  -> Just requireCapabilityTests
      "with-capability"     -> Just withCapabilityTests
      
      _ -> Nothing


-- | Capabilities native function tests
enforceGuardTests :: GasUnitTests
enforceGuardTests = tests
  where
    enforceGuardExpr = [text| (enforce-guard "$sampleLoadedKeysetName") |]
    allExprs = (enforceGuardExpr, enforceGuardExpr) :| []

    signEnvWithKeyset = setEnv (set eeMsgSigs (S.fromList samplePubKeys))

    tests =
      createGasUnitTests
      signEnvWithKeyset
      signEnvWithKeyset
      allExprs


keysetRefGuardTests :: GasUnitTests
keysetRefGuardTests = defGasUnitTests allExprs
  where
    keysetRefGuardExpr = [text| (keyset-ref-guard "$sampleLoadedKeysetName") |]
    allExprs = (keysetRefGuardExpr, keysetRefGuardExpr) :| []


createUserGuardTests :: GasUnitTests
createUserGuardTests = defGasUnitTests allExprs
  where
    createUserGuardExpr = [text| (create-user-guard ($acctModuleNameText.enforce-true)) |]
    allExprs = (createUserGuardExpr, createUserGuardExpr) :| []


createPactGuardTests :: GasUnitTests
createPactGuardTests = tests
  where
    createPactGuardExpr = [text| (create-pact-guard "test") |]
    allExprs = (createPactGuardExpr, createPactGuardExpr) :| []

    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name "some-defpact-func" def) [])
    updateWithPactExec = setState (set evalPactExec mockPactExec)

    tests =
      createGasUnitTests
      updateWithPactExec
      updateWithPactExec
      allExprs


createModuleGuardTests :: GasUnitTests
createModuleGuardTests = tests
  where
    createModuleGuardExpr = [text| (create-module-guard "test") |]
    allExprs = (createModuleGuardExpr, createModuleGuardExpr) :| []
    
    updateStackFrame = setState (set evalCallStack [defStackFrame])

    tests =
      createGasUnitTests
      updateStackFrame
      updateStackFrame
      allExprs


withCapabilityTests :: GasUnitTests
withCapabilityTests = defGasUnitTests allExprs
  where
    withCapExpr = [text| ($acctModuleNameText.test-with-cap-func) |]
    allExprs = (withCapExpr, withCapExpr) :| []
 

requireCapabilityTests :: GasUnitTests
requireCapabilityTests = tests
  where
    requireCapExpr = [text| (require-capability ($acctModuleNameText.GOV)) |]
    allExprs = (requireCapExpr, requireCapExpr) :| []

    cap = UserCapability acctModuleName (DefName "GOV") []
    updateGrantedCap = setState (set (evalCapabilities . capGranted) [cap])

    tests =
      createGasUnitTests
      updateGrantedCap
      updateGrantedCap
      allExprs


composeCapabilityTests :: GasUnitTests
composeCapabilityTests = tests
  where
    composeCapExpr = [text| (compose-capability ($acctModuleNameText.GOV)) |]
    allExprs = (composeCapExpr, composeCapExpr) :| []
      
    capInStackframe =
      StackFrame "" def
      (Just ((FunApp def ""
           (Just defModuleName) Defcap (funTypes $ FunType [] TyAny) Nothing)
        ,[])
      )
    updateStateWithCap = setState (set evalCallStack [capInStackframe])

    tests =
      createGasUnitTests
      updateStateWithCap
      updateStateWithCap
      allExprs


-- | Database native function tests
--   NOTE: Using MockDb means that database insert/write/update always succeed
txlogTests :: GasUnitTests
txlogTests = defGasUnitTests allExprs
  where
    txLogExpr =
      [text| (txlog $acctModuleNameText.accounts 0) |]
    allExprs = (txLogExpr, txLogExpr) :| []

txidsTests :: GasUnitTests
txidsTests = defGasUnitTests allExprs
  where
    txIdsExpr =
      [text| (txids $acctModuleNameText.accounts 0) |]
    allExprs = (txIdsExpr, txIdsExpr) :| []
      

keylogTests :: GasUnitTests
keylogTests = defGasUnitTests allExprs
  where
    keyLogExpr =
      [text| (keylog $acctModuleNameText.accounts "someId" 0) |]
    allExprs = (keyLogExpr, keyLogExpr) :| []
      

keysTests :: GasUnitTests
keysTests = defGasUnitTests allExprs
  where
    keysExprs =
      [text| (keys $acctModuleNameText.accounts) |]
    allExprs = (keysExprs, keysExprs) :| []
      

selectTests :: GasUnitTests
selectTests = defGasUnitTests allExprs
  where
    selectExpr =
      [text| (select $acctModuleNameText.accounts
                     (where "balance" (constantly true))
             ) |]
    allExprs = (selectExpr, selectExpr) :| []
      

withReadTests :: GasUnitTests
withReadTests = defGasUnitTests allExprs
  where
    withReadExpr =
      [text| (with-read
                $acctModuleNameText.accounts
                "someId"
                { "balance":= bal }
                bal
             )
      |]
    allExprs = (withReadExpr, withReadExpr) :| []
 

withDefaultReadTests :: GasUnitTests
withDefaultReadTests = defGasUnitTests allExprs
  where
    withDefReadExpr =
      [text| (with-default-read
                $acctModuleNameText.accounts
                "someId"
                { "balance": 1.0 }
                { "balance":= bal }
                bal
             )
      |]
    allExprs = (withDefReadExpr, withDefReadExpr) :| []


readTests :: GasUnitTests
readTests = defGasUnitTests allExprs
  where
    readExpr =
      [text| (read $acctModuleNameText.accounts "someId") |]
    allExprs = (readExpr, readExpr) :| []
      

writeTests :: GasUnitTests
writeTests = defGasUnitTests allExprs
  where
    writeExpr =
      [text| (write $acctModuleNameText.accounts
                    "some-id-that-is-not-present"
                    { "balance": 0.0 }
             ) |]
    allExprs = (writeExpr, writeExpr) :| []
        

updateTests :: GasUnitTests
updateTests = defGasUnitTests allExprs
  where
    updateExpr =
      [text| (update $acctModuleNameText.accounts
                     "someId"
                     { "balance": 10.0 }
             ) |]
    allExprs = (updateExpr, updateExpr) :| []
      

insertTests :: GasUnitTests
insertTests = defGasUnitTests allExprs
  where
    insertExpr =
      [text| (insert $acctModuleNameText.accounts
                     "some-id-that-is-not-present"
                     { "balance": 0.0 }
             )|]
    allExprs = (insertExpr, insertExpr) :| []


describeTableTests :: GasUnitTests
describeTableTests = defGasUnitTests allExprs
  where
    describeTableExpr =
      [text| (describe-table $acctModuleNameText.accounts) |]
    allExprs = (describeTableExpr, describeTableExpr) :| []
      

describeModuleTests :: GasUnitTests
describeModuleTests = defGasUnitTests allExprs
  where
    describeModuleExpr =
      [text| (describe-module "$acctModuleNameText") |]
    allExprs = (describeModuleExpr, describeModuleExpr) :| []
      

describeKeysetTests :: GasUnitTests
describeKeysetTests = defGasUnitTests allExprs
  where
    describeKeysetExpr =
      [text| (describe-keyset "$sampleLoadedKeysetName") |]
    allExprs = (describeKeysetExpr, describeKeysetExpr) :| []
    

createTableTests :: GasUnitTests
createTableTests = defGasUnitTests allExprs
  where
    createTableExpr =
      [text| (create-table $acctModuleNameText.accounts-for-testing-table-creation) |]
    allExprs = (createTableExpr, createTableExpr) :| []
      

-- | Keyset native function tests
defineKeysetTests :: GasUnitTests
defineKeysetTests = tests
  where
    rotateExpr = [text| (define-keyset "$sampleLoadedKeysetName" $sampleLoadedKeysetName) |]
    allExprs = (rotateExpr, rotateExpr) :| []

    -- Keyset rotation causes previous keyset to be enforced
    updateEnvMsgSig :: GasTest e -> GasTest e
    updateEnvMsgSig = setEnv (set eeMsgSigs (S.fromList samplePubKeys))
        
    updateTestDesc :: GasTest e -> GasTest e
    updateTestDesc =
      set gasTestDescription
      (rotateExpr <> ": rotating keyset")
    
    tests =
      createGasUnitTests
      (updateTestDesc . updateEnvMsgSig)
      (updateTestDesc . updateEnvMsgSig)
      allExprs


enforceKeysetTests :: GasUnitTests
enforceKeysetTests = tests
  where
    enforceKeysetExpr = [text| (enforce-keyset '$sampleLoadedKeysetName) |]
    allExprs = (enforceKeysetExpr, enforceKeysetExpr) :| []

    updateEnvMsgSig = setEnv (set eeMsgSigs (S.fromList samplePubKeys))

    tests =
      createGasUnitTests
      updateEnvMsgSig
      updateEnvMsgSig
      allExprs
    

readKeysetTests :: GasUnitTests
readKeysetTests = tests
  where
    readKeysetExpr = [text| (read-keyset 'my-keyset) |]
    allExprs = (readKeysetExpr, readKeysetExpr) :| []
      
    dataWithKeyset = toPactKeyset "my-keyset" "something" Nothing
    updateMsgBodyWithKeyset = setEnv (set eeMsgBody dataWithKeyset)

    tests =
      createGasUnitTests
      updateMsgBodyWithKeyset
      updateMsgBodyWithKeyset
      allExprs


keysAnyTests :: GasUnitTests
keysAnyTests = defGasUnitTests allExprs
  where
    keysAnyExpr = [text|(keys-any 10 1)|]
    allExprs = (keysAnyExpr, keysAnyExpr) :| []
                  

keysAllTests :: GasUnitTests
keysAllTests = defGasUnitTests allExprs
  where
    keysAllExpr = [text|(keys-all 3 3)|]
    allExprs = (keysAllExpr, keysAllExpr) :| []
                  

keys2Tests :: GasUnitTests
keys2Tests = defGasUnitTests allExprs
  where
    keys2Expr = [text|(keys-2 3 1)|]
    allExprs = (keys2Expr, keys2Expr) :| []


-- | Commitments native function tests
decryptCc20p1305Tests :: GasUnitTests
decryptCc20p1305Tests = defGasUnitTests allExprs
  where
    decryptExpr =
      [text| (decrypt-cc20p1305
              "Zi1REj5-iA"
              "AAAAAAECAwQFBgcI"
              "YWFk"
              "FYP6lG7xq7aExvoaHIH8Jg"
              "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
              "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
      |]
    allExprs = (decryptExpr, decryptExpr) :| []


validateKeypairTests :: GasUnitTests
validateKeypairTests = defGasUnitTests allExprs
  where
    validateExpr =
      [text| (validate-keypair
             "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
             "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a") |]
    allExprs = (validateExpr, validateExpr) :| []


-- | Time native function tests
addTimeTests :: GasUnitTests
addTimeTests = defGasUnitTests allExprs
  where
    addTimeExpr =
      [text| (add-time (time "2016-07-22T12:00:00Z") 15) |]
    allExprs = (addTimeExpr, addTimeExpr) :| []
      

daysTests :: GasUnitTests
daysTests = defGasUnitTests allExprs
  where
    daysExpr n =
      [text| (days $n) |]
    allExprs = NEL.map (\(_,i) -> (daysExpr i, daysExpr i)) sizesExpr


diffTimeTests :: GasUnitTests
diffTimeTests = defGasUnitTests allExprs
  where
    diffTime =
      [text| (diff-time (time "2016-07-22T12:00:00Z")
                        (time "2018-07-22T12:00:00Z"))
      |]
    allExprs = (diffTime, diffTime) :| []
        

formatTimeTests :: GasUnitTests
formatTimeTests = defGasUnitTests allExprs
  where
    formatTimeSimpleExpr = [text| (format-time "%F" (time "2016-07-22T12:00:00Z")) |]
    formatTimeComplexExpr =
      [text| (format-time "%Y-%m-%dT%H:%M:%S%N" (time "2016-07-23T13:30:45Z")) |]

    allExprs = (formatTimeSimpleExpr, formatTimeSimpleExpr) :|
               [(formatTimeComplexExpr, formatTimeComplexExpr)]


hoursTests :: GasUnitTests
hoursTests = defGasUnitTests allExprs
  where
    hoursExpr n =
      [text| (hours $n) |]
    allExprs = NEL.map (over both hoursExpr) sizesExpr


minutesTests :: GasUnitTests
minutesTests = defGasUnitTests allExprs
  where
    minutesExpr n =
      [text| (minutes $n) |]
    allExprs = NEL.map (over both minutesExpr) sizesExpr


parseTimeTests :: GasUnitTests
parseTimeTests = defGasUnitTests allExprs
  where
    parseTimeSimpleExpr =
      [text| (parse-time "%F" "2016-07-22") |]
    parseTimeComplexExpr =
      [text| (parse-time "%Y-%m-%dT%H:%M:%S%N" "2016-07-23T13:30:45+00:00") |]
    allExprs = (parseTimeSimpleExpr, parseTimeSimpleExpr) :|
               [(parseTimeComplexExpr, parseTimeComplexExpr)]


timeTests :: GasUnitTests
timeTests = defGasUnitTests allExprs
  where
    timeExpr =
      [text| (time "2016-07-22T12:00:00Z") |]
    allExprs = (timeExpr, timeExpr) :| []


-- | Operators native function tests
reverseBitsOptTests :: GasUnitTests
reverseBitsOptTests = defGasUnitTests allExprs
  where
    reverseBitsExpr x = [text| (~ $x) |]

    allExprs = NEL.map (over both reverseBitsExpr) sizesExpr


bitwiseOrOptTests :: GasUnitTests
bitwiseOrOptTests = defGasUnitTests allExprs
  where
    bitwiseOrExpr x = [text| (| 2 $x) |]

    allExprs = NEL.map (over both bitwiseOrExpr) sizesExpr


xorOptTests :: GasUnitTests
xorOptTests = defGasUnitTests allExprs
  where
    xorExpr x = [text| (xor 2 $x) |]

    allExprs = NEL.map (over both xorExpr) sizesExpr


sqrtOptTests :: GasUnitTests
sqrtOptTests = defGasUnitTests allExprs
  where
    sqrtExpr x = [text| (sqrt $x) |]
    sqrtDecimalExpr x = [text| (sqrt $x.1) |]

    allExprs = NEL.map (over both sqrtExpr) sizesExpr
      <> NEL.map (over both sqrtDecimalExpr) sizesExpr


shiftOptTests :: GasUnitTests
shiftOptTests = defGasUnitTests allExprs
  where
    shiftExpr x =
      [text| (shift 2 $x) |]
    shiftNegExpr x =
      [text| (shift -2 $x) |]

    allExprs = NEL.map (over both shiftExpr) sizesExpr
      <> NEL.map (over both shiftNegExpr) sizesExpr


roundOptTests :: GasUnitTests
roundOptTests = defGasUnitTests allExprs
  where
    roundExpr x =
      [text| (round $x.12345) |]
    roundPrecExpr x =
      [text| (round $x.12345 4) |]

    allExprs = NEL.map (over both roundExpr) sizesExpr
      <> NEL.map (over both roundPrecExpr) sizesExpr


orFuncOptTests :: GasUnitTests
orFuncOptTests = defGasUnitTests allExprs
  where
    orFuncExpr = [text| (or? (identity) (identity) true) |]

    allExprs = (orFuncExpr, orFuncExpr) :| []


orOptTests :: GasUnitTests
orOptTests = defGasUnitTests allExprs
  where
    orExpr = [text| (or false false) |]

    allExprs = (orExpr, orExpr) :| []


notFuncOptTests :: GasUnitTests
notFuncOptTests = defGasUnitTests allExprs
  where
    notFuncExpr = [text| (not? (identity) true) |]

    allExprs = (notFuncExpr, notFuncExpr) :| []


notOptTests :: GasUnitTests
notOptTests = defGasUnitTests allExprs
  where
    notExpr = [text| (not true) |]

    allExprs = (notExpr, notExpr) :| []


modOptTests :: GasUnitTests
modOptTests = defGasUnitTests allExprs
  where
    modExpr x =
      [text| (mod $x 2) |]

    allExprs = NEL.map (over both modExpr) sizesExpr



logOptTests :: GasUnitTests
logOptTests = defGasUnitTests allExprs
  where
    logExpr y =
      [text| (log 2 $y) |]
    logDecimalExpr y =
      [text| (log 2 $y.1) |]

    allExprs = NEL.map (over both logExpr) sizesExpr
      <> NEL.map (over both logDecimalExpr) sizesExpr



lnOptTests :: GasUnitTests
lnOptTests = defGasUnitTests allExprs
  where
    lnExpr x =
      [text| (ln $x) |]
    lnDecimalExpr x =
      [text| (ln $x.1) |]

    allExprs = NEL.map (over both lnExpr) sizesExpr
      <> NEL.map (over both lnDecimalExpr) sizesExpr


floorOptTests :: GasUnitTests
floorOptTests = defGasUnitTests allExprs
  where
    floorExpr x =
      [text| (floor $x.12345) |]
    floorPrecExpr x =
      [text| (floor $x.12345 4) |]

    allExprs = NEL.map (over both floorExpr) sizesExpr
      <> NEL.map (over both floorPrecExpr) sizesExpr


expOptTests :: GasUnitTests
expOptTests = defGasUnitTests allExprs
  where
    expExprSmall =
      [text| (exp 1) |]
    expExprMed =
      [text| (exp 10) |]
    expExprLarge =
      [text| (exp 100) |]

    allExprs = (expExprSmall, expExprSmall) :|
               [(expExprMed, expExprMed)
               ,(expExprLarge, expExprLarge)]


ceilingOptTests :: GasUnitTests
ceilingOptTests = defGasUnitTests allExprs
  where
    ceilingExpr x =
      [text| (ceiling $x.12345) |]
    ceilingPrecExpr x =
      [text| (ceiling $x.12345 4) |]

    allExprs = NEL.map (over both ceilingExpr) sizesExpr
      <> NEL.map (over both ceilingPrecExpr) sizesExpr
        

andFuncOptTests :: GasUnitTests
andFuncOptTests = defGasUnitTests allExprs
  where
    andFuncExpr =
      [text| (and? (identity) (identity) true) |]

    allExprs = (andFuncExpr, andFuncExpr) :| []


andOptTests :: GasUnitTests
andOptTests = defGasUnitTests allExprs
  where
    andExpr =
      [text| (and false true) |]

    allExprs = (andExpr, andExpr) :| []


absOptTests :: GasUnitTests
absOptTests = defGasUnitTests allExprs
  where
    absExpr x =
      [text| (abs -$x) |]
    absDecimalExpr x =
      [text| (abs -$x.0) |]

    allExprs = NEL.map (over both absExpr) sizesExpr
      <> NEL.map (over both absDecimalExpr) sizesExpr

        
raiseOptTests :: GasUnitTests
raiseOptTests = defGasUnitTests allExprs
  where
    raiseExpr y = 
      [text| (^ 2 $y) |]
    raiseDecimalExpr y =
      [text| (^ 2.1 $y.1) |]
    raiseBothExpr y =
      [text| (^ 2.1 $y) |]

    allExprs = NEL.map (over both raiseExpr) sizesExpr
      <> NEL.map (over both raiseDecimalExpr) sizesExpr
      <> NEL.map (over both raiseBothExpr) sizesExpr


greaterThanEqOptTests :: GasUnitTests
greaterThanEqOptTests = defGasUnitTests allExprs
  where
    greaterEqExpr x =
      [text| (>= $x $x) |]
    greaterEqDecimalExpr x =
      [text| (>= $x.0 $x.0) |]
    greaterEqTimeExpr =
      [text| (>= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
   
    allExprs = NEL.map (over both greaterEqExpr) sizesExpr
      <> NEL.map (over both greaterEqDecimalExpr) sizesExpr
      <> NEL.map (over both greaterEqExpr) escapedStringsExpr
      <> ((greaterEqTimeExpr, greaterEqTimeExpr) :| [])


greaterThanOptTests :: GasUnitTests
greaterThanOptTests = defGasUnitTests allExprs
  where
    greaterExpr x =
      [text| (> $x $x) |]
    greaterDecimalExpr x =
      [text| (> $x.0 $x.0) |]
    greaterTimeExpr =
      [text| (> (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
   
    allExprs = NEL.map (over both greaterExpr) sizesExpr
      <> NEL.map (over both greaterDecimalExpr) sizesExpr
      <> NEL.map (over both greaterExpr) escapedStringsExpr
      <> ((greaterTimeExpr, greaterTimeExpr) :| [])
    

equalOptTests :: GasUnitTests
equalOptTests = defGasUnitTests allExprs
  where
    eqExpr x =
      [text| (= $x $x) |]
    eqDecimalExpr x =
      [text| (= $x.0 $x.0) |]
    eqTimeExpr =
      [text| (= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = NEL.map (over both eqExpr) sizesExpr
      <> NEL.map (over both eqDecimalExpr) sizesExpr
      <> NEL.map (over both eqExpr) escapedStringsExpr
      <> NEL.map (over both eqExpr) strKeyIntValMapsExpr
      <> NEL.map (over both eqExpr) intListsExpr
      <> ((eqTimeExpr, eqTimeExpr) :| [])


lessThanEqualOptTests :: GasUnitTests
lessThanEqualOptTests = defGasUnitTests allExprs
  where
    lessEqExpr x =
      [text| (<= $x $x) |]
    lessEqDecimalExpr x =
      [text| (<= $x.0 $x.0) |]
    lessEqTimeExpr =
      [text| (<= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
   
    allExprs = NEL.map (over both lessEqExpr) sizesExpr
      <> NEL.map (over both lessEqDecimalExpr) sizesExpr
      <> NEL.map (over both lessEqExpr) escapedStringsExpr
      <> ((lessEqTimeExpr, lessEqTimeExpr) :| [])


lessThanOptTests :: GasUnitTests
lessThanOptTests = defGasUnitTests allExprs
  where
    lessExpr x =
      [text| (< $x $x) |]
    lessDecimalExpr x =
      [text| (< $x.0 $x.0) |]
    lessTimeExpr =
      [text| (< (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
   
    allExprs = NEL.map (over both lessExpr) sizesExpr
      <> NEL.map (over both lessDecimalExpr) sizesExpr
      <> NEL.map (over both lessExpr) escapedStringsExpr
      <> ((lessTimeExpr, lessTimeExpr) :| [])


divOptTests :: GasUnitTests
divOptTests = defGasUnitTests allExprs
  where
    divExpr x =
      [text| (/ $x $x) |]
    divDecimalExpr x =
      [text| (/ $x.0 $x.0) |]
    divBothExpr x =
      [text| (/ $x.0 $x) |]
   
    allExprs = NEL.map (over both divExpr) sizesExpr
      <> NEL.map (over both divDecimalExpr) sizesExpr
      <> NEL.map (over both divBothExpr) sizesExpr


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
   
    allExprs = NEL.map (over both subExpr) sizesExpr
      <> NEL.map (over both subDecimalExpr) sizesExpr
      <> NEL.map (over both subBothExpr) sizesExpr
      <> NEL.map (over both subOneExpr) sizesExpr
      <> NEL.map (over both subOneDecimalExpr) sizesExpr


addOptTests :: GasUnitTests
addOptTests = defGasUnitTests allExprs
  where
    addExpr x =
      [text| (+ $x $x) |]
    addDecimalExpr x =
      [text| (+ $x.0 $x.0) |]
    addBothExpr x =
      [text| (+ $x.0 $x) |]

    allExprs = NEL.map (over both addExpr) sizesExpr
      <> NEL.map (over both addDecimalExpr) sizesExpr
      <> NEL.map (over both addBothExpr) sizesExpr
      <> NEL.map (over both addExpr) escapedStringsExpr
      <> NEL.map (over both addExpr) strKeyIntValMapsExpr


multOptTests :: GasUnitTests
multOptTests = defGasUnitTests allExprs
  where
    multIntExpr x =
      [text| (* $x $x) |]
    multDecimalExpr x =
      [text| (* $x.0 $x.0) |]
    multBothExpr x =
      [text| (* $x.0 $x) |]

    allExprs = NEL.map (over both multIntExpr) sizesExpr
      <> NEL.map (over both multDecimalExpr) sizesExpr
      <> NEL.map (over both multBothExpr) sizesExpr


bitwiseOptTests :: GasUnitTests
bitwiseOptTests = defGasUnitTests allExprs
  where
    bitwiseExpr x =
      [text| (& $x $x) |]

    allExprs = NEL.map (over both bitwiseExpr) sizesExpr


notEqualOptTests :: GasUnitTests
notEqualOptTests = defGasUnitTests allExprs
  where
    notEqualExpr x =
      [text| (!= $x $x) |]
    notEqualDecimalExpr x =
      [text| (!= $x.0 $x.0) |]
        
    allExprs = NEL.map (over both notEqualExpr) sizesExpr
      <> NEL.map (over both notEqualExpr) escapedStringsExpr
      <> NEL.map (over both notEqualDecimalExpr) sizesExpr
      <> NEL.map (over both notEqualExpr) intListsExpr
      <> NEL.map (over both notEqualExpr) strKeyIntValMapsExpr



-- | General native function tests
whereTests :: GasUnitTests
whereTests = defGasUnitTests allExprs
  where
    whereExpr obj =
      [text| (where "a1" (constantly true) $obj) |]

    allExprs = NEL.map (over both whereExpr) strKeyIntValMapsExpr


typeOfTests :: GasUnitTests
typeOfTests = defGasUnitTests allExprs
  where
    typeOfExpr t =
      [text| (typeof $t) |]

    allExprs = NEL.map (over both typeOfExpr) strKeyIntValMapsExpr
      <> NEL.map (over both typeOfExpr) escapedStringsExpr
      <> NEL.map (over both typeOfExpr) intListsExpr
      <> NEL.map (over both typeOfExpr) sizesExpr


txHashTests :: GasUnitTests
txHashTests = defGasUnitTests allExprs
  where
    txHashExpr = [text| (tx-hash) |]
    allExprs = (txHashExpr, txHashExpr) :| []


tryTests :: GasUnitTests
tryTests = defGasUnitTests allExprs
  where
    tryPassExpr =
      [text| (try true (enforce true "this will definitely pass")) |]
    tryFailExpr =
      [text| (try true (enforce false "this will definitely fail")) |]

    allExprs = (tryPassExpr, tryPassExpr) :| [ (tryFailExpr, tryFailExpr) ]


takeTests :: GasUnitTests
takeTests = defGasUnitTests allExprs
  where
    takeFirstExpr t =
      [text| (take 1 $t) |]
    takeLastExpr t =
      [text| (take -1 $t) |]
    takeKeysExpr ((keyListDesc, keyList), (objDesc, obj)) =
      ([text| (take $keyListDesc $objDesc) |]
      ,[text| (take $keyList $obj) |])
    takeSingleKeyExpr obj =
      [text| (take ["a1"] $obj) |]
    
    keysToTakeArgs = NEL.zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         NEL.map (over both takeFirstExpr) intListsExpr
      <> NEL.map (over both takeLastExpr) intListsExpr
      <> NEL.map takeKeysExpr keysToTakeArgs
      <> NEL.map (over both takeSingleKeyExpr) strKeyIntValMapsExpr


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
    
    allExprs = NEL.map (dupe . str2intExpr . snd) sizes
      <> ( (dupe str2intLongHex) :|
          [ dupe str2intMedHex,
            dupe str2intSmallHex,
            
            dupe str2intLongBinary,
            dupe str2intMedBinary,
            dupe str2intSmallBinary,
            
            dupe str2intLongBase64,
            dupe str2intMedBase64,
            dupe str2intSmallBase64
          ])


sortTests :: GasUnitTests
sortTests = defGasUnitTests allExprs
  where
    sortListExpr li =
      [text| (sort $li) |]

    reversedListsExpr = NEL.map format intLists
      where
        format (desc, li) = (desc <> "NumberList", reversedListExpr li)
        reversedListExpr li =
          toText $ MockList $ map MockInt (reverse $ NEL.toList li)
    
    allExprs = NEL.map (over both sortListExpr) reversedListsExpr


reverseTests :: GasUnitTests
reverseTests = defGasUnitTests allExprs
  where
    reverseExpr li =
      [text| (reverse $li) |]

    allExprs = NEL.map (over both reverseExpr) intListsExpr


removeTests :: GasUnitTests
removeTests = defGasUnitTests allExprs
  where
    removeExpr obj =
      [text| (remove "a1" $obj) |]

    allExprs = NEL.map (over both removeExpr) strKeyIntValMapsExpr


pactIdTests :: GasUnitTests
pactIdTests = tests
  where
    pactIdExpr = [text|(pact-id)|]
    
    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name "some-defpact-func" def) [])
    updateState = setState (set evalPactExec mockPactExec)
    
    tests =
      createGasUnitTests updateState updateState (dupe pactIdExpr :| [])


yieldTests :: GasUnitTests
yieldTests = tests
  where
    yieldExpr obj = [text| (yield $obj) |]
    yieldExprWithTargetChain obj = [text| (yield $obj "some-chain-id") |]

    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name "some-defpact-func" def) [])

    mockModules = HM.fromList [(defModuleName, defModuleData)]
    mockStackframe = [defStackFrame]
    updateStateWithStackFrame = setState (set evalCallStack mockStackframe)
    updateStateWithPactExec = setState (set evalPactExec mockPactExec)
    setInitialState = setState $ const (initStateModules mockModules)


    allUpdatesForNoChain =
      updateStateWithPactExec
    allExprsNoChain =
      NEL.map (over both yieldExpr) strKeyIntValMapsExpr
    testsWithNoChain =
      createGasUnitTests
      allUpdatesForNoChain
      allUpdatesForNoChain
      allExprsNoChain

  
    allUpdatesForChain =
      updateStateWithStackFrame .
      updateStateWithPactExec .
      setInitialState
    allExprsWithChain =
      NEL.map (over both yieldExprWithTargetChain) strKeyIntValMapsExpr
    testsWithChain =
      createGasUnitTests
      allUpdatesForChain
      allUpdatesForChain
      allExprsWithChain


    tests = testsWithNoChain <> testsWithChain


resumeTests :: GasUnitTests
resumeTests = tests
  where
    resumeExpr binding = [text|(resume $binding a1)|]

    args :: NEL.NonEmpty (HM.HashMap T.Text Integer, (T.Text, PactExpression))
    args = NEL.map (\((_,m),b) -> (m,over both resumeExpr b))
           $ NEL.zip strKeyIntValMaps strKeyIntValBindingsExpr

    toSPVTests ::
      (HM.HashMap T.Text Integer, (T.Text, PactExpression))
      -> GasUnitTests
    toSPVTests (yieldMap, (desc, expr))
      = createGasUnitTests
        (setupForResume True (yieldMap, desc))
        (setupForResume True (yieldMap, desc))
        ((desc,expr) :| [])

    toNonSPVTests ::
      (HM.HashMap T.Text Integer, (T.Text, PactExpression))
      -> GasUnitTests
    toNonSPVTests (yieldMap, (desc, expr))
      = createGasUnitTests
        (setupForResume False (yieldMap, desc))
        (setupForResume False (yieldMap, desc))
        ((desc,expr) :| [])

    tests = concatGasUnitTests $
            NEL.map toSPVTests args <>
            NEL.map toNonSPVTests args

    setupForResume
      :: Bool
      -> (HM.HashMap T.Text Integer, T.Text)
      -> (GasTest e -> GasTest e)
    setupForResume isProv (yielded, desc) = allUpdates
      where
        allUpdates
          = bool
            ( updateEnvWithYield )    -- No provenance needed
            ( updateGasTestDesc .
              updateStateWithStackFrame .
              updateEnvWithChaindId .
              updateEnvWithYield .
              setInitialState
            )
            isProv
        
        updateEnvWithYield =
          setEnv (set eePactStep pactStep)
        updateEnvWithChaindId
          = setEnv (set (eePublicData . pdPublicMeta . pmChainId) chainId)
        updateStateWithStackFrame
          = setState (set evalCallStack [defStackFrame])
        updateGasTestDesc
          = set gasTestDescription $ desc <> " with provenance"
        setInitialState = setState $ const (initStateModules mockModules)

        mockModules
          = HM.fromList [(defModuleName, defModuleData)]
        pactStep
          = Just $ PactStep 2 False (PactId "") (Just yieldVal)
        yieldVal
          = Yield yieldData provenance
        provenance
          = bool Nothing (Just $ Provenance chainId defModuleHash) isProv
        chainId
          = ChainId "some-chain-id"
        yieldData
          = (ObjectMap . M.fromList . toPactValueInt . HM.toList) yielded
        toPactValueInt
          = map (\(t,v) -> (FieldKey t, PLiteral $ LInteger v))


pactVersionTests :: GasUnitTests
pactVersionTests = defGasUnitTests allExprs
  where
    versionExpr =
      [text| (pact-version) |]

    allExprs = (versionExpr, versionExpr) :| []


readStringTests :: GasUnitTests
readStringTests = tests
  where
    readStringExpr = [text|(read-string "name")|]

    allUpdates i = updateEnv
      where
        strVal
          = A.object ["name" A..= i]
        updateEnv
          = setEnv (set eeMsgBody strVal)

    setupTests (desc, s)
      = createGasUnitTests
        (allUpdates s)
        (allUpdates s)
        ( (readStringExpr <> " with " <> desc,
           readStringExpr) :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests escapedStringsExpr


readMsgTests :: GasUnitTests
readMsgTests = tests
  where
    readMsgExpr = [text|(read-msg)|]
       
    allUpdates m = setEnv (set eeMsgBody $ toJSON m)
    
    setupTests (desc, m)
      = createGasUnitTests
        (allUpdates m)
        (allUpdates m)
        ((readMsgExpr <> " with " <> desc,
          readMsgExpr) :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests strKeyIntValMapsExpr


readIntegerTests :: GasUnitTests
readIntegerTests = tests
  where
    readIntExpr = [text|(read-integer "amount")|]
    
    allUpdates i = updateEnv
      where
        intVal
          = A.object ["amount" A..= i]
        updateEnv
          = setEnv (set eeMsgBody intVal)

    setupTests (desc, i)
      = createGasUnitTests
        (allUpdates i)
        (allUpdates i)
        ((readIntExpr <> " with " <> desc,
          readIntExpr) :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests sizesExpr


readDecimalTests :: GasUnitTests
readDecimalTests = tests
  where
    readDecExpr = [text|(read-decimal "amount")|]
    
    allUpdates d = updateEnv
      where
        d' = d <> "1"
        decVal = A.object ["amount" A..= [text|0.$d'|]]
        updateEnv
          = setEnv (set eeMsgBody decVal)

    setupTests (desc, d)
      = createGasUnitTests
        (allUpdates d)
        (allUpdates d)
        ((readDecExpr <> " with " <> desc,
          readDecExpr) :| [])
   
    tests = concatGasUnitTests $
            NEL.map setupTests sizesExpr


mapTests :: GasUnitTests
mapTests = defGasUnitTests allExprs
  where
    mapExpr li =
      [text| (map (identity) $li) |]

    allExprs = NEL.map (over both mapExpr) intListsExpr


makeListTests :: GasUnitTests
makeListTests = defGasUnitTests allExprs
  where
    makeListExpr len =
      [text| (make-list $len true) |]

    allExprs = NEL.map (over both makeListExpr) sizesExpr


listModulesTests :: GasUnitTests
listModulesTests = defGasUnitTests allExprs
  where
    listModulesExpr =
      [text| (list-modules) |]

    allExprs = dupe listModulesExpr :| []


lengthTests :: GasUnitTests
lengthTests = defGasUnitTests allExprs
  where
    lengthExpr t =
      [text| (length $t) |]

    allExprs =
         NEL.map (over both lengthExpr) intListsExpr
      <> NEL.map (over both lengthExpr) escapedStringsExpr
      <> NEL.map (over both lengthExpr) strKeyIntValMapsExpr


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
    args = F.foldr1 (<>) $ NEL.map (\(_, n) -> NEL.map (\b -> (n,b)) baseList) sizes
    
    allExprs = NEL.map (dupe . int2strExpr) args


ifTests :: GasUnitTests
ifTests = defGasUnitTests allExprs
  where
    ifExpr =
      [text| (if true "then-clause" "else-clause") |]

    allExprs = (ifExpr, ifExpr) :| []


identityTests :: GasUnitTests
identityTests = defGasUnitTests allExprs
  where
    identityExpr val =
      [text| (identity $val) |]

    allExprs = NEL.map (over both identityExpr) intListsExpr


hashTests :: GasUnitTests
hashTests = defGasUnitTests allExprs
  where
    hashExpr val =
      [text| (hash $val) |]

    allExprs =
         NEL.map (over both hashExpr) escapedStringsExpr
      <> NEL.map (over both hashExpr) strKeyIntValMapsExpr

formatTests :: GasUnitTests
formatTests = defGasUnitTests allExprs
  where
    formatExpr (str,(desc,li)) =
      ([text| (format "{}...{}" $desc)|]
      ,[text| (format "$str" $li )|])

    curlyBraces =
      NEL.map
      (\(_,n) -> T.unwords $ replicate (fromIntegral n) "{}")
      sizes
    strListArgs = NEL.zip curlyBraces escapedStrListsExpr
    intListArgs = NEL.zip curlyBraces intListsExpr

    allExprs =
         NEL.map formatExpr strListArgs
      <> NEL.map formatExpr intListArgs


foldTests :: GasUnitTests
foldTests = defGasUnitTests allExprs
  where
    foldExpr li =
      [text| (fold (constantly 0) 1 $li) |]
    allExprs = NEL.map (over both foldExpr) intListsExpr


filterTests :: GasUnitTests
filterTests = defGasUnitTests allExprs
  where
    filterExpr li =
      [text| (filter (constantly true) $li)
      |]
    allExprs = NEL.map (over both filterExpr) intListsExpr


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
        (\(desc,n) -> (desc, (replicate (fromIntegral n) enforceFail) <> [enforcePass]))
        sizes
    listOfEnforcesListExpr
      = NEL.map (\(desc, li) -> (desc <> "EnforceList", toText (MockList li))) listOfEnforcesList

    allExprs
      = NEL.map (over both enforceOneExpr) listOfEnforcesListExpr


enforcePactVersionTests :: GasUnitTests
enforcePactVersionTests = defGasUnitTests allExprs
  where
    enforcePactVersionExpr =
      [text| (enforce-pact-version "3.0")|]
    allExprs = (enforcePactVersionExpr, enforcePactVersionExpr) :| []


-- TODO: Unable to currently test when enforce's
--       predicate function returns false.
enforceTests :: GasUnitTests
enforceTests = defGasUnitTests allExprs
  where
    allExprs = dupe
      [text| (enforce true "some-error-message")|] :| []
 

dropTests :: GasUnitTests
dropTests = defGasUnitTests allExprs
  where
    dropFirstExpr t =
      [text| (drop 1 $t) |]
    dropLastExpr t =
      [text| (drop -1 $t) |]
    dropKeysExpr ((keyListDesc, keyList), (objDesc, obj)) =
      ([text| (drop $keyListDesc $objDesc) |]
      ,[text| (drop $keyList $obj) |])
    dropSingleKeyExpr obj =
      [text| (drop ["a1"] $obj) |]
    
    keysToDropArgs = NEL.zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         NEL.map (over both dropFirstExpr) intListsExpr
      <> NEL.map (over both dropLastExpr) intListsExpr
      <> NEL.map dropKeysExpr keysToDropArgs
      <> NEL.map (over both dropSingleKeyExpr) strKeyIntValMapsExpr


namespaceTests :: GasUnitTests
namespaceTests = tests
  where
    namespaceExpr = [text| (namespace '$sampleNamespaceName) |]

    updateEnvWithSig =
      setEnv $ set eeMsgSigs (S.fromList samplePubKeys)

    tests = createGasUnitTests
            updateEnvWithSig
            updateEnvWithSig
            (dupe namespaceExpr :| [])


defineNamespaceTests :: GasUnitTests
defineNamespaceTests = tests
  where
    tests = simpleDefTests <> rotateNamespaceTests
    
    simpleDefTests = defGasUnitTests (dupe simpleDefExpr :| [])
      where
        simpleDefExpr =
          [text| (define-namespace 'some-other-namespace $sampleLoadedKeysetName) |]
 
    rotateNamespaceTests = rotateTests
      where
        rotateExpr = [text| (define-namespace '$sampleNamespaceName $sampleLoadedKeysetName) |]

        updateMsgSig =
          setEnv $ set eeMsgSigs (S.fromList samplePubKeys)
        updateDesc =
          set gasTestDescription $
          rotateExpr <> ": Defining namespace with the same name as one already defined."

        rotateTests =
          createGasUnitTests
          (updateMsgSig . updateDesc)
          (updateMsgSig . updateDesc)
          (dupe rotateExpr :| [])


containsTests :: GasUnitTests
containsTests = defGasUnitTests allExprs
  where
    containsListExpr ((valDesc, val), (liDesc, li)) =
      ([text| (contains $valDesc $liDesc) |]
      ,[text| (contains $val $li) |])

    containsObjExpr obj =
      [text| (contains "a1" $obj) |]

    containsStrExpr ((valDesc,val), (strDesc,str)) =
      ([text| (contains "a$valDesc" $strDesc) |]
      ,[text| (contains "a$val" $str) |])

    listArgs = NEL.zip sizesExpr intListsExpr
    strArgs = NEL.zip sizesExpr escapedStringsExpr

    allExprs =
      -- | When testing that a list contains a value
         NEL.map containsListExpr listArgs
      -- | When testing that an object has a key entry
      <> NEL.map (over both containsObjExpr) strKeyIntValMapsExpr
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
      dupe singleIgnoreExpr :| [dupe doubleIgnoreExpr, dupe tripleIgnoreExpr]


composeTests :: GasUnitTests
composeTests = defGasUnitTests allExprs
  where
    composeExpr =
      [text| (compose (+ 0) (+ 0) 0) |]
    allExprs = dupe composeExpr :| []


chainDataTests :: GasUnitTests
chainDataTests = defGasUnitTests allExprs
  where
    allExprs = dupe [text| (chain-data) |] :| []


atTests :: GasUnitTests
atTests = defGasUnitTests allExprs
  where
    atListExpr ((_,idx), (liDesc,li)) =
      ([text| (at $idx $liDesc) |]
      ,[text| (at $idx $li) |])

    atObjExpr obj =
      [text| (at "a1" $obj) |]

    listIndices = NEL.map (\(desc,i) -> (desc, toText $ MockInt $ pred i)) sizes
    listArgs = NEL.zip listIndices escapedStrListsExpr

    allExprs = NEL.map atListExpr listArgs
      <> NEL.map (over both atObjExpr) strKeyIntValMapsExpr

bindTests :: GasUnitTests
bindTests = defGasUnitTests allExprs
  where
    bindExpr ((objDesc, obj), (bindingDesc, binding)) =
      ([text| (bind $objDesc $bindingDesc a1) |]
      ,[text| (bind $obj $binding a1) |])

    args = NEL.zip
           strKeyIntValMapsExpr
           strKeyIntValBindingsExpr

    allExprs = NEL.map bindExpr args
