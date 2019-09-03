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

data GasTest e = GasTest
  { _gasTestExpression :: !T.Text
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


defMockGasTest :: T.Text -> GasTest ()
defMockGasTest expr =
  GasTest
  expr
  expr
  (createSetup defMockBackend defEvalState)
  mockSetupCleanup

defSqliteGasTest :: T.Text -> GasTest SQLiteDb
defSqliteGasTest expr =
  GasTest
  expr
  expr
  (createSetup defSqliteBackend defEvalState)
  sqliteSetupCleanup

createSetup :: IO (EvalEnv e) -> IO EvalState -> IO (GasSetup e)
createSetup env state = do
  e <- env
  s <- state
  return (e,s)


defGasUnitTests
  :: NEL.NonEmpty T.Text
  -> GasUnitTests
defGasUnitTests pactExprs = GasUnitTests sqliteTests mockTests
  where
    mockTests = NEL.map defMockGasTest pactExprs
    sqliteTests = NEL.map defSqliteGasTest pactExprs


createGasUnitTests
  :: (GasTest SQLiteDb -> GasTest SQLiteDb)
  -> (GasTest () -> GasTest ())
  -> NEL.NonEmpty T.Text
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
sizes =
    100000 :|
  [ 100,
    10
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
    enforceGuardExpr = [text| (enforce-guard "$sampleLoadedKeysetName") |] :| []

    signEnvWithKeyset = setEnv (set eeMsgSigs (S.fromList samplePubKeys))

    tests =
      createGasUnitTests
      signEnvWithKeyset
      signEnvWithKeyset
      enforceGuardExpr


keysetRefGuardTests :: GasUnitTests
keysetRefGuardTests = defGasUnitTests keysetRefGuardExpr
  where
    keysetRefGuardExpr = [text| (keyset-ref-guard "$sampleLoadedKeysetName") |] :| []


createUserGuardTests :: GasUnitTests
createUserGuardTests = defGasUnitTests createUserGuardExpr
  where
    createUserGuardExpr = [text| (create-user-guard ($acctModuleNameText.enforce-true)) |] :| []


createPactGuardTests :: GasUnitTests
createPactGuardTests = tests
  where
    createPactGuardExpr = [text| (create-pact-guard "test") |] :| []

    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name "some-defpact-func" def) [])
    updateWithPactExec = setState (set evalPactExec mockPactExec)

    tests =
      createGasUnitTests
      updateWithPactExec
      updateWithPactExec
      createPactGuardExpr


createModuleGuardTests :: GasUnitTests
createModuleGuardTests = tests
  where
    createModuleGuardExpr = [text| (create-module-guard "test") |]

    updateStackFrame = setState (set evalCallStack [defStackFrame])

    tests =
      createGasUnitTests
      updateStackFrame
      updateStackFrame
      (createModuleGuardExpr :| []) 


withCapabilityTests :: GasUnitTests
withCapabilityTests = defGasUnitTests withCapExpr
  where
    withCapExpr = [text| ($acctModuleNameText.test-with-cap-func) |] :| []


requireCapabilityTests :: GasUnitTests
requireCapabilityTests = tests
  where
    requireCapExpr = [text| (require-capability ($acctModuleNameText.GOV)) |]
 
    cap = UserCapability acctModuleName (DefName "GOV") []
    updateGrantedCap = setState (set (evalCapabilities . capGranted) [cap])

    tests =
      createGasUnitTests
      updateGrantedCap
      updateGrantedCap
      (requireCapExpr :| [])


composeCapabilityTests :: GasUnitTests
composeCapabilityTests = tests
  where
    composeCapExpr = [text| (compose-capability ($acctModuleNameText.GOV)) |]

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
      (composeCapExpr :| [])


-- | Database native function tests
--   NOTE: Using MockDb means that database insert/write/update always succeed
txlogTests :: GasUnitTests
txlogTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (txlog $acctModuleNameText.accounts 0) |] :| []

txidsTests :: GasUnitTests
txidsTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (txids $acctModuleNameText.accounts 0) |] :| []


keylogTests :: GasUnitTests
keylogTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (keylog $acctModuleNameText.accounts "someId" 0) |] :| []


keysTests :: GasUnitTests
keysTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (keys $acctModuleNameText.accounts) |] :| []


selectTests :: GasUnitTests
selectTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (select $acctModuleNameText.accounts
                     (where "balance" (constantly true))
             ) |] :| []


withReadTests :: GasUnitTests
withReadTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (with-read
                $acctModuleNameText.accounts
                "someId"
                { "balance":= bal }
                bal
             )
      |] :| []

withDefaultReadTests :: GasUnitTests
withDefaultReadTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (with-default-read
                $acctModuleNameText.accounts
                "someId"
                { "balance": 1.0 }
                { "balance":= bal }
                bal
             )
      |] :| []

readTests :: GasUnitTests
readTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (read $acctModuleNameText.accounts "someId") |] :| []


writeTests :: GasUnitTests
writeTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (write $acctModuleNameText.accounts
                    "some-id-that-is-not-present"
                    { "balance": 0.0 }
             ) |] :| []


updateTests :: GasUnitTests
updateTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (update $acctModuleNameText.accounts
                     "someId"
                     { "balance": 10.0 }
             ) |] :| []


insertTests :: GasUnitTests
insertTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (insert $acctModuleNameText.accounts
                     "some-id-that-is-not-present"
                     { "balance": 0.0 }
             )|] :| []


describeTableTests :: GasUnitTests
describeTableTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (describe-table $acctModuleNameText.accounts) |] :| []


describeModuleTests :: GasUnitTests
describeModuleTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (describe-module "$acctModuleNameText") |] :| []


describeKeysetTests :: GasUnitTests
describeKeysetTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (describe-keyset "$sampleLoadedKeysetName") |] :| []
    

createTableTests :: GasUnitTests
createTableTests = defGasUnitTests allExprs
  where
    allExprs =
      [text| (create-table $acctModuleNameText.accounts-for-testing-table-creation) |] :| []


-- | Keyset native function tests
defineKeysetTests :: GasUnitTests
defineKeysetTests = tests
  where
    rotateExpr = [text| (define-keyset "$sampleLoadedKeysetName" $sampleLoadedKeysetName) |]

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
      (rotateExpr :| [])


enforceKeysetTests :: GasUnitTests
enforceKeysetTests = tests
  where
    enforceKeysetExpr = [text| (enforce-keyset '$sampleLoadedKeysetName) |]

    updateEnvMsgSig = setEnv (set eeMsgSigs (S.fromList samplePubKeys))

    tests =
      createGasUnitTests
      updateEnvMsgSig
      updateEnvMsgSig
      (enforceKeysetExpr :| [])
    

readKeysetTests :: GasUnitTests
readKeysetTests = tests
  where
    readKeysetExpr = [text| (read-keyset 'my-keyset) |]
    
    dataWithKeyset = toPactKeyset "my-keyset" "something" Nothing
    updateMsgBodyWithKeyset = setEnv (set eeMsgBody dataWithKeyset)

    tests =
      createGasUnitTests
      updateMsgBodyWithKeyset
      updateMsgBodyWithKeyset
      (readKeysetExpr :| [])


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
pactIdTests = tests
  where
    pactIdExpr = [text|(pact-id)|]
    
    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name "some-defpact-func" def) [])
    updateState = setState (set evalPactExec mockPactExec)
    
    tests =
      createGasUnitTests updateState updateState (pactIdExpr :| [])


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
      NEL.map yieldExpr strKeyIntValMapsExpr
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
      NEL.map yieldExprWithTargetChain strKeyIntValMapsExpr
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

    args = NEL.map (\(m,b) -> (m,resumeExpr b))
           $ NEL.zip strKeyIntValMaps strKeyIntValBindingsExpr

    toSPVTests (yieldMap, expr)
      = createGasUnitTests
        (setupForResume True (yieldMap, expr))
        (setupForResume True (yieldMap, expr))
        (expr :| [])

    toNonSPVTests (yieldMap, expr)
      = createGasUnitTests
        (setupForResume False (yieldMap, expr))
        (setupForResume False (yieldMap, expr))
        (expr :| [])

    tests = concatGasUnitTests $
            NEL.map toSPVTests args <>
            NEL.map toNonSPVTests args

    setupForResume isProv (yielded, expr) = allUpdates
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
          = set gasTestDescription $ expr <> " with provenance"
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

    allExprs = versionExpr :| []


readStringTests :: GasUnitTests
readStringTests = tests
  where
    readStringExpr = [text|(read-string "name")|]

    allUpdates i = (updateDesc . updateEnv)
      where
        strVal
          = A.object ["name" A..= i]
        updateEnv
          = setEnv (set eeMsgBody strVal)
        updateDesc
          = set gasTestDescription
            (readStringExpr <> " when "
             <> (T.pack $ show strVal))

    setupTests i
      = createGasUnitTests
        (allUpdates i)
        (allUpdates i)
        (readStringExpr :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests escapedStringsExpr


readMsgTests :: GasUnitTests
readMsgTests = tests
  where
    readMsgExpr = [text|(read-msg)|]
       
    allUpdates m = setupMsgBody . updateDesc
      where
        setupMsgBody
          = setEnv (set eeMsgBody $ toJSON m)
        updateDesc
          = set gasTestDescription
            (readMsgExpr <> " when " <> (T.pack $ show $ toJSON m))
    
    setupTests m
      = createGasUnitTests
        (allUpdates m)
        (allUpdates m)
        (readMsgExpr :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests strKeyIntValMapsExpr


readIntegerTests :: GasUnitTests
readIntegerTests = tests
  where
    readIntExpr = [text|(read-integer "amount")|]
    
    allUpdates i = (updateDesc . updateEnv)
      where
        intVal
          = A.object ["amount" A..= i]
        updateEnv
          = setEnv (set eeMsgBody intVal)
        updateDesc
          = set gasTestDescription
            (readIntExpr <> " when "
             <> (T.pack $ show intVal))

    setupTests i
      = createGasUnitTests
        (allUpdates i)
        (allUpdates i)
        (readIntExpr :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests sizesExpr


readDecimalTests :: GasUnitTests
readDecimalTests = tests
  where
    readDecExpr = [text|(read-decimal "amount")|]
    
    allUpdates d = updateDesc . updateEnv
      where
        d' = d <> "1"
        decVal = A.object ["amount" A..= [text|0.$d'|]]
        updateEnv
          = setEnv (set eeMsgBody decVal)
        updateDesc
          = set gasTestDescription
            (readDecExpr <> " when "
              <> (T.pack $ show decVal))

    setupTests d
      = createGasUnitTests
        (allUpdates d)
        (allUpdates d)
        (readDecExpr :| [])
   
    tests = concatGasUnitTests $
            NEL.map setupTests sizesExpr


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
namespaceTests = tests
  where
    namespaceExpr = [text| (namespace '$sampleNamespaceName) |] :| []

    updateEnvWithSig =
      setEnv $ set eeMsgSigs (S.fromList samplePubKeys)

    tests = createGasUnitTests
            updateEnvWithSig
            updateEnvWithSig
            namespaceExpr


defineNamespaceTests :: GasUnitTests
defineNamespaceTests = tests
  where
    tests = simpleDefTests <> rotateNamespaceTests
    
    simpleDefTests = defGasUnitTests simpleDefExpr
      where
        simpleDefExpr =
          [text| (define-namespace 'some-other-namespace $sampleLoadedKeysetName) |] :| []
 
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
          (rotateExpr :| [])


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
