{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Pact.GasModel.GasTests
  (
    unitTests
  , allNatives
  , untestedNatives
  , unitTestFromDef

  ) where

import Control.Lens hiding ((.=),DefName)
import Data.Aeson (toJSON, ToJSON(..))
import Data.Bool (bool)
import Data.Default (def)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import NeatInterpolation (text)


import qualified Data.Aeson as A
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


import Pact.GasModel.Types
import Pact.GasModel.Utils
import Pact.Interpreter
import Pact.Native
import Pact.Types.Capability
import Pact.Types.Lang
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.Runtime


-- | Gas benchmark tests for Pact native functions
allNatives :: [NativeDefName]
allNatives = map fst (concatMap snd natives) <> nonNatives

-- | Non-native concepts to benchmark
nonNatives :: [NativeDefName]
nonNatives = [NativeDefName "use",
              NativeDefName "module",
              NativeDefName "interface"]

untestedNatives :: [NativeDefName]
untestedNatives = foldl' untested [] allNatives
  where
    untested li nativeName = case (HM.lookup nativeName unitTests) of
      Nothing -> nativeName : li
      Just _ -> li


unitTests :: HM.HashMap NativeDefName GasUnitTests
unitTests = HM.fromList $ foldl' getUnitTest [] allNatives
  where
    getUnitTest li nativeName =
      case unitTestFromDef nativeName of
        Nothing -> li
        Just ts -> (nativeName, ts) : li



unitTestFromDef :: NativeDefName -> Maybe GasUnitTests
unitTestFromDef nativeName = ($ nativeName) <$> HM.lookup nativeName allTests

allTests :: HM.HashMap NativeDefName (NativeDefName -> GasUnitTests)
allTests = HM.fromList
      -- General native functions
    [ ("at", atTests)
    , ("base64-decode", base64DecodeTests)
    , ("base64-encode", base64EncodeTests)
    , ("bind", bindTests)
    , ("chain-data", chainDataTests)
    , ("compose", composeTests)
    , ("concat", concatTests)
    , ("constantly", constantlyTests)
    , ("contains", containsTests)
    , ("define-namespace", defineNamespaceTests)
    , ("drop", dropTests)
    , ("enforce", enforceTests)
    , ("enforce-one", enforceOneTests)
    , ("enforce-pact-version", enforcePactVersionTests)
    , ("enumerate", enumerateTests)
    , ("filter", filterTests)
    , ("fold", foldTests)
    , ("format", formatTests)
    , ("hash", hashTests)
    , ("identity", identityTests)
    , ("if", ifTests)
    , ("int-to-str", intToStrTests)
    , ("is-charset", isCharsetTests)
    , ("length", lengthTests)
    , ("list-modules", listModulesTests)
    , ("make-list", makeListTests)
    , ("map", mapTests)
    , ("zip", zipTests)
    , ("namespace", namespaceTests)
    , ("pact-id", pactIdTests)
    , ("pact-version", pactVersionTests)
    , ("read-decimal", readDecimalTests)
    , ("read-integer", readIntegerTests)
    , ("read-msg", readMsgTests)
    , ("read-string", readStringTests)
    , ("remove", removeTests)
    , ("resume", resumeTests)
    , ("reverse", reverseTests)
    , ("sort", sortTests)
    , ("str-to-int", strToIntTests)
    , ("str-to-list", strToListTests)
    , ("take", takeTests)
    , ("try", tryTests)
    , ("tx-hash", txHashTests)
    , ("typeof", typeOfTests)
    , ("distinct", distinctTests)
    , ("where", whereTests)
    , ("yield", yieldTests)

      -- Operators native functions
    , ("!=", notEqualOptTests)
    , ("&", bitwiseOptTests)
    , ("*", multOptTests)
    , ("+", addOptTests)
    , ("-", subOptTests)
    , ("/", divOptTests)
    , ("<", lessThanOptTests)
    , ("<=", lessThanEqualOptTests)
    , ("=", equalOptTests)
    , (">", greaterThanOptTests)
    , (">=", greaterThanEqOptTests)
    , ("^", raiseOptTests)
    , ("abs", absOptTests)
    , ("and", andOptTests)
    , ("and?", andFuncOptTests)
    , ("ceiling", ceilingOptTests)
    , ("exp", expOptTests)
    , ("floor", floorOptTests)
    , ("ln", lnOptTests)
    , ("log", logOptTests)
    , ("mod", modOptTests)
    , ("not", notOptTests)
    , ("not?", notFuncOptTests)
    , ("or", orOptTests)
    , ("or?", orFuncOptTests)
    , ("round", roundOptTests)
    , ("shift", shiftOptTests)
    , ("sqrt", sqrtOptTests)
    , ("xor", xorOptTests)
    , ("|", bitwiseOrOptTests)
    , ("~", reverseBitsOptTests)

      -- Time native functions
    , ("add-time", addTimeTests)
    , ("days", daysTests)
    , ("diff-time", diffTimeTests)
    , ("format-time", formatTimeTests)
    , ("hours", hoursTests)
    , ("minutes", minutesTests)
    , ("parse-time", parseTimeTests)
    , ("time", timeTests)

      -- Commitments native functions
    , ("decrypt-cc20p1305", decryptCc20p1305Tests)
    , ("validate-keypair", validateKeypairTests)

      -- Keyset native functions
    , ("define-keyset", defineKeysetTests)
    , ("enforce-keyset", enforceKeysetTests)
    , ("keys-2", keys2Tests)
    , ("keys-all", keysAllTests)
    , ("keys-any", keysAnyTests)
    , ("read-keyset", readKeysetTests)

      -- Database native functions
    , ("create-table", createTableTests)
    , ("describe-keyset", describeKeysetTests)
    , ("describe-module", describeModuleTests)
    , ("describe-table", describeTableTests)
    , ("describe-namespace", describeNamespaceTests)
    , ("insert", insertTests)
    , ("keylog", keylogTests)
    , ("keys", keysTests)
    , ("read", readTests)
    , ("select", selectTests)
    , ("txids", txidsTests)
    , ("txlog", txlogTests)
    , ("update", updateTests)
    , ("with-default-read", withDefaultReadTests)
    , ("with-read", withReadTests)
    , ("write", writeTests)
    , ("fold-db", foldDBTests)

      -- Capabilities native functions
    , ("compose-capability", composeCapabilityTests)
    , ("install-capability", installCapabilityTests)
    , ("require-capability", requireCapabilityTests)
    , ("with-capability", withCapabilityTests)
    , ("emit-event", emitEventTests)

      -- Guard native tests
    , ("create-module-guard", createModuleGuardTests)
    , ("create-pact-guard", createPactGuardTests)
    , ("create-user-guard", createUserGuardTests)
    , ("create-capability-guard", createCapabilityGuardTests)
    , ("create-capability-pact-guard", createCapabilityPactGuardTests)
    , ("enforce-guard", enforceGuardTests)
    , ("keyset-ref-guard", keysetRefGuardTests)

      -- Principal creation and validation
    , ("create-principal", createPrincipalTests)
    , ("validate-principal", validatePrincipalTests)
    , ("is-principal", isPrincipalTests)
    , ("typeof-principal", typeofPrincipalTests)

      -- ZK pairing for curve BN254
    , ("point-add", pointAddTests)
    , ("scalar-mult", scalarMulTests)
    , ("pairing-check", pairingCheckTests)
    , ("egcd", egcdTests)
    , ("keccak256-bs", keccak256bsTests)
    , ("poseidon-hash", poseidonTests)

      -- Non-native concepts to benchmark
    , ("use", useTests)
    , ("module", moduleTests)
    , ("interface", interfaceTests)
    ]

-- | Non-native concepts tests
interfaceTests :: NativeDefName -> GasUnitTests
interfaceTests = defGasUnitTest $ PactExpression interfaceExprText Nothing
  where
    interfaceExprText = [text|
    (interface my-interface
      (defun say-hello:string (name:string))
    )

    (module some-random-module GOV
      (implements my-interface)
      (defcap GOV ()
        true
      )

      (defun say-hello:string (name:string)
        name)
    )|]


moduleTests :: NativeDefName -> GasUnitTests
moduleTests = defGasUnitTests allExprs
  where
    moduleExprText = [text|
    (module some-random-module GOV
      (defcap GOV ()
        true ))|]
    moduleExpr = defPactExpression moduleExprText
    moduleRotateDesc = [text|(module accounts GOV [...some module code ...]) update module|]
    moduleRotateExpr = PactExpression (regressionModule acctModuleName) (Just moduleRotateDesc)
    allExprs = [moduleExpr, moduleRotateExpr]



useTests :: NativeDefName -> GasUnitTests
useTests = defPactExpGasTest [text| (use $acctModuleNameText) |]


-- | Capabilities native function tests
enforceGuardTests :: NativeDefName -> GasUnitTests
enforceGuardTests = tests
  where
    enforceGuardExpr = defPactExpression
      [text| (enforce-guard "$sampleLoadedKeysetName") |]

    signEnvWithKeyset = setEnv (set eeMsgSigs (M.fromList $ F.toList samplePubKeysWithCaps))

    tests =
      createGasUnitTests
      signEnvWithKeyset
      signEnvWithKeyset
      [enforceGuardExpr]


keysetRefGuardTests :: NativeDefName -> GasUnitTests
keysetRefGuardTests = defPactExpGasTest
    [text| (keyset-ref-guard "$sampleLoadedKeysetName") |]



createUserGuardTests :: NativeDefName -> GasUnitTests
createUserGuardTests = defPactExpGasTest
    [text| (create-user-guard ($acctModuleNameText.enforce-true)) |]

createCapabilityGuardTests :: NativeDefName -> GasUnitTests
createCapabilityGuardTests =
  createGasUnitTests
  updateWithPactExec
  updateWithPactExec
  [ defPactExpression
    [text| (create-capability-guard ($acctModuleNameText.MANAGEDCAP "foo" "bar")) |]
  ]

createCapabilityPactGuardTests :: NativeDefName -> GasUnitTests
createCapabilityPactGuardTests =
  createGasUnitTests
  updateWithPactExec
  updateWithPactExec
  [ defPactExpression
    [text| (create-capability-pact-guard ($acctModuleNameText.MANAGEDCAP "foo" "bar")) |]
  ]

mockPactExec :: Maybe PactExec
mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name $ BareName "some-defpact-func" def) [])
                          False
                          mempty

updateWithPactExec :: GasSetup a -> GasSetup a
updateWithPactExec = setState (set evalPactExec mockPactExec)


createPactGuardTests :: NativeDefName -> GasUnitTests
createPactGuardTests = tests
  where
    createPactGuardExpr =
      defPactExpression [text| (create-pact-guard "test") |]


    tests =
      createGasUnitTests
      updateWithPactExec
      updateWithPactExec
      [createPactGuardExpr]


createModuleGuardTests :: NativeDefName -> GasUnitTests
createModuleGuardTests = tests
  where
    createModuleGuardExpr = PactExpression [text| (create-module-guard "test") |] Nothing

    updateStackFrame = setState (set evalCallStack [someStackFrame])

    tests =
      createGasUnitTests
      updateStackFrame
      updateStackFrame
      [createModuleGuardExpr]


installCapabilityTests :: NativeDefName -> GasUnitTests
installCapabilityTests = defPactExpGasTest
    [text|(install-capability ($acctModuleNameText.MANAGEDCAP "" ""))|]


withCapabilityTests :: NativeDefName -> GasUnitTests
withCapabilityTests = defPactExpGasTest
    [text| ($acctModuleNameText.test-with-cap-func) |]


emitEventTests :: NativeDefName -> GasUnitTests
emitEventTests = defPactExpGasTest
    [text| ($acctModuleNameText.test-emit-event-func) |]

requireCapabilityTests :: NativeDefName -> GasUnitTests
requireCapabilityTests = tests
  where
    requireCapExpr =
      defPactExpression [text| (require-capability ($acctModuleNameText.GOV)) |]

    cap = SigCapability (QualifiedName acctModuleName "GOV" def) []
    capSlot = CapSlot CapCallStack cap []
    updateGrantedCap = setState (set (evalCapabilities . capStack) [capSlot])

    tests =
      createGasUnitTests
      updateGrantedCap
      updateGrantedCap
      [requireCapExpr]

composeCapabilityTests :: NativeDefName -> GasUnitTests
composeCapabilityTests = tests
  where
    composeCapExpr =
      defPactExpression [text| (compose-capability ($acctModuleNameText.GOV)) |]

    capInStackframe =
      StackFrame "" def
      (Just ((FunApp def ""
           (Just someModuleName) Defcap (funTypes $ FunType [] TyAny) Nothing)
        ,[])
      )
    updateStateWithCap = setState (set evalCallStack [capInStackframe])

    tests =
      createGasUnitTests
      updateStateWithCap
      updateStateWithCap
      [composeCapExpr]


-- | Database native function tests
--   NOTE: Using MockDb means that database insert/write/update always succeed
txlogTests :: NativeDefName -> GasUnitTests
txlogTests = defPactExpGasTest
    [text| (txlog $acctModuleNameText.accounts 0) |]

txidsTests :: NativeDefName -> GasUnitTests
txidsTests = defPactExpGasTest
    [text| (txids $acctModuleNameText.accounts 0) |]


keylogTests :: NativeDefName -> GasUnitTests
keylogTests = defPactExpGasTest
    [text| (keylog $acctModuleNameText.accounts "someId" 0) |]

keysTests :: NativeDefName -> GasUnitTests
keysTests = defPactExpGasTest
    [text| (keys $acctModuleNameText.accounts) |]


selectTests :: NativeDefName -> GasUnitTests
selectTests = defPactExpGasTest
  [text| (select $acctModuleNameText.accounts
           (where "balance" (constantly true))
         ) |]


withReadTests :: NativeDefName -> GasUnitTests
withReadTests = defPactExpGasTest
    [text| (with-read
              $acctModuleNameText.accounts
              "someId"
              { "balance":= bal }
              bal
           )
           |]


withDefaultReadTests :: NativeDefName -> GasUnitTests
withDefaultReadTests = defPactExpGasTest
    [text| (with-default-read
              $acctModuleNameText.accounts
              "someId"
              { "balance": 1.0 }
              { "balance":= bal }
              bal
           )
           |]


readTests :: NativeDefName -> GasUnitTests
readTests = defPactExpGasTest
    [text| (read $acctModuleNameText.accounts "someId") |]

writeTests :: NativeDefName -> GasUnitTests
writeTests = defPactExpGasTest
    [text| (write $acctModuleNameText.accounts
                  "some-id-that-is-not-present"
                  { "balance": 0.0 }
           ) |]

foldDBTests :: NativeDefName -> GasUnitTests
foldDBTests = defPactExpGasTest
    [text|
      (let*
        ((qry (lambda (k obj) true)) ;; select all rows
          (f (lambda (k x) (at 'balance x)))
        )
        (fold-db $acctModuleNameText.accounts (qry) (f))
        ) |]


updateTests :: NativeDefName -> GasUnitTests
updateTests = defPactExpGasTest
    [text| (update $acctModuleNameText.accounts
                   "someId"
                   { "balance": 10.0 }
           ) |]


insertTests :: NativeDefName -> GasUnitTests
insertTests = defPactExpGasTest
    [text| (insert $acctModuleNameText.accounts
                   "some-id-that-is-not-present"
                   { "balance": 0.0 }
           )|]


describeTableTests :: NativeDefName -> GasUnitTests
describeTableTests = defPactExpGasTest
    [text| (describe-table $acctModuleNameText.accounts) |]


describeModuleTests :: NativeDefName -> GasUnitTests
describeModuleTests = defPactExpGasTest
    [text| (describe-module "$acctModuleNameText") |]


describeKeysetTests :: NativeDefName -> GasUnitTests
describeKeysetTests = defPactExpGasTest
    [text| (describe-keyset "$sampleLoadedKeysetName") |]

describeNamespaceTests :: NativeDefName -> GasUnitTests
describeNamespaceTests = defPactExpGasTest
      [text| (describe-namespace "$sampleNamespaceName") |]

createTableTests :: NativeDefName -> GasUnitTests
createTableTests = defPactExpGasTest
      [text| (create-table $acctModuleNameText.accounts-for-testing-table-creation) |]


-- | Keyset native function tests
defineKeysetTests :: NativeDefName -> GasUnitTests
defineKeysetTests = tests
  where
    simpleExpr =
      defPactExpression
      [text| (define-keyset "some-keyset-name-not-present-already" $sampleLoadedKeysetName) |]
    rotateExprText = [text| (define-keyset "$sampleLoadedKeysetName" $sampleLoadedKeysetName) |]
    rotateExpr = PactExpression rotateExprText (Just $ rotateExprText <> ": rotating keyset")
    allExprs = [rotateExpr, simpleExpr]

    -- Keyset rotation causes previous keyset to be enforced
    updateEnvMsgSig :: GasSetup e -> GasSetup e
    updateEnvMsgSig = setEnv (set eeMsgSigs (M.fromList $ F.toList samplePubKeysWithCaps))

    tests =
      createGasUnitTests
      updateEnvMsgSig
      updateEnvMsgSig
      allExprs


enforceKeysetTests :: NativeDefName -> GasUnitTests
enforceKeysetTests = tests
  where
    enforceKeysetExpr = defPactExpression [text| (enforce-keyset '$sampleLoadedKeysetName) |]

    updateEnvMsgSig = setEnv (set eeMsgSigs (M.fromList $ F.toList samplePubKeysWithCaps))

    tests =
      createGasUnitTests
      updateEnvMsgSig
      updateEnvMsgSig
      [enforceKeysetExpr]


readKeysetTests :: NativeDefName -> GasUnitTests
readKeysetTests = tests
  where
    readKeysetExpr = defPactExpression [text| (read-keyset 'my-keyset) |]

    dataWithKeyset = toPactKeyset "my-keyset" "something" Nothing
    updateMsgBodyWithKeyset = setEnv (set eeMsgBody dataWithKeyset)

    tests =
      createGasUnitTests
      updateMsgBodyWithKeyset
      updateMsgBodyWithKeyset
      [readKeysetExpr]


keysAnyTests :: NativeDefName -> GasUnitTests
keysAnyTests = defPactExpGasTest [text|(keys-any 10 1)|]

keysAllTests :: NativeDefName -> GasUnitTests
keysAllTests = defPactExpGasTest [text|(keys-all 3 3)|]

keys2Tests :: NativeDefName -> GasUnitTests
keys2Tests = defPactExpGasTest [text|(keys-2 3 1)|]

-- | Commitments native function tests
decryptCc20p1305Tests :: NativeDefName -> GasUnitTests
decryptCc20p1305Tests = defPactExpGasTest
      [text| (decrypt-cc20p1305
              "Zi1REj5-iA"
              "AAAAAAECAwQFBgcI"
              "YWFk"
              "FYP6lG7xq7aExvoaHIH8Jg"
              "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
              "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
      |]


validateKeypairTests :: NativeDefName -> GasUnitTests
validateKeypairTests = defPactExpGasTest
      [text| (validate-keypair
             "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
             "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a") |]


-- | Time native function tests
addTimeTests :: NativeDefName -> GasUnitTests
addTimeTests = defPactExpGasTest
    [text| (add-time (time "2016-07-22T12:00:00Z") 15) |]

daysTests :: NativeDefName -> GasUnitTests
daysTests = defGasUnitTests allExprs
  where
    daysExprText n = [text| (days $n) |]
    allExprs = map (createPactExpr daysExprText) sizesExpr


diffTimeTests :: NativeDefName -> GasUnitTests
diffTimeTests = defPactExpGasTest
      [text| (diff-time (time "2016-07-22T12:00:00Z")
                        (time "2018-07-22T12:00:00Z"))
      |]

formatTimeTests :: NativeDefName -> GasUnitTests
formatTimeTests = defGasUnitTests allExprs
  where
    formatTimeSimpleExpr =
      defPactExpression [text| (format-time "%F" (time "2016-07-22T12:00:00Z")) |]
    formatTimeComplexExpr =
      defPactExpression [text| (format-time "%Y-%m-%dT%H:%M:%S%N" (time "2016-07-23T13:30:45Z")) |]

    allExprs = [formatTimeSimpleExpr, formatTimeComplexExpr]


hoursTests :: NativeDefName -> GasUnitTests
hoursTests = defGasUnitTests allExprs
  where
    hoursExprText n = [text| (hours $n) |]
    allExprs = map (createPactExpr hoursExprText) sizesExpr


minutesTests :: NativeDefName -> GasUnitTests
minutesTests = defGasUnitTests allExprs
  where
    minutesExpr n =
      [text| (minutes $n) |]
    allExprs = map (createPactExpr minutesExpr) sizesExpr


parseTimeTests :: NativeDefName -> GasUnitTests
parseTimeTests = defGasUnitTests allExprs
  where
    parseTimeSimpleExpr =
      defPactExpression [text| (parse-time "%F" "2016-07-22") |]
    parseTimeComplexExpr =
      defPactExpression [text| (parse-time "%Y-%m-%dT%H:%M:%S%N" "2016-07-23T13:30:45+00:00") |]
    allExprs = [parseTimeSimpleExpr, parseTimeComplexExpr]


timeTests :: NativeDefName -> GasUnitTests
timeTests = defPactExpGasTest [text| (time "2016-07-22T12:00:00Z") |]


-- | Operators native function tests
reverseBitsOptTests :: NativeDefName -> GasUnitTests
reverseBitsOptTests = defGasUnitTests allExprs
  where
    reverseBitsExpr x = [text| (~ $x) |]

    allExprs = map (createPactExpr reverseBitsExpr) sizesExpr


bitwiseOrOptTests :: NativeDefName -> GasUnitTests
bitwiseOrOptTests = defGasUnitTests allExprs
  where
    bitwiseOrExpr x = [text| (| 2 $x) |]

    allExprs = map (createPactExpr bitwiseOrExpr) sizesExpr


xorOptTests :: NativeDefName -> GasUnitTests
xorOptTests = defGasUnitTests allExprs
  where
    xorExpr x = [text| (xor 2 $x) |]

    allExprs = map (createPactExpr xorExpr) sizesExpr


sqrtOptTests :: NativeDefName -> GasUnitTests
sqrtOptTests = defGasUnitTests allExprs
  where
    sqrtExpr x = [text| (sqrt $x) |]
    sqrtDecimalExpr x = [text| (sqrt $x.1) |]

    allExprs = map (createPactExpr sqrtExpr) sizesExpr
      <> map (createPactExpr sqrtDecimalExpr) sizesExpr


shiftOptTests :: NativeDefName -> GasUnitTests
shiftOptTests = defGasUnitTests allExprs
  where
    shiftExpr x =
      [text| (shift 2 $x) |]
    shiftNegExpr x =
      [text| (shift -2 $x) |]

    allExprs = map (createPactExpr shiftExpr) sizesExpr
      <> map (createPactExpr shiftNegExpr) sizesExpr


roundOptTests :: NativeDefName -> GasUnitTests
roundOptTests = defGasUnitTests allExprs
  where
    roundExpr x =
      [text| (round $x.12345) |]
    roundPrecExpr x =
      [text| (round $x.12345 4) |]

    allExprs = map (createPactExpr roundExpr) sizesExpr
      <> map (createPactExpr roundPrecExpr) sizesExpr


orFuncOptTests :: NativeDefName -> GasUnitTests
orFuncOptTests = defPactExpGasTest [text| (or? (identity) (identity) true) |]


orOptTests :: NativeDefName -> GasUnitTests
orOptTests = defPactExpGasTest [text| (or false false) |]


notFuncOptTests :: NativeDefName -> GasUnitTests
notFuncOptTests = defPactExpGasTest [text| (not? (identity) true) |]


notOptTests :: NativeDefName -> GasUnitTests
notOptTests = defPactExpGasTest [text| (not true) |]


modOptTests :: NativeDefName -> GasUnitTests
modOptTests = defGasUnitTests allExprs
  where
    modExpr x =
      [text| (mod $x 2) |]

    allExprs = map (createPactExpr modExpr) sizesExpr



logOptTests :: NativeDefName -> GasUnitTests
logOptTests = defGasUnitTests allExprs
  where
    logExpr y =
      [text| (log 2 $y) |]
    logDecimalExpr y =
      [text| (log 2 $y.1) |]

    allExprs = map (createPactExpr logExpr) sizesExpr
      <> map (createPactExpr logDecimalExpr) sizesExpr



lnOptTests :: NativeDefName -> GasUnitTests
lnOptTests = defGasUnitTests allExprs
  where
    lnExpr x =
      [text| (ln $x) |]
    lnDecimalExpr x =
      [text| (ln $x.1) |]

    allExprs = map (createPactExpr lnExpr) sizesExpr
      <> map (createPactExpr lnDecimalExpr) sizesExpr


floorOptTests :: NativeDefName -> GasUnitTests
floorOptTests = defGasUnitTests allExprs
  where
    floorExpr x =
      [text| (floor $x.12345) |]
    floorPrecExpr x =
      [text| (floor $x.12345 4) |]

    allExprs = map (createPactExpr floorExpr) sizesExpr
      <> map (createPactExpr floorPrecExpr) sizesExpr


expOptTests :: NativeDefName -> GasUnitTests
expOptTests = defGasUnitTests allExprs
  where
    expExprSmall =
      defPactExpression [text| (exp 1) |]
    expExprMed =
      defPactExpression [text| (exp 10) |]
    expExprLarge =
      defPactExpression [text| (exp 100) |]

    allExprs = [expExprSmall
               ,expExprMed
               ,expExprLarge]


ceilingOptTests :: NativeDefName -> GasUnitTests
ceilingOptTests = defGasUnitTests allExprs
  where
    ceilingExpr x =
      [text| (ceiling $x.12345) |]
    ceilingPrecExpr x =
      [text| (ceiling $x.12345 4) |]

    allExprs = map (createPactExpr ceilingExpr) sizesExpr
      <> map (createPactExpr ceilingPrecExpr) sizesExpr


andFuncOptTests :: NativeDefName -> GasUnitTests
andFuncOptTests = defPactExpGasTest [text| (and? (identity) (identity) true) |]


andOptTests :: NativeDefName -> GasUnitTests
andOptTests = defPactExpGasTest [text| (and false true) |]


absOptTests :: NativeDefName -> GasUnitTests
absOptTests = defGasUnitTests allExprs
  where
    absExpr x =
      [text| (abs -$x) |]
    absDecimalExpr x =
      [text| (abs -$x.0) |]

    allExprs = map (createPactExpr absExpr) sizesExpr
      <> map (createPactExpr absDecimalExpr) sizesExpr


raiseOptTests :: NativeDefName -> GasUnitTests
raiseOptTests = defGasUnitTests allExprs
  where
    raiseExpr y =
      [text| (^ 2 $y) |]
    raiseDecimalExpr y =
      [text| (^ 2.1 $y.1) |]
    raiseBothExpr y =
      [text| (^ 2.1 $y) |]

    allExprs = map (createPactExpr raiseExpr) sizesExpr
      <> map (createPactExpr raiseDecimalExpr) sizesExpr
      <> map (createPactExpr raiseBothExpr) sizesExpr


greaterThanEqOptTests :: NativeDefName -> GasUnitTests
greaterThanEqOptTests = defGasUnitTests allExprs
  where
    greaterEqExpr x =
      [text| (>= $x $x) |]
    greaterEqDecimalExpr x =
      [text| (>= $x.0 $x.0) |]
    greaterEqTimeExpr =
      [text| (>= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = map (createPactExpr greaterEqExpr) sizesExpr
      <> map (createPactExpr greaterEqDecimalExpr) sizesExpr
      <> map (createPactExpr greaterEqExpr) escapedStringsExpr
      <> [defPactExpression greaterEqTimeExpr]


greaterThanOptTests :: NativeDefName -> GasUnitTests
greaterThanOptTests = defGasUnitTests allExprs
  where
    greaterExpr x =
      [text| (> $x $x) |]
    greaterDecimalExpr x =
      [text| (> $x.0 $x.0) |]
    greaterTimeExpr =
      [text| (> (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = map (createPactExpr greaterExpr) sizesExpr
      <> map (createPactExpr greaterDecimalExpr) sizesExpr
      <> map (createPactExpr greaterExpr) escapedStringsExpr
      <> [defPactExpression greaterTimeExpr]


equalOptTests :: NativeDefName -> GasUnitTests
equalOptTests = defGasUnitTests allExprs
  where
    eqExpr x =
      [text| (= $x $x) |]
    eqDecimalExpr x =
      [text| (= $x.0 $x.0) |]
    eqTimeExpr =
      [text| (= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = map (createPactExpr eqExpr) sizesExpr
      <> map (createPactExpr eqDecimalExpr) sizesExpr
      <> map (createPactExpr eqExpr) escapedStringsExpr
      <> map (createPactExpr eqExpr) strKeyIntValMapsExpr
      <> map (createPactExpr eqExpr) intListsExpr
      <> [defPactExpression eqTimeExpr]


lessThanEqualOptTests :: NativeDefName -> GasUnitTests
lessThanEqualOptTests = defGasUnitTests allExprs
  where
    lessEqExpr x =
      [text| (<= $x $x) |]
    lessEqDecimalExpr x =
      [text| (<= $x.0 $x.0) |]
    lessEqTimeExpr =
      [text| (<= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = map (createPactExpr lessEqExpr) sizesExpr
      <> map (createPactExpr lessEqDecimalExpr) sizesExpr
      <> map (createPactExpr lessEqExpr) escapedStringsExpr
      <> [defPactExpression lessEqTimeExpr]


lessThanOptTests :: NativeDefName -> GasUnitTests
lessThanOptTests = defGasUnitTests allExprs
  where
    lessExpr x =
      [text| (< $x $x) |]
    lessDecimalExpr x =
      [text| (< $x.0 $x.0) |]
    lessTimeExpr =
      [text| (< (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = map (createPactExpr lessExpr) sizesExpr
      <> map (createPactExpr lessDecimalExpr) sizesExpr
      <> map (createPactExpr lessExpr) escapedStringsExpr
      <> [defPactExpression lessTimeExpr]


divOptTests :: NativeDefName -> GasUnitTests
divOptTests = defGasUnitTests allExprs
  where
    divExpr x =
      [text| (/ $x $x) |]
    divDecimalExpr x =
      [text| (/ $x.0 $x.0) |]
    divBothExpr x =
      [text| (/ $x.0 $x) |]

    allExprs = map (createPactExpr divExpr) sizesExpr
      <> map (createPactExpr divDecimalExpr) sizesExpr
      <> map (createPactExpr divBothExpr) sizesExpr


subOptTests :: NativeDefName -> GasUnitTests
subOptTests = defGasUnitTests allExprs
  where
    subExpr x =
      [text| (- $x $x) |]
    subDecimalExpr x =
      [text| (- $x.0 $x.0) |]
    subBothExpr x =
      [text| (- $x.0 $x) |]
    subOneExpr x =
      [text| (- $x) |]
    subOneDecimalExpr x =
      [text| (- $x.0) |]

    allExprs = map (createPactExpr subExpr) sizesExpr
      <> map (createPactExpr subDecimalExpr) sizesExpr
      <> map (createPactExpr subBothExpr) sizesExpr
      <> map (createPactExpr subOneExpr) sizesExpr
      <> map (createPactExpr subOneDecimalExpr) sizesExpr


addOptTests :: NativeDefName -> GasUnitTests
addOptTests = defGasUnitTests allExprs
  where
    addExpr x =
      [text| (+ $x $x) |]
    addDecimalExpr x =
      [text| (+ $x.0 $x.0) |]
    addBothExpr x =
      [text| (+ $x.0 $x) |]

    allExprs = map (createPactExpr addExpr) sizesExpr
      <> map (createPactExpr addDecimalExpr) sizesExpr
      <> map (createPactExpr addBothExpr) sizesExpr
      <> map (createPactExpr addExpr) escapedStringsExpr
      <> map (createPactExpr addExpr) strKeyIntValMapsExpr


multOptTests :: NativeDefName -> GasUnitTests
multOptTests = defGasUnitTests allExprs
  where
    multIntExpr x =
      [text| (* $x $x) |]
    multDecimalExpr x =
      [text| (* $x.0 $x.0) |]
    multBothExpr x =
      [text| (* $x.0 $x) |]

    allExprs = map (createPactExpr multIntExpr) sizesExpr
      <> map (createPactExpr multDecimalExpr) sizesExpr
      <> map (createPactExpr multBothExpr) sizesExpr


bitwiseOptTests :: NativeDefName -> GasUnitTests
bitwiseOptTests = defGasUnitTests allExprs
  where
    bitwiseExpr x =
      [text| (& $x $x) |]

    allExprs = map (createPactExpr bitwiseExpr) sizesExpr


notEqualOptTests :: NativeDefName -> GasUnitTests
notEqualOptTests = defGasUnitTests allExprs
  where
    notEqualExpr x =
      [text| (!= $x $x) |]
    notEqualDecimalExpr x =
      [text| (!= $x.0 $x.0) |]

    allExprs = map (createPactExpr notEqualExpr) sizesExpr
      <> map (createPactExpr notEqualExpr) escapedStringsExpr
      <> map (createPactExpr notEqualDecimalExpr) sizesExpr
      <> map (createPactExpr notEqualExpr) intListsExpr
      <> map (createPactExpr notEqualExpr) strKeyIntValMapsExpr



-- | General native function tests
whereTests :: NativeDefName -> GasUnitTests
whereTests = defGasUnitTests allExprs
  where
    whereExpr obj =
      [text| (where "a1" (constantly true) $obj) |]

    allExprs = map (createPactExpr whereExpr) strKeyIntValMapsExpr


typeOfTests :: NativeDefName -> GasUnitTests
typeOfTests = defGasUnitTests allExprs
  where
    typeOfExpr t =
      [text| (typeof $t) |]

    allExprs = map (createPactExpr typeOfExpr) strKeyIntValMapsExpr
      <> map (createPactExpr typeOfExpr) escapedStringsExpr
      <> map (createPactExpr typeOfExpr) intListsExpr
      <> map (createPactExpr typeOfExpr) sizesExpr


txHashTests :: NativeDefName -> GasUnitTests
txHashTests = defPactExpGasTest [text| (tx-hash) |]


tryTests :: NativeDefName -> GasUnitTests
tryTests = defGasUnitTests allExprs
  where
    tryPassExpr =
      defPactExpression [text| (try true (enforce true "this will definitely pass")) |]
    tryFailExpr =
      defPactExpression [text| (try true (enforce false "this will definitely fail")) |]

    allExprs = [tryPassExpr, tryFailExpr ]


takeTests :: NativeDefName -> GasUnitTests
takeTests = defGasUnitTests allExprs
  where
    takeFirstExpr t =
       [text| (take 1 $t) |]
    takeLastExpr t =
       [text| (take -1 $t) |]
    takeKeysExpr (PactExpression keyList keyDesc', PactExpression obj objDesc') =
      PactExpression
      [text| (take $keyList $obj) |]
      (Just [text| (take $keyListDesc $objDesc) |])
      where keyListDesc = fromMaybe keyList keyDesc'
            objDesc = fromMaybe obj objDesc'
    takeSingleKeyExpr obj =
       [text| (take ["a1"] $obj) |]

    keysToTakeArgs = zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         map (createPactExpr takeFirstExpr) intListsExpr
      <> map (createPactExpr takeLastExpr) intListsExpr
      <> map takeKeysExpr keysToTakeArgs
      <> map (createPactExpr takeSingleKeyExpr) strKeyIntValMapsExpr


strToIntTests :: NativeDefName -> GasUnitTests
strToIntTests = defGasUnitTests allExprs
  where
    str2intExpr valInt =
      PactExpression [text| (str-to-int $valStr) |] Nothing
        where valStr = escapeText $ intToStr valInt

    str2intLongHex = defPactExpression [text| (str-to-int 16 "186A0") |]
    str2intMedHex = defPactExpression [text| (str-to-int 16 "64") |]
    str2intSmallHex = defPactExpression [text| (str-to-int 16 "a") |]

    str2intLongBinary = defPactExpression [text| (str-to-int 2 "11000011010100000") |]
    str2intMedBinary = defPactExpression [text| (str-to-int 2 "1100100") |]
    str2intSmallBinary = defPactExpression [text| (str-to-int 2 "1010") |]

    str2intLongBase64 = defPactExpression [text| (str-to-int 64 "AYag") |]
    str2intMedBase64 = defPactExpression [text| (str-to-int 64 "ZA") |]
    str2intSmallBase64 = defPactExpression [text| (str-to-int 64 "Cg") |]

    allExprs = map (str2intExpr . snd) sizes
      <>  [ str2intLongHex,
            str2intMedHex,
            str2intSmallHex,

            str2intLongBinary,
            str2intMedBinary,
            str2intSmallBinary,

            str2intLongBase64,
            str2intMedBase64,
            str2intSmallBase64
          ]

base64EncodeTests :: NativeDefName -> GasUnitTests
base64EncodeTests = defGasUnitTests exprs
  where
    exprs = [fshort, fmedium, flong]

    f i =
      let s = toB64UrlUnpaddedText
            $ T.encodeUtf8
            $ T.replicate i "a"
      in defPactExpression [text| (base64-decode "$s") |]

    fshort = f 10
    fmedium = f 100
    flong = f 1000

base64DecodeTests :: NativeDefName -> GasUnitTests
base64DecodeTests = defGasUnitTests exprs
  where
    exprs = [fshort, fmedium, flong]

    f i =
      let s = T.replicate i "a"
      in defPactExpression [text| (base64-encode "$s") |]

    fshort = f 10
    fmedium = f 100
    flong = f 1000

distinctTests :: NativeDefName -> GasUnitTests
distinctTests = defGasUnitTests allExprs
  where
    distinctExpr li =
      [text| (distinct $li) |]

    allExprs = map (createPactExpr distinctExpr) duplicateListsExpr

sortTests :: NativeDefName -> GasUnitTests
sortTests = defGasUnitTests allExprs
  where
    sortListExpr li =
      [text| (sort $li) |]

    reversedListsExpr = map format intLists
      where
        format (desc, li) = PactExpression
                            (reversedListExpr li)
                            (Just $ desc <> "NumberList")
        reversedListExpr li =
          toText $ MockList $ map MockInt (reverse li)

    allExprs = map (createPactExpr sortListExpr) reversedListsExpr


reverseTests :: NativeDefName -> GasUnitTests
reverseTests = defGasUnitTests allExprs
  where
    reverseExpr li =
      [text| (reverse $li) |]

    allExprs = map (createPactExpr reverseExpr) intListsExpr


removeTests :: NativeDefName -> GasUnitTests
removeTests = defGasUnitTests allExprs
  where
    removeExpr obj =
      [text| (remove "a1" $obj) |]

    allExprs = map (createPactExpr removeExpr) strKeyIntValMapsExpr


pactIdTests :: NativeDefName -> GasUnitTests
pactIdTests = tests
  where
    pactIdExpr = defPactExpression [text|(pact-id)|]

    tests =
      createGasUnitTests updateWithPactExec updateWithPactExec [pactIdExpr]


yieldTests :: NativeDefName -> GasUnitTests
yieldTests = tests
  where
    yieldExpr obj = [text| (yield $obj) |]
    yieldExprWithTargetChain obj = [text| (yield $obj "some-chain-id") |]

    mockModules = HM.fromList [(someModuleName, someModuleData)]
    mockStackframe = [someStackFrame]
    updateStateWithStackFrame = setState (set evalCallStack mockStackframe)
    updateStateWithPactExec = setState (set evalPactExec mockPactExec)
    setInitialState = setState $ const (initStateModules mockModules)


    allUpdatesForNoChain =
      updateStateWithPactExec
    allExprsNoChain =
      map (createPactExpr yieldExpr) strKeyIntValMapsExpr
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
      map (createPactExpr yieldExprWithTargetChain) strKeyIntValMapsExpr
    testsWithChain =
      createGasUnitTests
      allUpdatesForChain
      allUpdatesForChain
      allExprsWithChain


    tests = testsWithNoChain <> testsWithChain


resumeTests :: NativeDefName -> GasUnitTests
resumeTests nativeName = tests
  where
    resumeExprText binding = [text|(resume $binding a1)|]

    addProvenanceDesc (PactExpression expr desc) =
      PactExpression
      expr
      (over _Just (<> " with provenance") desc)

    args :: [((HM.HashMap T.Text Integer), PactExpression)]
    args = map (\((_,m),b) -> (m,
                                   createPactExpr resumeExprText b))
           $ zip strKeyIntValMaps strKeyIntValBindingsExpr

    toSPVTests ::
      (HM.HashMap T.Text Integer, PactExpression)
      -> GasUnitTests
    toSPVTests (yieldMap, expr)
      = createGasUnitTests
        (setupForResume True yieldMap)
        (setupForResume True yieldMap)
        [expr]
        nativeName

    toNonSPVTests ::
      (HM.HashMap T.Text Integer, PactExpression)
      -> GasUnitTests
    toNonSPVTests (yieldMap, expr)
      = createGasUnitTests
        (setupForResume False yieldMap)
        (setupForResume False yieldMap)
        [addProvenanceDesc expr]
        nativeName

    tests = concatGasUnitTests $
            map toSPVTests args <>
            map toNonSPVTests args

    setupForResume
      :: Bool
      -> HM.HashMap T.Text Integer
      -> (GasSetup e -> GasSetup e)
    setupForResume isProv yielded = allUpdates
      where
        allUpdates
          = bool
            ( updateEnvWithYield )    -- No provenance setup needed
            ( updateStateWithStackFrame .
              updateEnvWithChaindId .
              updateEnvWithYield .
              setInitialState
            )
            isProv

        updateEnvWithYield =
          setEnv (set eePactStep pactStep)
        updateEnvWithChaindId
          = setEnv (set (eePublicData . pdPublicMeta . pmChainId) chainIdTest)
        updateStateWithStackFrame
          = setState (set evalCallStack [someStackFrame])
        setInitialState = setState $ const (initStateModules mockModules)

        mockModules
          = HM.fromList [(someModuleName, someModuleData)]
        pactStep
          = Just $ PactStep 2 False (PactId "") (Just yieldVal)
        yieldVal
          = Yield yieldData provenance Nothing
        provenance
          = bool Nothing (Just $ Provenance chainIdTest someModuleHash) isProv
        chainIdTest
          = ChainId "some-chain-id"
        yieldData
          = (ObjectMap . M.fromList . toPactValueInt . HM.toList) yielded
        toPactValueInt
          = map (\(t,v) -> (FieldKey t, PLiteral $ LInteger v))


pactVersionTests :: NativeDefName -> GasUnitTests
pactVersionTests = defPactExpGasTest [text| (pact-version) |]

concatTests :: NativeDefName -> GasUnitTests
concatTests = defGasUnitTests allExprs
  where
    concatExpr arg =
      [text| (concat $arg) |]
    allExprs = map (createPactExpr concatExpr) escapedStrListsExpr

strToListTests :: NativeDefName -> GasUnitTests
strToListTests = defGasUnitTests allExprs
  where
    strToListExpr arg =
      [text| (str-to-list $arg) |]
    allExprs = map (createPactExpr strToListExpr) escapedStringsExpr

readStringTests :: NativeDefName -> GasUnitTests
readStringTests nativeName = tests
  where
    readStringExprText = [text|(read-string "name")|]

    readStringExpr desc =
      PactExpression
      readStringExprText
      (Just $ readStringExprText <>
              " with name=" <> desc <> "String")

    updateEnvWithData s =
      setEnv (set eeMsgBody $ A.object ["name" A..= s])

    setupTests (desc, s)
      = createGasUnitTests
        (updateEnvWithData s)
        (updateEnvWithData s)
        [readStringExpr desc]
        nativeName

    tests = concatGasUnitTests $
            map setupTests strings


readMsgTests :: NativeDefName -> GasUnitTests
readMsgTests nativeName = tests
  where
    readMsgExprText = [text|(read-msg)|]
    readMsgExpr desc =
      PactExpression
      readMsgExprText
      (Just $ readMsgExprText <>
              " with msg=" <> desc <> "ObjectMap")

    updateEnvWithData m =
      setEnv (set eeMsgBody $ toJSON m)

    setupTests (desc, m)
      = createGasUnitTests
        (updateEnvWithData m)
        (updateEnvWithData m)
        [readMsgExpr desc]
        nativeName

    tests = concatGasUnitTests $
            map setupTests strKeyIntValMaps


readIntegerTests :: NativeDefName -> GasUnitTests
readIntegerTests nativeName = tests
  where
    readIntExprText = [text|(read-integer "amount")|]
    readIntExpr desc =
      PactExpression
      readIntExprText
      (Just $ readIntExprText <>
              " with amount=" <> desc <> "Number")

    updateEnvWithData i =
      setEnv (set eeMsgBody $ A.object ["amount" A..= i])

    setupTests (desc, i)
      = createGasUnitTests
        (updateEnvWithData i)
        (updateEnvWithData i)
        [readIntExpr desc]
        nativeName

    tests = concatGasUnitTests $
            map setupTests sizes


readDecimalTests :: NativeDefName -> GasUnitTests
readDecimalTests nativeName = tests
  where
    readDecExprText = [text|(read-decimal "amount")|]
    readDecExpr desc =
      PactExpression
      readDecExprText
      (Just $ readDecExprText <>
              " with amount=" <> desc <> "Decimal")

    updateEnvWithData d =
      setEnv (set eeMsgBody decVal)
      where
        d' = "0." <> intToStr d <> "1"
        decVal = A.object ["amount" A..= d']

    setupTests (desc, d)
      = createGasUnitTests
        (updateEnvWithData d)
        (updateEnvWithData d)
        [readDecExpr desc]
        nativeName

    tests = concatGasUnitTests $
            map setupTests sizes


mapTests :: NativeDefName -> GasUnitTests
mapTests = defGasUnitTests allExprs
  where
    mapExpr li =
      [text| (map (identity) $li) |]
    allExprs = map (createPactExpr mapExpr) intListsExpr

zipTests :: NativeDefName -> GasUnitTests
zipTests = defGasUnitTests allExprs
  where
    zipExpr li =
      [text| (zip (+) $li $li) |]
    allExprs = map (createPactExpr zipExpr) intListsExpr

makeListTests :: NativeDefName -> GasUnitTests
makeListTests = defGasUnitTests allExprs
  where
    makeListExpr len =
      [text| (make-list $len true) |]

    allExprs = map (createPactExpr makeListExpr) sizesExpr

enumerateTests :: NativeDefName -> GasUnitTests
enumerateTests = defGasUnitTests allExprs
  where
    enumerateExpr arg =
      [text| (enumerate 1 $arg) |]
    allExprs = map (createPactExpr enumerateExpr) sizesExpr

listModulesTests :: NativeDefName -> GasUnitTests
listModulesTests = defPactExpGasTest [text| (list-modules) |]


lengthTests :: NativeDefName -> GasUnitTests
lengthTests = defGasUnitTests allExprs
  where
    lengthExpr t =
      [text| (length $t) |]

    allExprs =
         map (createPactExpr lengthExpr) intListsExpr
      <> map (createPactExpr lengthExpr) escapedStringsExpr
      <> map (createPactExpr lengthExpr) strKeyIntValMapsExpr

isCharsetTests :: NativeDefName -> GasUnitTests
isCharsetTests = defGasUnitTests allExprs
  where
    isCharsetExprAscii =
      defPactExpression [text|(is-charset CHARSET_ASCII "hello world")|]
    isCharsetExprNotAscii =
      defPactExpression [text|(is-charset CHARSET_ASCII "I am nÖt ascii")|]
    isCharsetExprLatin1 =
      defPactExpression [text|(is-charset CHARSET_LATIN1 "I am nÖt ascii, but I am latin1!")|]
    allExprs = [ isCharsetExprAscii
               , isCharsetExprNotAscii
               , isCharsetExprLatin1
               ]


intToStrTests :: NativeDefName -> GasUnitTests
intToStrTests = defGasUnitTests allExprs
  where
    int2strExpr (valInt,baseInt) =
      defPactExpression [text| (int-to-str $base $val) |]
      where base = intToStr baseInt
            val = intToStr valInt

    baseList = 64:[2..16]
    -- TODO is foldr1 the best thing to do here
    -- | Test every base conversion against three different number sizes
    args = F.foldr1 (<>) $ map (\(_, n) -> map (\b -> (n,b)) baseList) sizes

    allExprs = map (int2strExpr) args


ifTests :: NativeDefName -> GasUnitTests
ifTests = defPactExpGasTest
    [text| (if true "then-clause" "else-clause") |]


identityTests :: NativeDefName -> GasUnitTests
identityTests = defGasUnitTests allExprs
  where
    identityExpr val =
      [text| (identity $val) |]

    allExprs = map (createPactExpr identityExpr) intListsExpr


hashTests :: NativeDefName -> GasUnitTests
hashTests = defGasUnitTests allExprs
  where
    hashExpr val =
      [text| (hash $val) |]

    allExprs =
         map (createPactExpr hashExpr) escapedStringsExpr
      <> map (createPactExpr hashExpr) strKeyIntValMapsExpr

formatTests :: NativeDefName -> GasUnitTests
formatTests = defGasUnitTests allExprs
  where
    formatExpr (str,(PactExpression expr desc')) =
      PactExpression
      [text| (format "$str" $expr )|]
      (Just [text| (format "{}...{}" $desc)|])
      where desc = fromMaybe expr desc'

    curlyBraces =
      map
      (\(_,n) -> T.unwords $ replicate (fromIntegral n) "{}")
      sizes
    strListArgs = zip curlyBraces escapedStrListsExpr
    intListArgs = zip curlyBraces intListsExpr

    allExprs =
         map formatExpr strListArgs
      <> map formatExpr intListArgs


foldTests :: NativeDefName -> GasUnitTests
foldTests = defGasUnitTests allExprs
  where
    foldExpr li =
      [text| (fold (constantly 0) 1 $li) |]
    allExprs = map (createPactExpr foldExpr) intListsExpr


filterTests :: NativeDefName -> GasUnitTests
filterTests = defGasUnitTests allExprs
  where
    filterExpr li =
      [text| (filter (constantly true) $li)|]
    allExprs = map (createPactExpr filterExpr) intListsExpr


enforceOneTests :: NativeDefName -> GasUnitTests
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
      = map
        (\(desc,n) -> (desc, (replicate (fromIntegral n) enforceFail) <> [enforcePass]))
        sizes
    listOfEnforcesListExpr
      = map
        (\(desc, li) -> PactExpression (toText (MockList li)) (Just $ desc <> "EnforceList"))
        listOfEnforcesList

    allExprs
      = map (createPactExpr enforceOneExpr) listOfEnforcesListExpr


enforcePactVersionTests :: NativeDefName -> GasUnitTests
enforcePactVersionTests = defPactExpGasTest
    [text| (enforce-pact-version "3.0")|]


-- TODO: Unable to currently test when enforce's
--       predicate function returns false.
enforceTests :: NativeDefName -> GasUnitTests
enforceTests = defPactExpGasTest
    [text| (enforce true "some-error-message")|]


dropTests :: NativeDefName -> GasUnitTests
dropTests = defGasUnitTests allExprs
  where
    dropFirstExpr t =
      [text| (drop 1 $t) |]
    dropLastExpr t =
      [text| (drop -1 $t) |]
    dropKeysExpr (PactExpression keyList keyListDesc', PactExpression obj objDesc') =
      PactExpression
      [text| (drop $keyList $obj) |]
      (Just $ [text| (drop $keyListDesc $objDesc) |])
      where keyListDesc = fromMaybe keyList keyListDesc'
            objDesc = fromMaybe obj objDesc'
    dropSingleKeyExpr obj =
      [text| (drop ["a1"] $obj) |]

    keysToDropArgs = zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         map (createPactExpr dropFirstExpr) intListsExpr
      <> map (createPactExpr dropLastExpr) intListsExpr
      <> map dropKeysExpr keysToDropArgs
      <> map (createPactExpr dropSingleKeyExpr) strKeyIntValMapsExpr


namespaceTests :: NativeDefName -> GasUnitTests
namespaceTests = tests
  where
    namespaceExpr = defPactExpression [text| (namespace '$sampleNamespaceName) |]

    updateEnvWithSig =
      setEnv $ set eeMsgSigs (M.fromList $ F.toList samplePubKeysWithCaps)

    tests = createGasUnitTests
            updateEnvWithSig
            updateEnvWithSig
            [namespaceExpr]


defineNamespaceTests :: NativeDefName -> GasUnitTests
defineNamespaceTests = tests
  where
    tests = simpleDefTests <> rotateNamespaceTests

    simpleDefTests = defGasUnitTests [simpleDefExpr]
      where
        simpleDefExpr =
          defPactExpression [text| (define-namespace 'some-other-namespace $sampleLoadedKeysetName $sampleLoadedKeysetName) |]

    rotateNamespaceTests = rotateTests
      where
        rotateExprText = [text| (define-namespace '$sampleNamespaceName $sampleLoadedKeysetName $sampleLoadedKeysetName) |]
        rotateExpr =
          PactExpression
          rotateExprText
          (Just $ rotateExprText <> ": Defining namespace with the same name as one already defined.")

        updateMsgSig =
          setEnv $ set eeMsgSigs (M.fromList $ F.toList samplePubKeysWithCaps)

        rotateTests =
          createGasUnitTests
          (updateMsgSig)
          (updateMsgSig)
          [rotateExpr]


containsTests :: NativeDefName -> GasUnitTests
containsTests = defGasUnitTests allExprs
  where
    containsListExpr (PactExpression val valDesc', PactExpression li liDesc') =
      PactExpression
      [text| (contains $val $li) |]
      (Just [text| (contains $valDesc $liDesc) |])
      where valDesc = fromMaybe val valDesc'
            liDesc = fromMaybe li liDesc'

    containsObjExpr obj =
      [text| (contains "a1" $obj) |]

    containsStrExpr (PactExpression val valDesc', PactExpression str strDesc') =
      PactExpression
      [text| (contains "a$val" $str) |]
      (Just [text| (contains "a$valDesc" $strDesc) |])
      where valDesc = fromMaybe val valDesc'
            strDesc = fromMaybe str strDesc'

    listArgs = zip sizesExpr intListsExpr
    strArgs = zip sizesExpr escapedStringsExpr

    allExprs =
         map containsListExpr listArgs
      <> map (createPactExpr containsObjExpr) strKeyIntValMapsExpr
      <> map containsStrExpr strArgs


constantlyTests :: NativeDefName -> GasUnitTests
constantlyTests = defGasUnitTests allExprs
  where
    singleIgnoreExpr =
      defPactExpression [text| (constantly 0 "firstIgnore") |]
    doubleIgnoreExpr =
      defPactExpression [text| (constantly 0 "firstIgnore" "secondIgnore") |]
    tripleIgnoreExpr =
      defPactExpression [text| (constantly 0 "firstIgnore" "secondIgnore" "thirdIgnore") |]
    allExprs =
      [singleIgnoreExpr, doubleIgnoreExpr, tripleIgnoreExpr]


composeTests :: NativeDefName -> GasUnitTests
composeTests = defPactExpGasTest
    [text| (compose (+ 0) (+ 0) 0) |]


chainDataTests :: NativeDefName -> GasUnitTests
chainDataTests = defPactExpGasTest [text| (chain-data) |]


atTests :: NativeDefName -> GasUnitTests
atTests = defGasUnitTests allExprs
  where
    atListExpr (PactExpression idx _, PactExpression li liDesc') =
      PactExpression
      [text| (at $idx $li) |]
      (Just [text| (at $idx $liDesc) |])
      where liDesc = fromMaybe li liDesc'

    atObjExpr obj =
      [text| (at "a1" $obj) |]

    listIndices = map
                  (\(desc,i) -> (PactExpression (toText $ MockInt $ pred i) (Just desc)))
                  sizes
    listArgs = zip listIndices escapedStrListsExpr

    allExprs = map atListExpr listArgs
      <> map (createPactExpr atObjExpr) strKeyIntValMapsExpr

bindTests :: NativeDefName -> GasUnitTests
bindTests = defGasUnitTests allExprs
  where
    bindExprText obj bind = [text| (bind $obj $bind a1) |]
    bindExpr (PactExpression obj objDesc', PactExpression binding bindingDesc') =
      PactExpression
      (bindExprText obj binding)
      (Just $ bindExprText objDesc bindingDesc)
      where objDesc = fromMaybe obj objDesc'
            bindingDesc = fromMaybe binding bindingDesc'

    args = zip
           strKeyIntValMapsExpr
           strKeyIntValBindingsExpr

    allExprs = map bindExpr args

createPrincipalTests :: NativeDefName -> GasUnitTests
createPrincipalTests = createGasUnitTests
    (updateWithPactExec . updateStackFrame . updateEnv)
    (updateWithPactExec . updateStackFrame . updateEnv)
    allExprs
  where
    allExprs =
      [ kExpr
      , wExpr
      , pExpr
      , rExpr
      , uExpr
      , mExpr
      ]

    kExpr = defPactExpression
      [text| (create-principal (read-keyset 'ks1)) |]


    wExpr = defPactExpression
      [text| (create-principal (read-keyset 'ks2)) |]

    pExpr = defPactExpression
      [text| (create-principal (create-pact-guard "test")) |]

    rExpr = defPactExpression
      [text| (create-principal (keyset-ref-guard "$sampleLoadedKeysetName")) |]

    uExpr = defPactExpression
      [text| (create-principal (create-user-guard ($acctModuleNameText.enforce-true))) |]

    mExpr = defPactExpression
      [text| (create-principal (create-module-guard "test")) |]

    updateStackFrame = setState (set evalCallStack [someStackFrame])

    updateEnv = setEnv $ set eeMsgBody $ A.object
      [ "ks1" A..= A.object
        [ "keys" A..= ["76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      , "ks2" A..= A.object
        [ "keys" A..=
          [ "76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text
          , "011b1bb033d77f0ef7fe0c09f7b10ed91c7f432f6fdc1ba68acdc776fa53d99c" :: T.Text
          ]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      ]


validatePrincipalTests :: NativeDefName -> GasUnitTests
validatePrincipalTests = createGasUnitTests
    (updateWithPactExec . updateStackFrame . updateEnv)
    (updateWithPactExec . updateStackFrame . updateEnv)
    allExprs
  where
    allExprs =
      [ kExpr
      , wExpr
      , pExpr
      , rExpr
      , uExpr
      , mExpr
      ]

    kExpr = defPactExpression
      [text|
        (validate-principal
          (read-keyset 'ks1)
          (create-principal (read-keyset 'ks1))) |]


    wExpr = defPactExpression
      [text|
        (validate-principal
          (read-keyset 'ks2)
          (create-principal (read-keyset 'ks2))) |]

    pExpr = defPactExpression
      [text|
        (validate-principal
          (create-pact-guard "test")
          (create-principal (create-pact-guard "test"))) |]

    rExpr = defPactExpression
      [text|
        (validate-principal
          (keyset-ref-guard "$sampleLoadedKeysetName")
          (create-principal (keyset-ref-guard "$sampleLoadedKeysetName"))) |]

    uExpr = defPactExpression
      [text|
        (validate-principal
          (create-user-guard ($acctModuleNameText.enforce-true))
          (create-principal (create-user-guard ($acctModuleNameText.enforce-true)))) |]

    mExpr = defPactExpression
      [text|
        (validate-principal
          (create-module-guard "test")
          (create-principal (create-module-guard "test"))) |]

    updateStackFrame = setState (set evalCallStack [someStackFrame])

    updateEnv = setEnv $ set eeMsgBody $ A.object
      [ "ks1" A..= A.object
        [ "keys" A..= ["76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      , "ks2" A..= A.object
        [ "keys" A..=
          [ "76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text
          , "011b1bb033d77f0ef7fe0c09f7b10ed91c7f432f6fdc1ba68acdc776fa53d99c" :: T.Text
          ]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      ]

isPrincipalTests :: NativeDefName -> GasUnitTests
isPrincipalTests = createGasUnitTests
    (updateWithPactExec . updateStackFrame . updateEnv)
    (updateWithPactExec . updateStackFrame . updateEnv)
    allExprs
  where
    allExprs =
      [ kExpr
      , wExpr
      , pExpr
      , rExpr
      , uExpr
      , mExpr
      ]

    kExpr = defPactExpression
      [text| (is-principal (create-principal (read-keyset 'ks1))) |]


    wExpr = defPactExpression
      [text| (is-principal (create-principal (read-keyset 'ks2))) |]

    pExpr = defPactExpression
      [text| (is-principal (create-principal (create-pact-guard "test"))) |]

    rExpr = defPactExpression
      [text| (is-principal (create-principal (keyset-ref-guard "$sampleLoadedKeysetName"))) |]

    uExpr = defPactExpression
      [text| (is-principal (create-principal (create-user-guard ($acctModuleNameText.enforce-true)))) |]

    mExpr = defPactExpression
      [text| (is-principal (create-principal (create-module-guard "test"))) |]


    updateStackFrame = setState (set evalCallStack [someStackFrame])

    updateEnv = setEnv $ set eeMsgBody $ A.object
      [ "ks1" A..= A.object
        [ "keys" A..= ["76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      , "ks2" A..= A.object
        [ "keys" A..=
          [ "76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text
          , "011b1bb033d77f0ef7fe0c09f7b10ed91c7f432f6fdc1ba68acdc776fa53d99c" :: T.Text
          ]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      ]


typeofPrincipalTests :: NativeDefName -> GasUnitTests
typeofPrincipalTests = createGasUnitTests
    (updateWithPactExec . updateStackFrame . updateEnv)
    (updateWithPactExec . updateStackFrame . updateEnv)
    allExprs
  where
    allExprs =
      [ kExpr
      , wExpr
      , pExpr
      , rExpr
      , uExpr
      , mExpr
      ]

    kExpr = defPactExpression
      [text| (typeof-principal (create-principal (read-keyset 'ks1))) |]


    wExpr = defPactExpression
      [text| (typeof-principal (create-principal (read-keyset 'ks2))) |]

    pExpr = defPactExpression
      [text| (typeof-principal (create-principal (create-pact-guard "test"))) |]

    rExpr = defPactExpression
      [text| (typeof-principal (create-principal (keyset-ref-guard "$sampleLoadedKeysetName"))) |]

    uExpr = defPactExpression
      [text| (typeof-principal (create-principal (create-user-guard ($acctModuleNameText.enforce-true)))) |]

    mExpr = defPactExpression
      [text| (typeof-principal (create-principal (create-module-guard "test"))) |]

    updateStackFrame = setState (set evalCallStack [someStackFrame])

    updateEnv = setEnv $ set eeMsgBody $ A.object
      [ "ks1" A..= A.object
        [ "keys" A..= ["76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      , "ks2" A..= A.object
        [ "keys" A..=
          [ "76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text
          , "011b1bb033d77f0ef7fe0c09f7b10ed91c7f432f6fdc1ba68acdc776fa53d99c" :: T.Text
          ]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      ]

scalarMulTests :: NativeDefName -> GasUnitTests
scalarMulTests = defGasUnitTests allExprs
  where
  scalarMulG1 = [text| (scalar-mult 'g1 {'x:1, 'y:2} 10) |]
  scalarMulG2 = [text| (scalar-mult 'g2
    { 'x: [10857046999023057135944570762232829481370756359578518086990519993285655852781, 11559732032986387107991004021392285783925812861821192530917403151452391805634]
    , 'y: [8495653923123431417604973247489272438418190587263600148770280649306958101930, 4082367875863433681332203403145435568316851327593401208105741076214120093531]}
    10)|]
  allExprs = fmap defPactExpression [scalarMulG1, scalarMulG2]


pointAddTests :: NativeDefName -> GasUnitTests
pointAddTests = defGasUnitTests allExprs
  where
  pointAddG1 = [text| (point-add 'g1 {'x:1, 'y:2} {'x:1, 'y:2}) |]
  pointAddG2 = [text| (point-add 'g2
    { 'x: [10857046999023057135944570762232829481370756359578518086990519993285655852781, 11559732032986387107991004021392285783925812861821192530917403151452391805634]
    , 'y: [8495653923123431417604973247489272438418190587263600148770280649306958101930, 4082367875863433681332203403145435568316851327593401208105741076214120093531]}
    { 'x: [10857046999023057135944570762232829481370756359578518086990519993285655852781, 11559732032986387107991004021392285783925812861821192530917403151452391805634]
    , 'y: [8495653923123431417604973247489272438418190587263600148770280649306958101930, 4082367875863433681332203403145435568316851327593401208105741076214120093531]})|]
  allExprs = fmap defPactExpression [pointAddG1, pointAddG2]

pairingCheckTests :: NativeDefName -> GasUnitTests
pairingCheckTests = defGasUnitTests allExprs
  where
  pairingCheck = [text| (pairing-check [{'x:1, 'y:2}]
    [{ 'x: [10857046999023057135944570762232829481370756359578518086990519993285655852781, 11559732032986387107991004021392285783925812861821192530917403151452391805634]
    , 'y: [8495653923123431417604973247489272438418190587263600148770280649306958101930, 4082367875863433681332203403145435568316851327593401208105741076214120093531]}]
    )|]
  allExprs = fmap defPactExpression [pairingCheck]

egcdTests :: NativeDefName -> GasUnitTests
egcdTests = defGasUnitTest $ PactExpression egcdExprText Nothing
  where
    egcdExprText = [text|
    (egcd 3 26)
    (egcd 5865413254 646787313212)
    (egcd 98765432109876543212 12345678901234567896)
    |]

keccak256bsTests :: NativeDefName -> GasUnitTests
keccak256bsTests = defGasUnitTest $ PactExpression keccak256bsExprText Nothing
  where
    keccak256bsExprText = [text|
    (keccak256-bs 64 4637928374822348932)
    (keccak256-bs 128 86734239273823482392374839238192)
    (keccak256-bs 256 2939802230983298498274024970323894828329382938283938293283)
    |]

poseidonTests:: NativeDefName -> GasUnitTests
poseidonTests = defGasUnitTest $ PactExpression poseidonExprText Nothing
  where
    poseidonExprText = [text|
    (poseidon-hash 1 2)
    (poseidon-hash 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999 88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888)
    |]