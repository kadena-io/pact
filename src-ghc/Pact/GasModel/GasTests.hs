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
import Data.List.NonEmpty (NonEmpty(..))
import NeatInterpolation (trimming)


import qualified Data.Aeson as A
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.List.NonEmpty as NEL
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
unitTestFromDef nativeName = tests
  where
    tests = case (asString nativeName) of
      -- General native functions
      "at"                   -> Just $ atTests nativeName
      "base64-decode"        -> Just $ base64DecodeTests nativeName
      "base64-encode"        -> Just $ base64EncodeTests nativeName
      "bind"                 -> Just $ bindTests nativeName
      "chain-data"           -> Just $ chainDataTests nativeName
      "compose"              -> Just $ composeTests nativeName
      "constantly"           -> Just $ constantlyTests nativeName
      "contains"             -> Just $ containsTests nativeName
      "define-namespace"     -> Just $ defineNamespaceTests nativeName
      "drop"                 -> Just $ dropTests nativeName
      "enforce"              -> Just $ enforceTests nativeName
      "enforce-one"          -> Just $ enforceOneTests nativeName
      "enforce-pact-version" -> Just $ enforcePactVersionTests nativeName
      "filter"               -> Just $ filterTests nativeName
      "fold"                 -> Just $ foldTests nativeName
      "format"               -> Just $ formatTests nativeName
      "hash"                 -> Just $ hashTests nativeName
      "identity"             -> Just $ identityTests nativeName
      "if"                   -> Just $ ifTests nativeName
      "int-to-str"           -> Just $ intToStrTests nativeName
      "is-charset"           -> Just $ isCharsetTests nativeName
      "length"               -> Just $ lengthTests nativeName
      "list-modules"         -> Just $ listModulesTests nativeName
      "make-list"            -> Just $ makeListTests nativeName
      "map"                  -> Just $ mapTests nativeName
      "namespace"            -> Just $ namespaceTests nativeName
      "pact-id"              -> Just $ pactIdTests nativeName
      "pact-version"         -> Just $ pactVersionTests nativeName
      "read-decimal"         -> Just $ readDecimalTests nativeName
      "read-integer"         -> Just $ readIntegerTests nativeName
      "read-msg"             -> Just $ readMsgTests nativeName
      "read-string"          -> Just $ readStringTests nativeName
      "remove"               -> Just $ removeTests nativeName
      "resume"               -> Just $ resumeTests nativeName
      "reverse"              -> Just $ reverseTests nativeName
      "sort"                 -> Just $ sortTests nativeName
      "str-to-int"           -> Just $ strToIntTests nativeName
      "take"                 -> Just $ takeTests nativeName
      "try"                  -> Just $ tryTests nativeName
      "tx-hash"              -> Just $ txHashTests nativeName
      "typeof"               -> Just $ typeOfTests nativeName
      "where"                -> Just $ whereTests nativeName
      "yield"                -> Just $ yieldTests nativeName

      -- Operators native functions
      "!="      -> Just $ notEqualOptTests nativeName
      "&"       -> Just $ bitwiseOptTests nativeName
      "*"       -> Just $ multOptTests nativeName
      "+"       -> Just $ addOptTests nativeName
      "-"       -> Just $ subOptTests nativeName
      "/"       -> Just $ divOptTests nativeName
      "<"       -> Just $ lessThanOptTests nativeName
      "<="      -> Just $ lessThanEqualOptTests nativeName
      "="       -> Just $ equalOptTests nativeName
      ">"       -> Just $ greaterThanOptTests nativeName
      ">="      -> Just $ greaterThanEqOptTests nativeName
      "^"       -> Just $ raiseOptTests nativeName
      "abs"     -> Just $ absOptTests nativeName
      "and"     -> Just $ andOptTests nativeName
      "and?"    -> Just $ andFuncOptTests nativeName
      "ceiling" -> Just $ ceilingOptTests nativeName
      "exp"     -> Just $ expOptTests nativeName
      "floor"   -> Just $ floorOptTests nativeName
      "ln"      -> Just $ lnOptTests nativeName
      "log"     -> Just $ logOptTests nativeName
      "mod"     -> Just $ modOptTests nativeName
      "not"     -> Just $ notOptTests nativeName
      "not?"    -> Just $ notFuncOptTests nativeName
      "or"      -> Just $ orOptTests nativeName
      "or?"     -> Just $ orFuncOptTests nativeName
      "round"   -> Just $ roundOptTests nativeName
      "shift"   -> Just $ shiftOptTests nativeName
      "sqrt"    -> Just $ sqrtOptTests nativeName
      "xor"     -> Just $ xorOptTests nativeName
      "|"       -> Just $ bitwiseOrOptTests nativeName
      "~"       -> Just $ reverseBitsOptTests nativeName

      -- Time native functions
      "add-time"    -> Just $ addTimeTests nativeName
      "days"        -> Just $ daysTests nativeName
      "diff-time"   -> Just $ diffTimeTests nativeName
      "format-time" -> Just $ formatTimeTests nativeName
      "hours"       -> Just $ hoursTests nativeName
      "minutes"     -> Just $ minutesTests nativeName
      "parse-time"  -> Just $ parseTimeTests nativeName
      "time"        -> Just $ timeTests nativeName

      -- Commitments native functions
      "decrypt-cc20p1305" -> Just $ decryptCc20p1305Tests nativeName
      "validate-keypair"  -> Just $ validateKeypairTests nativeName

      -- Keyset native functions
      "define-keyset"  -> Just $ defineKeysetTests nativeName
      "enforce-keyset" -> Just $ enforceKeysetTests nativeName
      "keys-2"         -> Just $ keys2Tests nativeName
      "keys-all"       -> Just $ keysAllTests nativeName
      "keys-any"       -> Just $ keysAnyTests nativeName
      "read-keyset"    -> Just $ readKeysetTests nativeName

      -- Database native functions
      "create-table"      -> Just $ createTableTests nativeName
      "describe-keyset"   -> Just $ describeKeysetTests nativeName
      "describe-module"   -> Just $ describeModuleTests nativeName
      "describe-table"    -> Just $ describeTableTests nativeName
      "insert"            -> Just $ insertTests nativeName
      "keylog"            -> Just $ keylogTests nativeName
      "keys"              -> Just $ keysTests nativeName
      "read"              -> Just $ readTests nativeName
      "select"            -> Just $ selectTests nativeName
      "txids"             -> Just $ txidsTests nativeName
      "txlog"             -> Just $ txlogTests nativeName
      "update"            -> Just $ updateTests nativeName
      "with-default-read" -> Just $ withDefaultReadTests nativeName
      "with-read"         -> Just $ withReadTests nativeName
      "write"             -> Just $ writeTests nativeName

      -- Capabilities native functions
      "compose-capability"  -> Just $ composeCapabilityTests nativeName
      "create-module-guard" -> Just $ createModuleGuardTests nativeName
      "create-pact-guard"   -> Just $ createPactGuardTests nativeName
      "create-user-guard"   -> Just $ createUserGuardTests nativeName
      "enforce-guard"       -> Just $ enforceGuardTests nativeName
      "install-capability"  -> Just $ installCapabilityTests nativeName
      "keyset-ref-guard"    -> Just $ keysetRefGuardTests nativeName
      "require-capability"  -> Just $ requireCapabilityTests nativeName
      "with-capability"     -> Just $ withCapabilityTests nativeName

      -- Non-native concepts to benchmark
      "use"       -> Just $ useTests nativeName
      "module"    -> Just $ moduleTests nativeName
      "interface" -> Just $ interfaceTests nativeName

      _ -> Nothing


-- | Non-native concepts tests
interfaceTests :: NativeDefName -> GasUnitTests
interfaceTests = defGasUnitTests allExprs
  where
    interfaceExprText = [trimming|
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

    interfaceExpr = PactExpression interfaceExprText Nothing
    allExprs = interfaceExpr :| []


moduleTests :: NativeDefName -> GasUnitTests
moduleTests = defGasUnitTests allExprs
  where
    moduleExprText = [trimming|
    (module some-random-module GOV
      (defcap GOV ()
        true ))|]
    moduleExpr = defPactExpression moduleExprText
    moduleRotateDesc = [trimming|(module accounts GOV [...some module code ...]) update module|]
    moduleRotateExpr = PactExpression (accountsModule acctModuleName) (Just moduleRotateDesc)
    allExprs = moduleExpr :| [moduleRotateExpr]


useTests :: NativeDefName -> GasUnitTests
useTests = defGasUnitTests allExprs
  where
    useExpr = defPactExpression [trimming| (use $acctModuleNameText) |]
    allExprs = useExpr :| []


-- | Capabilities native function tests
enforceGuardTests :: NativeDefName -> GasUnitTests
enforceGuardTests = tests
  where
    enforceGuardExpr = defPactExpression
      [trimming| (enforce-guard "$sampleLoadedKeysetName") |]
    allExprs = enforceGuardExpr :| []

    signEnvWithKeyset = setEnv (set eeMsgSigs (M.fromList $ F.toList samplePubKeysWithCaps))

    tests =
      createGasUnitTests
      signEnvWithKeyset
      signEnvWithKeyset
      allExprs


keysetRefGuardTests :: NativeDefName -> GasUnitTests
keysetRefGuardTests = defGasUnitTests allExprs
  where
    keysetRefGuardExpr =
      defPactExpression [trimming| (keyset-ref-guard "$sampleLoadedKeysetName") |]
    allExprs = keysetRefGuardExpr :| []


createUserGuardTests :: NativeDefName -> GasUnitTests
createUserGuardTests = defGasUnitTests allExprs
  where
    createUserGuardExpr =
      defPactExpression
      [trimming| (create-user-guard ($acctModuleNameText.enforce-true)) |]
    allExprs = createUserGuardExpr :| []


createPactGuardTests :: NativeDefName -> GasUnitTests
createPactGuardTests = tests
  where
    createPactGuardExpr =
      defPactExpression [trimming| (create-pact-guard "test") |]
    allExprs = createPactGuardExpr :| []

    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name $ BareName "some-defpact-func" def) [])
                          False
    updateWithPactExec = setState (set evalPactExec mockPactExec)

    tests =
      createGasUnitTests
      updateWithPactExec
      updateWithPactExec
      allExprs


createModuleGuardTests :: NativeDefName -> GasUnitTests
createModuleGuardTests = tests
  where
    createModuleGuardExpr = PactExpression [trimming| (create-module-guard "test") |] Nothing
    allExprs = createModuleGuardExpr :| []

    updateStackFrame = setState (set evalCallStack [someStackFrame])

    tests =
      createGasUnitTests
      updateStackFrame
      updateStackFrame
      allExprs


installCapabilityTests :: NativeDefName -> GasUnitTests
installCapabilityTests = defGasUnitTests allExprs
  where
    installCapExpr =
      defPactExpression [trimming|(install-capability ($acctModuleNameText.MANAGEDCAP "" ""))|]
    allExprs = installCapExpr :| []


withCapabilityTests :: NativeDefName -> GasUnitTests
withCapabilityTests = defGasUnitTests allExprs
  where
    withCapExpr =
      defPactExpression [trimming| ($acctModuleNameText.test-with-cap-func) |]
    allExprs = withCapExpr :| []


requireCapabilityTests :: NativeDefName -> GasUnitTests
requireCapabilityTests = tests
  where
    requireCapExpr =
      defPactExpression [trimming| (require-capability ($acctModuleNameText.GOV)) |]
    allExprs = requireCapExpr :| []

    cap = SigCapability (QualifiedName acctModuleName "GOV" def) []
    capSlot = CapSlot CapCallStack cap []
    updateGrantedCap = setState (set (evalCapabilities . capStack) [capSlot])

    tests =
      createGasUnitTests
      updateGrantedCap
      updateGrantedCap
      allExprs

composeCapabilityTests :: NativeDefName -> GasUnitTests
composeCapabilityTests = tests
  where
    composeCapExpr =
      defPactExpression [trimming| (compose-capability ($acctModuleNameText.GOV)) |]
    allExprs = composeCapExpr :| []

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
      allExprs


-- | Database native function tests
--   NOTE: Using MockDb means that database insert/write/update always succeed
txlogTests :: NativeDefName -> GasUnitTests
txlogTests = defGasUnitTests allExprs
  where
    txLogExpr =
      defPactExpression [trimming| (txlog $acctModuleNameText.accounts 0) |]
    allExprs = txLogExpr :| []

txidsTests :: NativeDefName -> GasUnitTests
txidsTests = defGasUnitTests allExprs
  where
    txIdsExpr =
      defPactExpression [trimming| (txids $acctModuleNameText.accounts 0) |]
    allExprs = txIdsExpr :| []


keylogTests :: NativeDefName -> GasUnitTests
keylogTests = defGasUnitTests allExprs
  where
    keyLogExpr =
      defPactExpression [trimming| (keylog $acctModuleNameText.accounts "someId" 0) |]
    allExprs = keyLogExpr :| []


keysTests :: NativeDefName -> GasUnitTests
keysTests = defGasUnitTests allExprs
  where
    keysExprs =
      defPactExpression [trimming| (keys $acctModuleNameText.accounts) |]
    allExprs = keysExprs :| []


selectTests :: NativeDefName -> GasUnitTests
selectTests = defGasUnitTests allExprs
  where
    selectExpr =
      defPactExpression
      [trimming| (select $acctModuleNameText.accounts
               (where "balance" (constantly true))
             ) |]
    allExprs = selectExpr :| []


withReadTests :: NativeDefName -> GasUnitTests
withReadTests = defGasUnitTests allExprs
  where
    withReadExpr =
      defPactExpression [trimming| (with-read
                $acctModuleNameText.accounts
                "someId"
                { "balance":= bal }
                bal
             )
      |]
    allExprs = withReadExpr :| []


withDefaultReadTests :: NativeDefName -> GasUnitTests
withDefaultReadTests = defGasUnitTests allExprs
  where
    withDefReadExpr =
      defPactExpression [trimming| (with-default-read
                $acctModuleNameText.accounts
                "someId"
                { "balance": 1.0 }
                { "balance":= bal }
                bal
             )
      |]
    allExprs = withDefReadExpr :| []


readTests :: NativeDefName -> GasUnitTests
readTests = defGasUnitTests allExprs
  where
    readExpr =
      defPactExpression [trimming| (read $acctModuleNameText.accounts "someId") |]
    allExprs = readExpr :| []


writeTests :: NativeDefName -> GasUnitTests
writeTests = defGasUnitTests allExprs
  where
    writeExpr =
      defPactExpression [trimming| (write $acctModuleNameText.accounts
                    "some-id-that-is-not-present"
                    { "balance": 0.0 }
             ) |]
    allExprs = writeExpr :| []


updateTests :: NativeDefName -> GasUnitTests
updateTests = defGasUnitTests allExprs
  where
    updateExpr =
      defPactExpression [trimming| (update $acctModuleNameText.accounts
                     "someId"
                     { "balance": 10.0 }
             ) |]
    allExprs = updateExpr :| []


insertTests :: NativeDefName -> GasUnitTests
insertTests = defGasUnitTests allExprs
  where
    insertExpr =
      defPactExpression [trimming| (insert $acctModuleNameText.accounts
                     "some-id-that-is-not-present"
                     { "balance": 0.0 }
             )|]
    allExprs = insertExpr :| []


describeTableTests :: NativeDefName -> GasUnitTests
describeTableTests = defGasUnitTests allExprs
  where
    describeTableExpr =
      defPactExpression [trimming| (describe-table $acctModuleNameText.accounts) |]
    allExprs = describeTableExpr :| []


describeModuleTests :: NativeDefName -> GasUnitTests
describeModuleTests = defGasUnitTests allExprs
  where
    describeModuleExpr =
      defPactExpression [trimming| (describe-module "$acctModuleNameText") |]
    allExprs = describeModuleExpr :| []


describeKeysetTests :: NativeDefName -> GasUnitTests
describeKeysetTests = defGasUnitTests allExprs
  where
    describeKeysetExpr =
      defPactExpression [trimming| (describe-keyset "$sampleLoadedKeysetName") |]
    allExprs = describeKeysetExpr :| []


createTableTests :: NativeDefName -> GasUnitTests
createTableTests = defGasUnitTests allExprs
  where
    createTableExpr =
      defPactExpression
      [trimming| (create-table $acctModuleNameText.accounts-for-testing-table-creation) |]
    allExprs = createTableExpr :| []


-- | Keyset native function tests
defineKeysetTests :: NativeDefName -> GasUnitTests
defineKeysetTests = tests
  where
    simpleExpr =
      defPactExpression
      [trimming| (define-keyset "some-keyset-name-not-present-already" $sampleLoadedKeysetName) |]
    rotateExprText = [trimming| (define-keyset "$sampleLoadedKeysetName" $sampleLoadedKeysetName) |]
    rotateExpr = PactExpression rotateExprText (Just $ rotateExprText <> ": rotating keyset")
    allExprs = rotateExpr :| [simpleExpr]

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
    enforceKeysetExpr = defPactExpression [trimming| (enforce-keyset '$sampleLoadedKeysetName) |]
    allExprs = enforceKeysetExpr :| []

    updateEnvMsgSig = setEnv (set eeMsgSigs (M.fromList $ F.toList samplePubKeysWithCaps))

    tests =
      createGasUnitTests
      updateEnvMsgSig
      updateEnvMsgSig
      allExprs


readKeysetTests :: NativeDefName -> GasUnitTests
readKeysetTests = tests
  where
    readKeysetExpr = defPactExpression [trimming| (read-keyset 'my-keyset) |]
    allExprs = readKeysetExpr :| []

    dataWithKeyset = toPactKeyset "my-keyset" "something" Nothing
    updateMsgBodyWithKeyset = setEnv (set eeMsgBody dataWithKeyset)

    tests =
      createGasUnitTests
      updateMsgBodyWithKeyset
      updateMsgBodyWithKeyset
      allExprs


keysAnyTests :: NativeDefName -> GasUnitTests
keysAnyTests = defGasUnitTests allExprs
  where
    keysAnyExpr = defPactExpression [trimming|(keys-any 10 1)|]
    allExprs = keysAnyExpr :| []


keysAllTests :: NativeDefName -> GasUnitTests
keysAllTests = defGasUnitTests allExprs
  where
    keysAllExpr = defPactExpression [trimming|(keys-all 3 3)|]
    allExprs = keysAllExpr :| []


keys2Tests :: NativeDefName -> GasUnitTests
keys2Tests = defGasUnitTests allExprs
  where
    keys2Expr = defPactExpression [trimming|(keys-2 3 1)|]
    allExprs = keys2Expr :| []


-- | Commitments native function tests
decryptCc20p1305Tests :: NativeDefName -> GasUnitTests
decryptCc20p1305Tests = defGasUnitTests allExprs
  where
    decryptExpr =
      defPactExpression [trimming| (decrypt-cc20p1305
              "Zi1REj5-iA"
              "AAAAAAECAwQFBgcI"
              "YWFk"
              "FYP6lG7xq7aExvoaHIH8Jg"
              "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
              "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
      |]
    allExprs = decryptExpr :| []


validateKeypairTests :: NativeDefName -> GasUnitTests
validateKeypairTests = defGasUnitTests allExprs
  where
    validateExpr =
      defPactExpression [trimming| (validate-keypair
             "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
             "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a") |]
    allExprs = validateExpr :| []


-- | Time native function tests
addTimeTests :: NativeDefName -> GasUnitTests
addTimeTests = defGasUnitTests allExprs
  where
    addTimeExpr =
      defPactExpression [trimming| (add-time (time "2016-07-22T12:00:00Z") 15) |]
    allExprs = addTimeExpr :| []


daysTests :: NativeDefName -> GasUnitTests
daysTests = defGasUnitTests allExprs
  where
    daysExprText n = [trimming| (days $n) |]
    allExprs = NEL.map (createPactExpr daysExprText) sizesExpr


diffTimeTests :: NativeDefName -> GasUnitTests
diffTimeTests = defGasUnitTests allExprs
  where
    diffTime =
      defPactExpression [trimming| (diff-time (time "2016-07-22T12:00:00Z")
                        (time "2018-07-22T12:00:00Z"))
      |]
    allExprs = diffTime :| []


formatTimeTests :: NativeDefName -> GasUnitTests
formatTimeTests = defGasUnitTests allExprs
  where
    formatTimeSimpleExpr =
      defPactExpression [trimming| (format-time "%F" (time "2016-07-22T12:00:00Z")) |]
    formatTimeComplexExpr =
      defPactExpression [trimming| (format-time "%Y-%m-%dT%H:%M:%S%N" (time "2016-07-23T13:30:45Z")) |]

    allExprs = formatTimeSimpleExpr :|
               [formatTimeComplexExpr]


hoursTests :: NativeDefName -> GasUnitTests
hoursTests = defGasUnitTests allExprs
  where
    hoursExprText n = [trimming| (hours $n) |]
    allExprs = NEL.map (createPactExpr hoursExprText) sizesExpr


minutesTests :: NativeDefName -> GasUnitTests
minutesTests = defGasUnitTests allExprs
  where
    minutesExpr n =
      [trimming| (minutes $n) |]
    allExprs = NEL.map (createPactExpr minutesExpr) sizesExpr


parseTimeTests :: NativeDefName -> GasUnitTests
parseTimeTests = defGasUnitTests allExprs
  where
    parseTimeSimpleExpr =
      defPactExpression [trimming| (parse-time "%F" "2016-07-22") |]
    parseTimeComplexExpr =
      defPactExpression [trimming| (parse-time "%Y-%m-%dT%H:%M:%S%N" "2016-07-23T13:30:45+00:00") |]
    allExprs =  parseTimeSimpleExpr :|
               [parseTimeComplexExpr]


timeTests :: NativeDefName -> GasUnitTests
timeTests = defGasUnitTests allExprs
  where
    timeExpr =
      defPactExpression [trimming| (time "2016-07-22T12:00:00Z") |]
    allExprs = timeExpr :| []


-- | Operators native function tests
reverseBitsOptTests :: NativeDefName -> GasUnitTests
reverseBitsOptTests = defGasUnitTests allExprs
  where
    reverseBitsExpr x = [trimming| (~ $x) |]

    allExprs = NEL.map (createPactExpr reverseBitsExpr) sizesExpr


bitwiseOrOptTests :: NativeDefName -> GasUnitTests
bitwiseOrOptTests = defGasUnitTests allExprs
  where
    bitwiseOrExpr x = [trimming| (| 2 $x) |]

    allExprs = NEL.map (createPactExpr bitwiseOrExpr) sizesExpr


xorOptTests :: NativeDefName -> GasUnitTests
xorOptTests = defGasUnitTests allExprs
  where
    xorExpr x = [trimming| (xor 2 $x) |]

    allExprs = NEL.map (createPactExpr xorExpr) sizesExpr


sqrtOptTests :: NativeDefName -> GasUnitTests
sqrtOptTests = defGasUnitTests allExprs
  where
    sqrtExpr x = [trimming| (sqrt $x) |]
    sqrtDecimalExpr x = [trimming| (sqrt $x.1) |]

    allExprs = NEL.map (createPactExpr sqrtExpr) sizesExpr
      <> NEL.map (createPactExpr sqrtDecimalExpr) sizesExpr


shiftOptTests :: NativeDefName -> GasUnitTests
shiftOptTests = defGasUnitTests allExprs
  where
    shiftExpr x =
      [trimming| (shift 2 $x) |]
    shiftNegExpr x =
      [trimming| (shift -2 $x) |]

    allExprs = NEL.map (createPactExpr shiftExpr) sizesExpr
      <> NEL.map (createPactExpr shiftNegExpr) sizesExpr


roundOptTests :: NativeDefName -> GasUnitTests
roundOptTests = defGasUnitTests allExprs
  where
    roundExpr x =
      [trimming| (round $x.12345) |]
    roundPrecExpr x =
      [trimming| (round $x.12345 4) |]

    allExprs = NEL.map (createPactExpr roundExpr) sizesExpr
      <> NEL.map (createPactExpr roundPrecExpr) sizesExpr


orFuncOptTests :: NativeDefName -> GasUnitTests
orFuncOptTests = defGasUnitTests allExprs
  where
    orFuncExpr = defPactExpression [trimming| (or? (identity) (identity) true) |]

    allExprs = orFuncExpr :| []


orOptTests :: NativeDefName -> GasUnitTests
orOptTests = defGasUnitTests allExprs
  where
    orExpr = defPactExpression [trimming| (or false false) |]

    allExprs = orExpr :| []


notFuncOptTests :: NativeDefName -> GasUnitTests
notFuncOptTests = defGasUnitTests allExprs
  where
    notFuncExpr = defPactExpression [trimming| (not? (identity) true) |]

    allExprs = notFuncExpr :| []


notOptTests :: NativeDefName -> GasUnitTests
notOptTests = defGasUnitTests allExprs
  where
    notExpr = defPactExpression [trimming| (not true) |]

    allExprs = notExpr :| []


modOptTests :: NativeDefName -> GasUnitTests
modOptTests = defGasUnitTests allExprs
  where
    modExpr x =
      [trimming| (mod $x 2) |]

    allExprs = NEL.map (createPactExpr modExpr) sizesExpr



logOptTests :: NativeDefName -> GasUnitTests
logOptTests = defGasUnitTests allExprs
  where
    logExpr y =
      [trimming| (log 2 $y) |]
    logDecimalExpr y =
      [trimming| (log 2 $y.1) |]

    allExprs = NEL.map (createPactExpr logExpr) sizesExpr
      <> NEL.map (createPactExpr logDecimalExpr) sizesExpr



lnOptTests :: NativeDefName -> GasUnitTests
lnOptTests = defGasUnitTests allExprs
  where
    lnExpr x =
      [trimming| (ln $x) |]
    lnDecimalExpr x =
      [trimming| (ln $x.1) |]

    allExprs = NEL.map (createPactExpr lnExpr) sizesExpr
      <> NEL.map (createPactExpr lnDecimalExpr) sizesExpr


floorOptTests :: NativeDefName -> GasUnitTests
floorOptTests = defGasUnitTests allExprs
  where
    floorExpr x =
      [trimming| (floor $x.12345) |]
    floorPrecExpr x =
      [trimming| (floor $x.12345 4) |]

    allExprs = NEL.map (createPactExpr floorExpr) sizesExpr
      <> NEL.map (createPactExpr floorPrecExpr) sizesExpr


expOptTests :: NativeDefName -> GasUnitTests
expOptTests = defGasUnitTests allExprs
  where
    expExprSmall =
      defPactExpression [trimming| (exp 1) |]
    expExprMed =
      defPactExpression [trimming| (exp 10) |]
    expExprLarge =
      defPactExpression [trimming| (exp 100) |]

    allExprs =  expExprSmall :|
               [expExprMed
               ,expExprLarge]


ceilingOptTests :: NativeDefName -> GasUnitTests
ceilingOptTests = defGasUnitTests allExprs
  where
    ceilingExpr x =
      [trimming| (ceiling $x.12345) |]
    ceilingPrecExpr x =
      [trimming| (ceiling $x.12345 4) |]

    allExprs = NEL.map (createPactExpr ceilingExpr) sizesExpr
      <> NEL.map (createPactExpr ceilingPrecExpr) sizesExpr


andFuncOptTests :: NativeDefName -> GasUnitTests
andFuncOptTests = defGasUnitTests allExprs
  where
    andFuncExpr =
      defPactExpression [trimming| (and? (identity) (identity) true) |]

    allExprs = andFuncExpr :| []


andOptTests :: NativeDefName -> GasUnitTests
andOptTests = defGasUnitTests allExprs
  where
    andExpr =
      defPactExpression [trimming| (and false true) |]

    allExprs = andExpr :| []


absOptTests :: NativeDefName -> GasUnitTests
absOptTests = defGasUnitTests allExprs
  where
    absExpr x =
      [trimming| (abs -$x) |]
    absDecimalExpr x =
      [trimming| (abs -$x.0) |]

    allExprs = NEL.map (createPactExpr absExpr) sizesExpr
      <> NEL.map (createPactExpr absDecimalExpr) sizesExpr


raiseOptTests :: NativeDefName -> GasUnitTests
raiseOptTests = defGasUnitTests allExprs
  where
    raiseExpr y =
      [trimming| (^ 2 $y) |]
    raiseDecimalExpr y =
      [trimming| (^ 2.1 $y.1) |]
    raiseBothExpr y =
      [trimming| (^ 2.1 $y) |]

    allExprs = NEL.map (createPactExpr raiseExpr) sizesExpr
      <> NEL.map (createPactExpr raiseDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr raiseBothExpr) sizesExpr


greaterThanEqOptTests :: NativeDefName -> GasUnitTests
greaterThanEqOptTests = defGasUnitTests allExprs
  where
    greaterEqExpr x =
      [trimming| (>= $x $x) |]
    greaterEqDecimalExpr x =
      [trimming| (>= $x.0 $x.0) |]
    greaterEqTimeExpr =
      [trimming| (>= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = NEL.map (createPactExpr greaterEqExpr) sizesExpr
      <> NEL.map (createPactExpr greaterEqDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr greaterEqExpr) escapedStringsExpr
      <> ((defPactExpression greaterEqTimeExpr) :| [])


greaterThanOptTests :: NativeDefName -> GasUnitTests
greaterThanOptTests = defGasUnitTests allExprs
  where
    greaterExpr x =
      [trimming| (> $x $x) |]
    greaterDecimalExpr x =
      [trimming| (> $x.0 $x.0) |]
    greaterTimeExpr =
      [trimming| (> (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = NEL.map (createPactExpr greaterExpr) sizesExpr
      <> NEL.map (createPactExpr greaterDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr greaterExpr) escapedStringsExpr
      <> ((defPactExpression greaterTimeExpr) :| [])


equalOptTests :: NativeDefName -> GasUnitTests
equalOptTests = defGasUnitTests allExprs
  where
    eqExpr x =
      [trimming| (= $x $x) |]
    eqDecimalExpr x =
      [trimming| (= $x.0 $x.0) |]
    eqTimeExpr =
      [trimming| (= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = NEL.map (createPactExpr eqExpr) sizesExpr
      <> NEL.map (createPactExpr eqDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr eqExpr) escapedStringsExpr
      <> NEL.map (createPactExpr eqExpr) strKeyIntValMapsExpr
      <> NEL.map (createPactExpr eqExpr) intListsExpr
      <> ((defPactExpression eqTimeExpr) :| [])


lessThanEqualOptTests :: NativeDefName -> GasUnitTests
lessThanEqualOptTests = defGasUnitTests allExprs
  where
    lessEqExpr x =
      [trimming| (<= $x $x) |]
    lessEqDecimalExpr x =
      [trimming| (<= $x.0 $x.0) |]
    lessEqTimeExpr =
      [trimming| (<= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = NEL.map (createPactExpr lessEqExpr) sizesExpr
      <> NEL.map (createPactExpr lessEqDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr lessEqExpr) escapedStringsExpr
      <> ((defPactExpression lessEqTimeExpr) :| [])


lessThanOptTests :: NativeDefName -> GasUnitTests
lessThanOptTests = defGasUnitTests allExprs
  where
    lessExpr x =
      [trimming| (< $x $x) |]
    lessDecimalExpr x =
      [trimming| (< $x.0 $x.0) |]
    lessTimeExpr =
      [trimming| (< (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = NEL.map (createPactExpr lessExpr) sizesExpr
      <> NEL.map (createPactExpr lessDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr lessExpr) escapedStringsExpr
      <> ((defPactExpression lessTimeExpr) :| [])


divOptTests :: NativeDefName -> GasUnitTests
divOptTests = defGasUnitTests allExprs
  where
    divExpr x =
      [trimming| (/ $x $x) |]
    divDecimalExpr x =
      [trimming| (/ $x.0 $x.0) |]
    divBothExpr x =
      [trimming| (/ $x.0 $x) |]

    allExprs = NEL.map (createPactExpr divExpr) sizesExpr
      <> NEL.map (createPactExpr divDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr divBothExpr) sizesExpr


subOptTests :: NativeDefName -> GasUnitTests
subOptTests = defGasUnitTests allExprs
  where
    subExpr x =
      [trimming| (- $x $x) |]
    subDecimalExpr x =
      [trimming| (- $x.0 $x.0) |]
    subBothExpr x =
      [trimming| (- $x.0 $x) |]
    subOneExpr x =
      [trimming| (- $x) |]
    subOneDecimalExpr x =
      [trimming| (- $x.0) |]

    allExprs = NEL.map (createPactExpr subExpr) sizesExpr
      <> NEL.map (createPactExpr subDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr subBothExpr) sizesExpr
      <> NEL.map (createPactExpr subOneExpr) sizesExpr
      <> NEL.map (createPactExpr subOneDecimalExpr) sizesExpr


addOptTests :: NativeDefName -> GasUnitTests
addOptTests = defGasUnitTests allExprs
  where
    addExpr x =
      [trimming| (+ $x $x) |]
    addDecimalExpr x =
      [trimming| (+ $x.0 $x.0) |]
    addBothExpr x =
      [trimming| (+ $x.0 $x) |]

    allExprs = NEL.map (createPactExpr addExpr) sizesExpr
      <> NEL.map (createPactExpr addDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr addBothExpr) sizesExpr
      <> NEL.map (createPactExpr addExpr) escapedStringsExpr
      <> NEL.map (createPactExpr addExpr) strKeyIntValMapsExpr


multOptTests :: NativeDefName -> GasUnitTests
multOptTests = defGasUnitTests allExprs
  where
    multIntExpr x =
      [trimming| (* $x $x) |]
    multDecimalExpr x =
      [trimming| (* $x.0 $x.0) |]
    multBothExpr x =
      [trimming| (* $x.0 $x) |]

    allExprs = NEL.map (createPactExpr multIntExpr) sizesExpr
      <> NEL.map (createPactExpr multDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr multBothExpr) sizesExpr


bitwiseOptTests :: NativeDefName -> GasUnitTests
bitwiseOptTests = defGasUnitTests allExprs
  where
    bitwiseExpr x =
      [trimming| (& $x $x) |]

    allExprs = NEL.map (createPactExpr bitwiseExpr) sizesExpr


notEqualOptTests :: NativeDefName -> GasUnitTests
notEqualOptTests = defGasUnitTests allExprs
  where
    notEqualExpr x =
      [trimming| (!= $x $x) |]
    notEqualDecimalExpr x =
      [trimming| (!= $x.0 $x.0) |]

    allExprs = NEL.map (createPactExpr notEqualExpr) sizesExpr
      <> NEL.map (createPactExpr notEqualExpr) escapedStringsExpr
      <> NEL.map (createPactExpr notEqualDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr notEqualExpr) intListsExpr
      <> NEL.map (createPactExpr notEqualExpr) strKeyIntValMapsExpr



-- | General native function tests
whereTests :: NativeDefName -> GasUnitTests
whereTests = defGasUnitTests allExprs
  where
    whereExpr obj =
      [trimming| (where "a1" (constantly true) $obj) |]

    allExprs = NEL.map (createPactExpr whereExpr) strKeyIntValMapsExpr


typeOfTests :: NativeDefName -> GasUnitTests
typeOfTests = defGasUnitTests allExprs
  where
    typeOfExpr t =
      [trimming| (typeof $t) |]

    allExprs = NEL.map (createPactExpr typeOfExpr) strKeyIntValMapsExpr
      <> NEL.map (createPactExpr typeOfExpr) escapedStringsExpr
      <> NEL.map (createPactExpr typeOfExpr) intListsExpr
      <> NEL.map (createPactExpr typeOfExpr) sizesExpr


txHashTests :: NativeDefName -> GasUnitTests
txHashTests = defGasUnitTests allExprs
  where
    txHashExpr = defPactExpression [trimming| (tx-hash) |]
    allExprs = txHashExpr :| []


tryTests :: NativeDefName -> GasUnitTests
tryTests = defGasUnitTests allExprs
  where
    tryPassExpr =
      defPactExpression [trimming| (try true (enforce true "this will definitely pass")) |]
    tryFailExpr =
      defPactExpression [trimming| (try true (enforce false "this will definitely fail")) |]

    allExprs = tryPassExpr :| [ tryFailExpr ]


takeTests :: NativeDefName -> GasUnitTests
takeTests = defGasUnitTests allExprs
  where
    takeFirstExpr t =
       [trimming| (take 1 $t) |]
    takeLastExpr t =
       [trimming| (take -1 $t) |]
    takeKeysExpr (PactExpression keyList keyDesc', PactExpression obj objDesc') =
      PactExpression
      [trimming| (take $keyList $obj) |]
      (Just [trimming| (take $keyListDesc $objDesc) |])
      where keyListDesc = fromMaybe keyList keyDesc'
            objDesc = fromMaybe obj objDesc'
    takeSingleKeyExpr obj =
       [trimming| (take ["a1"] $obj) |]

    keysToTakeArgs = NEL.zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         NEL.map (createPactExpr takeFirstExpr) intListsExpr
      <> NEL.map (createPactExpr takeLastExpr) intListsExpr
      <> NEL.map takeKeysExpr keysToTakeArgs
      <> NEL.map (createPactExpr takeSingleKeyExpr) strKeyIntValMapsExpr


strToIntTests :: NativeDefName -> GasUnitTests
strToIntTests = defGasUnitTests allExprs
  where
    str2intExpr valInt =
      PactExpression [trimming| (str-to-int $valStr) |] Nothing
        where valStr = escapeText $ intToStr valInt

    str2intLongHex = defPactExpression [trimming| (str-to-int 16 "186A0") |]
    str2intMedHex = defPactExpression [trimming| (str-to-int 16 "64") |]
    str2intSmallHex = defPactExpression [trimming| (str-to-int 16 "a") |]

    str2intLongBinary = defPactExpression [trimming| (str-to-int 2 "11000011010100000") |]
    str2intMedBinary = defPactExpression [trimming| (str-to-int 2 "1100100") |]
    str2intSmallBinary = defPactExpression [trimming| (str-to-int 2 "1010") |]

    str2intLongBase64 = defPactExpression [trimming| (str-to-int 64 "AYag") |]
    str2intMedBase64 = defPactExpression [trimming| (str-to-int 64 "ZA") |]
    str2intSmallBase64 = defPactExpression [trimming| (str-to-int 64 "Cg") |]

    allExprs = NEL.map (str2intExpr . snd) sizes
      <> (  str2intLongHex :|
          [ str2intMedHex,
            str2intSmallHex,

            str2intLongBinary,
            str2intMedBinary,
            str2intSmallBinary,

            str2intLongBase64,
            str2intMedBase64,
            str2intSmallBase64
          ])

base64EncodeTests :: NativeDefName -> GasUnitTests
base64EncodeTests = defGasUnitTests exprs
  where
    exprs = NEL.fromList [fshort, fmedium, flong]

    f i =
      let s = toB64UrlUnpaddedText
            $ T.encodeUtf8
            $ T.replicate i "a"
      in defPactExpression [trimming| (base64-decode "$s") |]

    fshort = f 10
    fmedium = f 100
    flong = f 1000

base64DecodeTests :: NativeDefName -> GasUnitTests
base64DecodeTests = defGasUnitTests exprs
  where
    exprs = NEL.fromList [fshort, fmedium, flong]

    f i =
      let s = T.replicate i "a"
      in defPactExpression [trimming| (base64-encode "$s") |]

    fshort = f 10
    fmedium = f 100
    flong = f 1000


sortTests :: NativeDefName -> GasUnitTests
sortTests = defGasUnitTests allExprs
  where
    sortListExpr li =
      [trimming| (sort $li) |]

    reversedListsExpr = NEL.map format intLists
      where
        format (desc, li) = PactExpression
                            (reversedListExpr li)
                            (Just $ desc <> "NumberList")
        reversedListExpr li =
          toText $ MockList $ map MockInt (reverse $ NEL.toList li)

    allExprs = NEL.map (createPactExpr sortListExpr) reversedListsExpr


reverseTests :: NativeDefName -> GasUnitTests
reverseTests = defGasUnitTests allExprs
  where
    reverseExpr li =
      [trimming| (reverse $li) |]

    allExprs = NEL.map (createPactExpr reverseExpr) intListsExpr


removeTests :: NativeDefName -> GasUnitTests
removeTests = defGasUnitTests allExprs
  where
    removeExpr obj =
      [trimming| (remove "a1" $obj) |]

    allExprs = NEL.map (createPactExpr removeExpr) strKeyIntValMapsExpr


pactIdTests :: NativeDefName -> GasUnitTests
pactIdTests = tests
  where
    pactIdExpr = defPactExpression [trimming|(pact-id)|]

    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name $ BareName "some-defpact-func" def) [])
                          False
    updateState = setState (set evalPactExec mockPactExec)

    tests =
      createGasUnitTests updateState updateState (pactIdExpr :| [])


yieldTests :: NativeDefName -> GasUnitTests
yieldTests = tests
  where
    yieldExpr obj = [trimming| (yield $obj) |]
    yieldExprWithTargetChain obj = [trimming| (yield $obj "some-chain-id") |]

    mockPactExec = Just $ PactExec 2 Nothing Nothing 0
                          (PactId "somePactId")
                          (PactContinuation (Name $ BareName "some-defpact-func" def) [])
                          False

    mockModules = HM.fromList [(someModuleName, someModuleData)]
    mockStackframe = [someStackFrame]
    updateStateWithStackFrame = setState (set evalCallStack mockStackframe)
    updateStateWithPactExec = setState (set evalPactExec mockPactExec)
    setInitialState = setState $ const (initStateModules mockModules)


    allUpdatesForNoChain =
      updateStateWithPactExec
    allExprsNoChain =
      NEL.map (createPactExpr yieldExpr) strKeyIntValMapsExpr
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
      NEL.map (createPactExpr yieldExprWithTargetChain) strKeyIntValMapsExpr
    testsWithChain =
      createGasUnitTests
      allUpdatesForChain
      allUpdatesForChain
      allExprsWithChain


    tests = testsWithNoChain <> testsWithChain


resumeTests :: NativeDefName -> GasUnitTests
resumeTests nativeName = tests
  where
    resumeExprText binding = [trimming|(resume $binding a1)|]

    addProvenanceDesc (PactExpression expr desc) =
      PactExpression
      expr
      (over _Just (<> " with provenance") desc)

    args :: NEL.NonEmpty ((HM.HashMap T.Text Integer), PactExpression)
    args = NEL.map (\((_,m),b) -> (m,
                                   createPactExpr resumeExprText b))
           $ NEL.zip strKeyIntValMaps strKeyIntValBindingsExpr

    toSPVTests ::
      (HM.HashMap T.Text Integer, PactExpression)
      -> GasUnitTests
    toSPVTests (yieldMap, expr)
      = createGasUnitTests
        (setupForResume True yieldMap)
        (setupForResume True yieldMap)
        (expr :| [])
        nativeName

    toNonSPVTests ::
      (HM.HashMap T.Text Integer, PactExpression)
      -> GasUnitTests
    toNonSPVTests (yieldMap, expr)
      = createGasUnitTests
        (setupForResume False yieldMap)
        (setupForResume False yieldMap)
        ((addProvenanceDesc expr) :| [])
        nativeName

    tests = concatGasUnitTests $
            NEL.map toSPVTests args <>
            NEL.map toNonSPVTests args

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
          = Yield yieldData provenance
        provenance
          = bool Nothing (Just $ Provenance chainIdTest someModuleHash) isProv
        chainIdTest
          = ChainId "some-chain-id"
        yieldData
          = (ObjectMap . M.fromList . toPactValueInt . HM.toList) yielded
        toPactValueInt
          = map (\(t,v) -> (FieldKey t, PLiteral $ LInteger v))


pactVersionTests :: NativeDefName -> GasUnitTests
pactVersionTests = defGasUnitTests allExprs
  where
    versionExpr =
      defPactExpression [trimming| (pact-version) |]

    allExprs = versionExpr :| []


readStringTests :: NativeDefName -> GasUnitTests
readStringTests nativeName = tests
  where
    readStringExprText = [trimming|(read-string "name")|]

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
        ( (readStringExpr desc) :| [])
        nativeName

    tests = concatGasUnitTests $
            NEL.map setupTests strings


readMsgTests :: NativeDefName -> GasUnitTests
readMsgTests nativeName = tests
  where
    readMsgExprText = [trimming|(read-msg)|]
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
        ((readMsgExpr desc) :| [])
        nativeName

    tests = concatGasUnitTests $
            NEL.map setupTests strKeyIntValMaps


readIntegerTests :: NativeDefName -> GasUnitTests
readIntegerTests nativeName = tests
  where
    readIntExprText = [trimming|(read-integer "amount")|]
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
        ((readIntExpr desc) :| [])
        nativeName

    tests = concatGasUnitTests $
            NEL.map setupTests sizes


readDecimalTests :: NativeDefName -> GasUnitTests
readDecimalTests nativeName = tests
  where
    readDecExprText = [trimming|(read-decimal "amount")|]
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
        ((readDecExpr desc) :| [])
        nativeName

    tests = concatGasUnitTests $
            NEL.map setupTests sizes


mapTests :: NativeDefName -> GasUnitTests
mapTests = defGasUnitTests allExprs
  where
    mapExpr li =
      [trimming| (map (identity) $li) |]

    allExprs = NEL.map (createPactExpr mapExpr) intListsExpr


makeListTests :: NativeDefName -> GasUnitTests
makeListTests = defGasUnitTests allExprs
  where
    makeListExpr len =
      [trimming| (make-list $len true) |]

    allExprs = NEL.map (createPactExpr makeListExpr) sizesExpr


listModulesTests :: NativeDefName -> GasUnitTests
listModulesTests = defGasUnitTests allExprs
  where
    listModulesExpr =
      defPactExpression [trimming| (list-modules) |]

    allExprs = listModulesExpr :| []


lengthTests :: NativeDefName -> GasUnitTests
lengthTests = defGasUnitTests allExprs
  where
    lengthExpr t =
      [trimming| (length $t) |]

    allExprs =
         NEL.map (createPactExpr lengthExpr) intListsExpr
      <> NEL.map (createPactExpr lengthExpr) escapedStringsExpr
      <> NEL.map (createPactExpr lengthExpr) strKeyIntValMapsExpr

isCharsetTests :: NativeDefName -> GasUnitTests
isCharsetTests = defGasUnitTests allExprs
  where
    isCharsetExprAscii =
      defPactExpression [trimming|(is-charset CHARSET_ASCII "hello world")|]
    isCharsetExprNotAscii =
      defPactExpression [trimming|(is-charset CHARSET_ASCII "I am nÖt ascii")|]
    isCharsetExprLatin1 =
      defPactExpression [trimming|(is-charset CHARSET_LATIN1 "I am nÖt ascii, but I am latin1!")|]
    allExprs = isCharsetExprAscii :|
               [isCharsetExprNotAscii, isCharsetExprLatin1]


intToStrTests :: NativeDefName -> GasUnitTests
intToStrTests = defGasUnitTests allExprs
  where
    int2strExpr (valInt,baseInt) =
      defPactExpression [trimming| (int-to-str $base $val) |]
      where base = intToStr baseInt
            val = intToStr valInt

    baseList :: NonEmpty Integer
    baseList = 64 :| [2..16]
    -- TODO is foldr1 the best thing to do here
    -- | Test every base conversion against three different number sizes
    args = F.foldr1 (<>) $ NEL.map (\(_, n) -> NEL.map (\b -> (n,b)) baseList) sizes

    allExprs = NEL.map (int2strExpr) args


ifTests :: NativeDefName -> GasUnitTests
ifTests = defGasUnitTests allExprs
  where
    ifExpr =
      defPactExpression [trimming| (if true "then-clause" "else-clause") |]

    allExprs = ifExpr :| []


identityTests :: NativeDefName -> GasUnitTests
identityTests = defGasUnitTests allExprs
  where
    identityExpr val =
      [trimming| (identity $val) |]

    allExprs = NEL.map (createPactExpr identityExpr) intListsExpr


hashTests :: NativeDefName -> GasUnitTests
hashTests = defGasUnitTests allExprs
  where
    hashExpr val =
      [trimming| (hash $val) |]

    allExprs =
         NEL.map (createPactExpr hashExpr) escapedStringsExpr
      <> NEL.map (createPactExpr hashExpr) strKeyIntValMapsExpr

formatTests :: NativeDefName -> GasUnitTests
formatTests = defGasUnitTests allExprs
  where
    formatExpr (str,(PactExpression expr desc')) =
      PactExpression
      [trimming| (format "$str" $expr )|]
      (Just [trimming| (format "{}...{}" $desc)|])
      where desc = fromMaybe expr desc'

    curlyBraces =
      NEL.map
      (\(_,n) -> T.unwords $ replicate (fromIntegral n) "{}")
      sizes
    strListArgs = NEL.zip curlyBraces escapedStrListsExpr
    intListArgs = NEL.zip curlyBraces intListsExpr

    allExprs =
         NEL.map formatExpr strListArgs
      <> NEL.map formatExpr intListArgs


foldTests :: NativeDefName -> GasUnitTests
foldTests = defGasUnitTests allExprs
  where
    foldExpr li =
      [trimming| (fold (constantly 0) 1 $li) |]
    allExprs = NEL.map (createPactExpr foldExpr) intListsExpr


filterTests :: NativeDefName -> GasUnitTests
filterTests = defGasUnitTests allExprs
  where
    filterExpr li =
      [trimming| (filter (constantly true) $li)|]
    allExprs = NEL.map (createPactExpr filterExpr) intListsExpr


enforceOneTests :: NativeDefName -> GasUnitTests
enforceOneTests = defGasUnitTests allExprs
  where
    enforceOneExpr tests =
      [trimming| (enforce-one "some-error-message" $tests) |]

    enforcePass = MockExpr
      [trimming| (enforce true "this-should-always-succeed") |]

    enforceFail = MockExpr
      [trimming| (enforce false "skip me") |]

    -- | Lists of failing enforce statements with a passing one at the end
    --   example: [ [ (enforce-statement) (enforce-statement) ] ]
    listOfEnforcesList
      = NEL.map
        (\(desc,n) -> (desc, (replicate (fromIntegral n) enforceFail) <> [enforcePass]))
        sizes
    listOfEnforcesListExpr
      = NEL.map
        (\(desc, li) -> PactExpression (toText (MockList li)) (Just $ desc <> "EnforceList"))
        listOfEnforcesList

    allExprs
      = NEL.map (createPactExpr enforceOneExpr) listOfEnforcesListExpr


enforcePactVersionTests :: NativeDefName -> GasUnitTests
enforcePactVersionTests = defGasUnitTests allExprs
  where
    enforcePactVersionExpr =
      defPactExpression [trimming| (enforce-pact-version "3.0")|]
    allExprs = enforcePactVersionExpr :| []


-- TODO: Unable to currently test when enforce's
--       predicate function returns false.
enforceTests :: NativeDefName -> GasUnitTests
enforceTests = defGasUnitTests allExprs
  where
    allExprs =
      defPactExpression [trimming| (enforce true "some-error-message")|] :| []


dropTests :: NativeDefName -> GasUnitTests
dropTests = defGasUnitTests allExprs
  where
    dropFirstExpr t =
      [trimming| (drop 1 $t) |]
    dropLastExpr t =
      [trimming| (drop -1 $t) |]
    dropKeysExpr (PactExpression keyList keyListDesc', PactExpression obj objDesc') =
      PactExpression
      [trimming| (drop $keyList $obj) |]
      (Just $ [trimming| (drop $keyListDesc $objDesc) |])
      where keyListDesc = fromMaybe keyList keyListDesc'
            objDesc = fromMaybe obj objDesc'
    dropSingleKeyExpr obj =
      [trimming| (drop ["a1"] $obj) |]

    keysToDropArgs = NEL.zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         NEL.map (createPactExpr dropFirstExpr) intListsExpr
      <> NEL.map (createPactExpr dropLastExpr) intListsExpr
      <> NEL.map dropKeysExpr keysToDropArgs
      <> NEL.map (createPactExpr dropSingleKeyExpr) strKeyIntValMapsExpr


namespaceTests :: NativeDefName -> GasUnitTests
namespaceTests = tests
  where
    namespaceExpr = defPactExpression [trimming| (namespace '$sampleNamespaceName) |]

    updateEnvWithSig =
      setEnv $ set eeMsgSigs (M.fromList $ F.toList samplePubKeysWithCaps)

    tests = createGasUnitTests
            updateEnvWithSig
            updateEnvWithSig
            (namespaceExpr :| [])


defineNamespaceTests :: NativeDefName -> GasUnitTests
defineNamespaceTests = tests
  where
    tests = simpleDefTests <> rotateNamespaceTests

    simpleDefTests = defGasUnitTests (simpleDefExpr :| [])
      where
        simpleDefExpr =
          defPactExpression [trimming| (define-namespace 'some-other-namespace $sampleLoadedKeysetName $sampleLoadedKeysetName) |]

    rotateNamespaceTests = rotateTests
      where
        rotateExprText = [trimming| (define-namespace '$sampleNamespaceName $sampleLoadedKeysetName $sampleLoadedKeysetName) |]
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
          (rotateExpr :| [])


containsTests :: NativeDefName -> GasUnitTests
containsTests = defGasUnitTests allExprs
  where
    containsListExpr (PactExpression val valDesc', PactExpression li liDesc') =
      PactExpression
      [trimming| (contains $val $li) |]
      (Just [trimming| (contains $valDesc $liDesc) |])
      where valDesc = fromMaybe val valDesc'
            liDesc = fromMaybe li liDesc'

    containsObjExpr obj =
      [trimming| (contains "a1" $obj) |]

    containsStrExpr (PactExpression val valDesc', PactExpression str strDesc') =
      PactExpression
      [trimming| (contains "a$val" $str) |]
      (Just [trimming| (contains "a$valDesc" $strDesc) |])
      where valDesc = fromMaybe val valDesc'
            strDesc = fromMaybe str strDesc'

    listArgs = NEL.zip sizesExpr intListsExpr
    strArgs = NEL.zip sizesExpr escapedStringsExpr

    allExprs =
         NEL.map containsListExpr listArgs
      <> NEL.map (createPactExpr containsObjExpr) strKeyIntValMapsExpr
      <> NEL.map containsStrExpr strArgs


constantlyTests :: NativeDefName -> GasUnitTests
constantlyTests = defGasUnitTests allExprs
  where
    singleIgnoreExpr =
      defPactExpression [trimming| (constantly 0 "firstIgnore") |]
    doubleIgnoreExpr =
      defPactExpression [trimming| (constantly 0 "firstIgnore" "secondIgnore") |]
    tripleIgnoreExpr =
      defPactExpression [trimming| (constantly 0 "firstIgnore" "secondIgnore" "thirdIgnore") |]
    allExprs =
      singleIgnoreExpr :| [doubleIgnoreExpr, tripleIgnoreExpr]


composeTests :: NativeDefName -> GasUnitTests
composeTests = defGasUnitTests allExprs
  where
    composeExpr =
      defPactExpression [trimming| (compose (+ 0) (+ 0) 0) |]
    allExprs = composeExpr :| []


chainDataTests :: NativeDefName -> GasUnitTests
chainDataTests = defGasUnitTests allExprs
  where
    allExprs = (defPactExpression [trimming| (chain-data) |]) :| []


atTests :: NativeDefName -> GasUnitTests
atTests = defGasUnitTests allExprs
  where
    atListExpr (PactExpression idx _, PactExpression li liDesc') =
      PactExpression
      [trimming| (at $idx $li) |]
      (Just [trimming| (at $idx $liDesc) |])
      where liDesc = fromMaybe li liDesc'

    atObjExpr obj =
      [trimming| (at "a1" $obj) |]

    listIndices = NEL.map
                  (\(desc,i) -> (PactExpression (toText $ MockInt $ pred i) (Just desc)))
                  sizes
    listArgs = NEL.zip listIndices escapedStrListsExpr

    allExprs = NEL.map atListExpr listArgs
      <> NEL.map (createPactExpr atObjExpr) strKeyIntValMapsExpr

bindTests :: NativeDefName -> GasUnitTests
bindTests = defGasUnitTests allExprs
  where
    bindExprText obj bind = [trimming| (bind $obj $bind a1) |]
    bindExpr (PactExpression obj objDesc', PactExpression binding bindingDesc') =
      PactExpression
      (bindExprText obj binding)
      (Just $ bindExprText objDesc bindingDesc)
      where objDesc = fromMaybe obj objDesc'
            bindingDesc = fromMaybe binding bindingDesc'

    args = NEL.zip
           strKeyIntValMapsExpr
           strKeyIntValBindingsExpr

    allExprs = NEL.map bindExpr args
