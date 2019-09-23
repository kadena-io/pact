{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Pact.GasModel.GasTests
  (
    unitTests
  , allNatives
  , untestedNatives
  , unitTestFromDef

  ) where

import Control.Lens               hiding ((.=),DefName)
import Data.Aeson                 (toJSON, ToJSON(..))
import Data.Default               (def)
import Data.List                  (foldl')
import Data.Bool                  (bool)
import NeatInterpolation          (text)
import Data.List.NonEmpty         (NonEmpty(..))
import Data.Maybe                 (fromMaybe)


import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable       as F
import qualified Data.Set            as S
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.List.NonEmpty  as NEL


import Pact.GasModel.Utils
import Pact.Types.GasModel
import Pact.Native
import Pact.Types.Native
import Pact.Types.PactValue       (PactValue(..))

import Pact.Interpreter
import Pact.Types.Lang
import Pact.Types.Runtime



-- | Gas benchmark tests for Pact native functions
allNatives :: [NativeDef]
allNatives = concatMap snd natives

untestedNatives :: [NativeDefName]
untestedNatives = foldl' check [] allNatives
  where
    check li (nativeName,_) = case (HM.lookup nativeName unitTests) of
      Nothing -> nativeName : li
      Just _ -> li


unitTests :: HM.HashMap NativeDefName GasUnitTests
unitTests = HM.fromList $ foldl' getUnitTest [] allNatives 
  where
    getUnitTest li (nativeName,_) =
      case unitTestFromDef nativeName of
        Nothing -> li
        Just ts -> (nativeName, ts) : li
    


unitTestFromDef :: NativeDefName -> Maybe GasUnitTests
unitTestFromDef nativeName = tests
  where
    tests = case (asString nativeName) of
      -- General native functions
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

      -- Operators native functions
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

      -- Time native functions
      "add-time"    -> Just addTimeTests
      "days"        -> Just daysTests
      "diff-time"   -> Just diffTimeTests
      "format-time" -> Just formatTimeTests
      "hours"       -> Just hoursTests
      "minutes"     -> Just minutesTests
      "parse-time"  -> Just parseTimeTests
      "time"        -> Just timeTests

      -- Commitments native functions
      "decrypt-cc20p1305" -> Just decryptCc20p1305Tests
      "validate-keypair"  -> Just validateKeypairTests

      -- Keyset native functions
      "define-keyset"  -> Just defineKeysetTests
      "enforce-keyset" -> Just enforceKeysetTests
      "keys-2"         -> Just keys2Tests
      "keys-all"       -> Just keysAllTests
      "keys-any"       -> Just keysAnyTests
      "read-keyset"    -> Just readKeysetTests

      -- Database native functions
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

      -- Capabilities native functions
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
    enforceGuardExpr =
      PactExpression [text| (enforce-guard "$sampleLoadedKeysetName") |] Nothing
    allExprs = enforceGuardExpr :| []

    signEnvWithKeyset = setEnv (set eeMsgSigs (S.fromList samplePubKeys))

    tests =
      createGasUnitTests
      signEnvWithKeyset
      signEnvWithKeyset
      allExprs


keysetRefGuardTests :: GasUnitTests
keysetRefGuardTests = defGasUnitTests allExprs
  where
    keysetRefGuardExpr =
      PactExpression [text| (keyset-ref-guard "$sampleLoadedKeysetName") |] Nothing
    allExprs = keysetRefGuardExpr :| []


createUserGuardTests :: GasUnitTests
createUserGuardTests = defGasUnitTests allExprs
  where
    createUserGuardExpr =
      PactExpression
      [text| (create-user-guard ($acctModuleNameText.enforce-true)) |]
      Nothing
    allExprs = createUserGuardExpr :| []


createPactGuardTests :: GasUnitTests
createPactGuardTests = tests
  where
    createPactGuardExpr =
      PactExpression [text| (create-pact-guard "test") |] Nothing
    allExprs = createPactGuardExpr :| []

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
    createModuleGuardExpr = PactExpression [text| (create-module-guard "test") |] Nothing
    allExprs = createModuleGuardExpr :| []
    
    updateStackFrame = setState (set evalCallStack [someStackFrame])

    tests =
      createGasUnitTests
      updateStackFrame
      updateStackFrame
      allExprs


withCapabilityTests :: GasUnitTests
withCapabilityTests = defGasUnitTests allExprs
  where
    withCapExpr =
      PactExpression [text| ($acctModuleNameText.test-with-cap-func) |] Nothing
    allExprs = withCapExpr :| []
 

requireCapabilityTests :: GasUnitTests
requireCapabilityTests = tests
  where
    requireCapExpr =
      PactExpression [text| (require-capability ($acctModuleNameText.GOV)) |] Nothing
    allExprs = requireCapExpr :| []

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
    composeCapExpr =
      PactExpression [text| (compose-capability ($acctModuleNameText.GOV)) |] Nothing
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
txlogTests :: GasUnitTests
txlogTests = defGasUnitTests allExprs
  where
    txLogExpr =
      PactExpression [text| (txlog $acctModuleNameText.accounts 0) |] Nothing
    allExprs = txLogExpr :| []

txidsTests :: GasUnitTests
txidsTests = defGasUnitTests allExprs
  where
    txIdsExpr =
      PactExpression [text| (txids $acctModuleNameText.accounts 0) |] Nothing
    allExprs = txIdsExpr :| []
      

keylogTests :: GasUnitTests
keylogTests = defGasUnitTests allExprs
  where
    keyLogExpr =
      PactExpression [text| (keylog $acctModuleNameText.accounts "someId" 0) |] Nothing
    allExprs = keyLogExpr :| []
      

keysTests :: GasUnitTests
keysTests = defGasUnitTests allExprs
  where
    keysExprs =
      PactExpression [text| (keys $acctModuleNameText.accounts) |] Nothing
    allExprs = keysExprs :| []
      

selectTests :: GasUnitTests
selectTests = defGasUnitTests allExprs
  where
    selectExpr =
      PactExpression
      [text| (select $acctModuleNameText.accounts
               (where "balance" (constantly true))
             ) |]
      Nothing
    allExprs = selectExpr :| []
      

withReadTests :: GasUnitTests
withReadTests = defGasUnitTests allExprs
  where
    withReadExpr =
      PactExpression [text| (with-read
                $acctModuleNameText.accounts
                "someId"
                { "balance":= bal }
                bal
             )
      |]
      Nothing
    allExprs = withReadExpr :| []
 

withDefaultReadTests :: GasUnitTests
withDefaultReadTests = defGasUnitTests allExprs
  where
    withDefReadExpr =
      PactExpression [text| (with-default-read
                $acctModuleNameText.accounts
                "someId"
                { "balance": 1.0 }
                { "balance":= bal }
                bal
             )
      |]
      Nothing
    allExprs = withDefReadExpr :| []


readTests :: GasUnitTests
readTests = defGasUnitTests allExprs
  where
    readExpr =
      PactExpression [text| (read $acctModuleNameText.accounts "someId") |] Nothing
    allExprs = readExpr :| []
      

writeTests :: GasUnitTests
writeTests = defGasUnitTests allExprs
  where
    writeExpr =
      PactExpression [text| (write $acctModuleNameText.accounts
                    "some-id-that-is-not-present"
                    { "balance": 0.0 }
             ) |]
      Nothing
    allExprs = writeExpr :| []
        

updateTests :: GasUnitTests
updateTests = defGasUnitTests allExprs
  where
    updateExpr =
      PactExpression [text| (update $acctModuleNameText.accounts
                     "someId"
                     { "balance": 10.0 }
             ) |]
      Nothing
    allExprs = updateExpr :| []
      

insertTests :: GasUnitTests
insertTests = defGasUnitTests allExprs
  where
    insertExpr =
      PactExpression [text| (insert $acctModuleNameText.accounts
                     "some-id-that-is-not-present"
                     { "balance": 0.0 }
             )|]
      Nothing
    allExprs = insertExpr :| []


describeTableTests :: GasUnitTests
describeTableTests = defGasUnitTests allExprs
  where
    describeTableExpr =
      PactExpression [text| (describe-table $acctModuleNameText.accounts) |] Nothing
    allExprs = describeTableExpr :| []
      

describeModuleTests :: GasUnitTests
describeModuleTests = defGasUnitTests allExprs
  where
    describeModuleExpr =
      PactExpression [text| (describe-module "$acctModuleNameText") |] Nothing
    allExprs = describeModuleExpr :| []
      

describeKeysetTests :: GasUnitTests
describeKeysetTests = defGasUnitTests allExprs
  where
    describeKeysetExpr =
      PactExpression [text| (describe-keyset "$sampleLoadedKeysetName") |] Nothing
    allExprs = describeKeysetExpr :| []
    

createTableTests :: GasUnitTests
createTableTests = defGasUnitTests allExprs
  where
    createTableExpr =
      PactExpression
      [text| (create-table $acctModuleNameText.accounts-for-testing-table-creation) |]
      Nothing
    allExprs = createTableExpr :| []
      

-- | Keyset native function tests
defineKeysetTests :: GasUnitTests
defineKeysetTests = tests
  where
    simpleExpr =
      PactExpression
      [text| (define-keyset "some-keyset-name-not-present-already" $sampleLoadedKeysetName) |]
      Nothing
    rotateExprText = [text| (define-keyset "$sampleLoadedKeysetName" $sampleLoadedKeysetName) |]
    rotateExpr = PactExpression rotateExprText (Just $ rotateExprText <> ": rotating keyset")
    allExprs = rotateExpr :| [simpleExpr]

    -- Keyset rotation causes previous keyset to be enforced
    updateEnvMsgSig :: GasSetup e -> GasSetup e
    updateEnvMsgSig = setEnv (set eeMsgSigs (S.fromList samplePubKeys))
    
    tests =
      createGasUnitTests
      updateEnvMsgSig
      updateEnvMsgSig
      allExprs


enforceKeysetTests :: GasUnitTests
enforceKeysetTests = tests
  where
    enforceKeysetExpr = defPactExpression [text| (enforce-keyset '$sampleLoadedKeysetName) |]
    allExprs = enforceKeysetExpr :| []

    updateEnvMsgSig = setEnv (set eeMsgSigs (S.fromList samplePubKeys))

    tests =
      createGasUnitTests
      updateEnvMsgSig
      updateEnvMsgSig
      allExprs
    

readKeysetTests :: GasUnitTests
readKeysetTests = tests
  where
    readKeysetExpr = defPactExpression [text| (read-keyset 'my-keyset) |]
    allExprs = readKeysetExpr :| []
      
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
    keysAnyExpr = defPactExpression [text|(keys-any 10 1)|]
    allExprs = keysAnyExpr :| []
                  

keysAllTests :: GasUnitTests
keysAllTests = defGasUnitTests allExprs
  where
    keysAllExpr = defPactExpression [text|(keys-all 3 3)|]
    allExprs = keysAllExpr :| []
                  

keys2Tests :: GasUnitTests
keys2Tests = defGasUnitTests allExprs
  where
    keys2Expr = defPactExpression [text|(keys-2 3 1)|]
    allExprs = keys2Expr :| []


-- | Commitments native function tests
decryptCc20p1305Tests :: GasUnitTests
decryptCc20p1305Tests = defGasUnitTests allExprs
  where
    decryptExpr =
      defPactExpression [text| (decrypt-cc20p1305
              "Zi1REj5-iA"
              "AAAAAAECAwQFBgcI"
              "YWFk"
              "FYP6lG7xq7aExvoaHIH8Jg"
              "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
              "5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb")
      |]
    allExprs = decryptExpr :| []


validateKeypairTests :: GasUnitTests
validateKeypairTests = defGasUnitTests allExprs
  where
    validateExpr =
      defPactExpression [text| (validate-keypair
             "8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a"
             "77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a") |]
    allExprs = validateExpr :| []


-- | Time native function tests
addTimeTests :: GasUnitTests
addTimeTests = defGasUnitTests allExprs
  where
    addTimeExpr =
      defPactExpression [text| (add-time (time "2016-07-22T12:00:00Z") 15) |]
    allExprs = addTimeExpr :| []
      

daysTests :: GasUnitTests
daysTests = defGasUnitTests allExprs
  where
    daysExprText n = [text| (days $n) |]
    allExprs = NEL.map (createPactExpr daysExprText) sizesExpr


diffTimeTests :: GasUnitTests
diffTimeTests = defGasUnitTests allExprs
  where
    diffTime =
      defPactExpression [text| (diff-time (time "2016-07-22T12:00:00Z")
                        (time "2018-07-22T12:00:00Z"))
      |]
    allExprs = diffTime :| []
        

formatTimeTests :: GasUnitTests
formatTimeTests = defGasUnitTests allExprs
  where
    formatTimeSimpleExpr =
      defPactExpression [text| (format-time "%F" (time "2016-07-22T12:00:00Z")) |]
    formatTimeComplexExpr =
      defPactExpression [text| (format-time "%Y-%m-%dT%H:%M:%S%N" (time "2016-07-23T13:30:45Z")) |]

    allExprs = formatTimeSimpleExpr :|
               [formatTimeComplexExpr]


hoursTests :: GasUnitTests
hoursTests = defGasUnitTests allExprs
  where
    hoursExprText n = [text| (hours $n) |]
    allExprs = NEL.map (createPactExpr hoursExprText) sizesExpr


minutesTests :: GasUnitTests
minutesTests = defGasUnitTests allExprs
  where
    minutesExpr n =
      [text| (minutes $n) |]
    allExprs = NEL.map (createPactExpr minutesExpr) sizesExpr


parseTimeTests :: GasUnitTests
parseTimeTests = defGasUnitTests allExprs
  where
    parseTimeSimpleExpr =
      defPactExpression [text| (parse-time "%F" "2016-07-22") |]
    parseTimeComplexExpr =
      defPactExpression [text| (parse-time "%Y-%m-%dT%H:%M:%S%N" "2016-07-23T13:30:45+00:00") |]
    allExprs =  parseTimeSimpleExpr :|
               [parseTimeComplexExpr]


timeTests :: GasUnitTests
timeTests = defGasUnitTests allExprs
  where
    timeExpr =
      defPactExpression [text| (time "2016-07-22T12:00:00Z") |]
    allExprs = timeExpr :| []


-- | Operators native function tests
reverseBitsOptTests :: GasUnitTests
reverseBitsOptTests = defGasUnitTests allExprs
  where
    reverseBitsExpr x = [text| (~ $x) |]

    allExprs = NEL.map (createPactExpr reverseBitsExpr) sizesExpr


bitwiseOrOptTests :: GasUnitTests
bitwiseOrOptTests = defGasUnitTests allExprs
  where
    bitwiseOrExpr x = [text| (| 2 $x) |]

    allExprs = NEL.map (createPactExpr bitwiseOrExpr) sizesExpr


xorOptTests :: GasUnitTests
xorOptTests = defGasUnitTests allExprs
  where
    xorExpr x = [text| (xor 2 $x) |]

    allExprs = NEL.map (createPactExpr xorExpr) sizesExpr


sqrtOptTests :: GasUnitTests
sqrtOptTests = defGasUnitTests allExprs
  where
    sqrtExpr x = [text| (sqrt $x) |]
    sqrtDecimalExpr x = [text| (sqrt $x.1) |]

    allExprs = NEL.map (createPactExpr sqrtExpr) sizesExpr
      <> NEL.map (createPactExpr sqrtDecimalExpr) sizesExpr


shiftOptTests :: GasUnitTests
shiftOptTests = defGasUnitTests allExprs
  where
    shiftExpr x =
      [text| (shift 2 $x) |]
    shiftNegExpr x =
      [text| (shift -2 $x) |]

    allExprs = NEL.map (createPactExpr shiftExpr) sizesExpr
      <> NEL.map (createPactExpr shiftNegExpr) sizesExpr


roundOptTests :: GasUnitTests
roundOptTests = defGasUnitTests allExprs
  where
    roundExpr x =
      [text| (round $x.12345) |]
    roundPrecExpr x =
      [text| (round $x.12345 4) |]

    allExprs = NEL.map (createPactExpr roundExpr) sizesExpr
      <> NEL.map (createPactExpr roundPrecExpr) sizesExpr


orFuncOptTests :: GasUnitTests
orFuncOptTests = defGasUnitTests allExprs
  where
    orFuncExpr = defPactExpression [text| (or? (identity) (identity) true) |]

    allExprs = orFuncExpr :| []


orOptTests :: GasUnitTests
orOptTests = defGasUnitTests allExprs
  where
    orExpr = defPactExpression [text| (or false false) |]

    allExprs = orExpr :| []


notFuncOptTests :: GasUnitTests
notFuncOptTests = defGasUnitTests allExprs
  where
    notFuncExpr = defPactExpression [text| (not? (identity) true) |]

    allExprs = notFuncExpr :| []


notOptTests :: GasUnitTests
notOptTests = defGasUnitTests allExprs
  where
    notExpr = defPactExpression [text| (not true) |]

    allExprs = notExpr :| []


modOptTests :: GasUnitTests
modOptTests = defGasUnitTests allExprs
  where
    modExpr x =
      [text| (mod $x 2) |]

    allExprs = NEL.map (createPactExpr modExpr) sizesExpr



logOptTests :: GasUnitTests
logOptTests = defGasUnitTests allExprs
  where
    logExpr y =
      [text| (log 2 $y) |]
    logDecimalExpr y =
      [text| (log 2 $y.1) |]

    allExprs = NEL.map (createPactExpr logExpr) sizesExpr
      <> NEL.map (createPactExpr logDecimalExpr) sizesExpr



lnOptTests :: GasUnitTests
lnOptTests = defGasUnitTests allExprs
  where
    lnExpr x =
      [text| (ln $x) |]
    lnDecimalExpr x =
      [text| (ln $x.1) |]

    allExprs = NEL.map (createPactExpr lnExpr) sizesExpr
      <> NEL.map (createPactExpr lnDecimalExpr) sizesExpr


floorOptTests :: GasUnitTests
floorOptTests = defGasUnitTests allExprs
  where
    floorExpr x =
      [text| (floor $x.12345) |]
    floorPrecExpr x =
      [text| (floor $x.12345 4) |]

    allExprs = NEL.map (createPactExpr floorExpr) sizesExpr
      <> NEL.map (createPactExpr floorPrecExpr) sizesExpr


expOptTests :: GasUnitTests
expOptTests = defGasUnitTests allExprs
  where
    expExprSmall =
      defPactExpression [text| (exp 1) |]
    expExprMed =
      defPactExpression [text| (exp 10) |]
    expExprLarge =
      defPactExpression [text| (exp 100) |]

    allExprs =  expExprSmall :|
               [expExprMed
               ,expExprLarge]


ceilingOptTests :: GasUnitTests
ceilingOptTests = defGasUnitTests allExprs
  where
    ceilingExpr x =
      [text| (ceiling $x.12345) |]
    ceilingPrecExpr x =
      [text| (ceiling $x.12345 4) |]

    allExprs = NEL.map (createPactExpr ceilingExpr) sizesExpr
      <> NEL.map (createPactExpr ceilingPrecExpr) sizesExpr
        

andFuncOptTests :: GasUnitTests
andFuncOptTests = defGasUnitTests allExprs
  where
    andFuncExpr =
      defPactExpression [text| (and? (identity) (identity) true) |]

    allExprs = andFuncExpr :| []


andOptTests :: GasUnitTests
andOptTests = defGasUnitTests allExprs
  where
    andExpr =
      defPactExpression [text| (and false true) |]

    allExprs = andExpr :| []


absOptTests :: GasUnitTests
absOptTests = defGasUnitTests allExprs
  where
    absExpr x =
      [text| (abs -$x) |]
    absDecimalExpr x =
      [text| (abs -$x.0) |]

    allExprs = NEL.map (createPactExpr absExpr) sizesExpr
      <> NEL.map (createPactExpr absDecimalExpr) sizesExpr

        
raiseOptTests :: GasUnitTests
raiseOptTests = defGasUnitTests allExprs
  where
    raiseExpr y = 
      [text| (^ 2 $y) |]
    raiseDecimalExpr y =
      [text| (^ 2.1 $y.1) |]
    raiseBothExpr y =
      [text| (^ 2.1 $y) |]

    allExprs = NEL.map (createPactExpr raiseExpr) sizesExpr
      <> NEL.map (createPactExpr raiseDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr raiseBothExpr) sizesExpr


greaterThanEqOptTests :: GasUnitTests
greaterThanEqOptTests = defGasUnitTests allExprs
  where
    greaterEqExpr x =
      [text| (>= $x $x) |]
    greaterEqDecimalExpr x =
      [text| (>= $x.0 $x.0) |]
    greaterEqTimeExpr =
      [text| (>= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
   
    allExprs = NEL.map (createPactExpr greaterEqExpr) sizesExpr
      <> NEL.map (createPactExpr greaterEqDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr greaterEqExpr) escapedStringsExpr
      <> ((defPactExpression greaterEqTimeExpr) :| [])


greaterThanOptTests :: GasUnitTests
greaterThanOptTests = defGasUnitTests allExprs
  where
    greaterExpr x =
      [text| (> $x $x) |]
    greaterDecimalExpr x =
      [text| (> $x.0 $x.0) |]
    greaterTimeExpr =
      [text| (> (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
   
    allExprs = NEL.map (createPactExpr greaterExpr) sizesExpr
      <> NEL.map (createPactExpr greaterDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr greaterExpr) escapedStringsExpr
      <> ((defPactExpression greaterTimeExpr) :| [])
    

equalOptTests :: GasUnitTests
equalOptTests = defGasUnitTests allExprs
  where
    eqExpr x =
      [text| (= $x $x) |]
    eqDecimalExpr x =
      [text| (= $x.0 $x.0) |]
    eqTimeExpr =
      [text| (= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]

    allExprs = NEL.map (createPactExpr eqExpr) sizesExpr
      <> NEL.map (createPactExpr eqDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr eqExpr) escapedStringsExpr
      <> NEL.map (createPactExpr eqExpr) strKeyIntValMapsExpr
      <> NEL.map (createPactExpr eqExpr) intListsExpr
      <> ((defPactExpression eqTimeExpr) :| [])


lessThanEqualOptTests :: GasUnitTests
lessThanEqualOptTests = defGasUnitTests allExprs
  where
    lessEqExpr x =
      [text| (<= $x $x) |]
    lessEqDecimalExpr x =
      [text| (<= $x.0 $x.0) |]
    lessEqTimeExpr =
      [text| (<= (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
   
    allExprs = NEL.map (createPactExpr lessEqExpr) sizesExpr
      <> NEL.map (createPactExpr lessEqDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr lessEqExpr) escapedStringsExpr
      <> ((defPactExpression lessEqTimeExpr) :| [])


lessThanOptTests :: GasUnitTests
lessThanOptTests = defGasUnitTests allExprs
  where
    lessExpr x =
      [text| (< $x $x) |]
    lessDecimalExpr x =
      [text| (< $x.0 $x.0) |]
    lessTimeExpr =
      [text| (< (time "2016-07-22T12:00:00Z") (time "2018-07-22T12:00:00Z")) |]
   
    allExprs = NEL.map (createPactExpr lessExpr) sizesExpr
      <> NEL.map (createPactExpr lessDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr lessExpr) escapedStringsExpr
      <> ((defPactExpression lessTimeExpr) :| [])


divOptTests :: GasUnitTests
divOptTests = defGasUnitTests allExprs
  where
    divExpr x =
      [text| (/ $x $x) |]
    divDecimalExpr x =
      [text| (/ $x.0 $x.0) |]
    divBothExpr x =
      [text| (/ $x.0 $x) |]
   
    allExprs = NEL.map (createPactExpr divExpr) sizesExpr
      <> NEL.map (createPactExpr divDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr divBothExpr) sizesExpr


subOptTests :: GasUnitTests
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
   
    allExprs = NEL.map (createPactExpr subExpr) sizesExpr
      <> NEL.map (createPactExpr subDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr subBothExpr) sizesExpr
      <> NEL.map (createPactExpr subOneExpr) sizesExpr
      <> NEL.map (createPactExpr subOneDecimalExpr) sizesExpr


addOptTests :: GasUnitTests
addOptTests = defGasUnitTests allExprs
  where
    addExpr x =
      [text| (+ $x $x) |]
    addDecimalExpr x =
      [text| (+ $x.0 $x.0) |]
    addBothExpr x =
      [text| (+ $x.0 $x) |]

    allExprs = NEL.map (createPactExpr addExpr) sizesExpr
      <> NEL.map (createPactExpr addDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr addBothExpr) sizesExpr
      <> NEL.map (createPactExpr addExpr) escapedStringsExpr
      <> NEL.map (createPactExpr addExpr) strKeyIntValMapsExpr


multOptTests :: GasUnitTests
multOptTests = defGasUnitTests allExprs
  where
    multIntExpr x =
      [text| (* $x $x) |]
    multDecimalExpr x =
      [text| (* $x.0 $x.0) |]
    multBothExpr x =
      [text| (* $x.0 $x) |]

    allExprs = NEL.map (createPactExpr multIntExpr) sizesExpr
      <> NEL.map (createPactExpr multDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr multBothExpr) sizesExpr


bitwiseOptTests :: GasUnitTests
bitwiseOptTests = defGasUnitTests allExprs
  where
    bitwiseExpr x =
      [text| (& $x $x) |]

    allExprs = NEL.map (createPactExpr bitwiseExpr) sizesExpr


notEqualOptTests :: GasUnitTests
notEqualOptTests = defGasUnitTests allExprs
  where
    notEqualExpr x =
      [text| (!= $x $x) |]
    notEqualDecimalExpr x =
      [text| (!= $x.0 $x.0) |]
        
    allExprs = NEL.map (createPactExpr notEqualExpr) sizesExpr
      <> NEL.map (createPactExpr notEqualExpr) escapedStringsExpr
      <> NEL.map (createPactExpr notEqualDecimalExpr) sizesExpr
      <> NEL.map (createPactExpr notEqualExpr) intListsExpr
      <> NEL.map (createPactExpr notEqualExpr) strKeyIntValMapsExpr



-- | General native function tests
whereTests :: GasUnitTests
whereTests = defGasUnitTests allExprs
  where
    whereExpr obj =
      [text| (where "a1" (constantly true) $obj) |]

    allExprs = NEL.map (createPactExpr whereExpr) strKeyIntValMapsExpr


typeOfTests :: GasUnitTests
typeOfTests = defGasUnitTests allExprs
  where
    typeOfExpr t =
      [text| (typeof $t) |]

    allExprs = NEL.map (createPactExpr typeOfExpr) strKeyIntValMapsExpr
      <> NEL.map (createPactExpr typeOfExpr) escapedStringsExpr
      <> NEL.map (createPactExpr typeOfExpr) intListsExpr
      <> NEL.map (createPactExpr typeOfExpr) sizesExpr


txHashTests :: GasUnitTests
txHashTests = defGasUnitTests allExprs
  where
    txHashExpr = defPactExpression [text| (tx-hash) |]
    allExprs = txHashExpr :| []


tryTests :: GasUnitTests
tryTests = defGasUnitTests allExprs
  where
    tryPassExpr =
      defPactExpression [text| (try true (enforce true "this will definitely pass")) |]
    tryFailExpr =
      defPactExpression [text| (try true (enforce false "this will definitely fail")) |]

    allExprs = tryPassExpr :| [ tryFailExpr ]


takeTests :: GasUnitTests
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
    
    keysToTakeArgs = NEL.zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         NEL.map (createPactExpr takeFirstExpr) intListsExpr
      <> NEL.map (createPactExpr takeLastExpr) intListsExpr
      <> NEL.map takeKeysExpr keysToTakeArgs
      <> NEL.map (createPactExpr takeSingleKeyExpr) strKeyIntValMapsExpr


strToIntTests :: GasUnitTests
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


sortTests :: GasUnitTests
sortTests = defGasUnitTests allExprs
  where
    sortListExpr li =
      [text| (sort $li) |]

    reversedListsExpr = NEL.map format intLists
      where
        format (desc, li) = PactExpression
                            (reversedListExpr li)
                            (Just $ desc <> "NumberList")
        reversedListExpr li =
          toText $ MockList $ map MockInt (reverse $ NEL.toList li)
    
    allExprs = NEL.map (createPactExpr sortListExpr) reversedListsExpr


reverseTests :: GasUnitTests
reverseTests = defGasUnitTests allExprs
  where
    reverseExpr li =
      [text| (reverse $li) |]

    allExprs = NEL.map (createPactExpr reverseExpr) intListsExpr


removeTests :: GasUnitTests
removeTests = defGasUnitTests allExprs
  where
    removeExpr obj =
      [text| (remove "a1" $obj) |]

    allExprs = NEL.map (createPactExpr removeExpr) strKeyIntValMapsExpr


pactIdTests :: GasUnitTests
pactIdTests = tests
  where
    pactIdExpr = defPactExpression [text|(pact-id)|]
    
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


resumeTests :: GasUnitTests
resumeTests = tests
  where
    resumeExprText binding = [text|(resume $binding a1)|]
 
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

    toNonSPVTests ::
      (HM.HashMap T.Text Integer, PactExpression)
      -> GasUnitTests
    toNonSPVTests (yieldMap, expr)
      = createGasUnitTests
        (setupForResume False yieldMap)
        (setupForResume False yieldMap)
        ((addProvenanceDesc expr) :| [])

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
          = setEnv (set (eePublicData . pdPublicMeta . pmChainId) chainId)
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
          = bool Nothing (Just $ Provenance chainId someModuleHash) isProv
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
      defPactExpression [text| (pact-version) |]

    allExprs = versionExpr :| []


readStringTests :: GasUnitTests
readStringTests = tests
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
        ( (readStringExpr desc) :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests strings


readMsgTests :: GasUnitTests
readMsgTests = tests
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
        ((readMsgExpr desc) :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests strKeyIntValMaps


readIntegerTests :: GasUnitTests
readIntegerTests = tests
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
        ((readIntExpr desc) :| [])

    tests = concatGasUnitTests $
            NEL.map setupTests sizes


readDecimalTests :: GasUnitTests
readDecimalTests = tests
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
        ((readDecExpr desc) :| [])
   
    tests = concatGasUnitTests $
            NEL.map setupTests sizes


mapTests :: GasUnitTests
mapTests = defGasUnitTests allExprs
  where
    mapExpr li =
      [text| (map (identity) $li) |]

    allExprs = NEL.map (createPactExpr mapExpr) intListsExpr


makeListTests :: GasUnitTests
makeListTests = defGasUnitTests allExprs
  where
    makeListExpr len =
      [text| (make-list $len true) |]

    allExprs = NEL.map (createPactExpr makeListExpr) sizesExpr


listModulesTests :: GasUnitTests
listModulesTests = defGasUnitTests allExprs
  where
    listModulesExpr =
      defPactExpression [text| (list-modules) |]

    allExprs = listModulesExpr :| []


lengthTests :: GasUnitTests
lengthTests = defGasUnitTests allExprs
  where
    lengthExpr t =
      [text| (length $t) |]

    allExprs =
         NEL.map (createPactExpr lengthExpr) intListsExpr
      <> NEL.map (createPactExpr lengthExpr) escapedStringsExpr
      <> NEL.map (createPactExpr lengthExpr) strKeyIntValMapsExpr


intToStrTests :: GasUnitTests
intToStrTests = defGasUnitTests allExprs
  where
    int2strExpr (valInt,baseInt) =
      defPactExpression [text| (int-to-str $base $val) |]
      where base = intToStr baseInt
            val = intToStr valInt

    baseList :: NonEmpty Integer
    baseList = 64 :| [2..16]
    -- TODO is foldr1 the best thing to do here
    -- | Test every base conversion against three different number sizes
    args = F.foldr1 (<>) $ NEL.map (\(_, n) -> NEL.map (\b -> (n,b)) baseList) sizes
    
    allExprs = NEL.map (int2strExpr) args


ifTests :: GasUnitTests
ifTests = defGasUnitTests allExprs
  where
    ifExpr =
      defPactExpression [text| (if true "then-clause" "else-clause") |]

    allExprs = ifExpr :| []


identityTests :: GasUnitTests
identityTests = defGasUnitTests allExprs
  where
    identityExpr val =
      [text| (identity $val) |]

    allExprs = NEL.map (createPactExpr identityExpr) intListsExpr


hashTests :: GasUnitTests
hashTests = defGasUnitTests allExprs
  where
    hashExpr val =
      [text| (hash $val) |]

    allExprs =
         NEL.map (createPactExpr hashExpr) escapedStringsExpr
      <> NEL.map (createPactExpr hashExpr) strKeyIntValMapsExpr

formatTests :: GasUnitTests
formatTests = defGasUnitTests allExprs
  where
    formatExpr (str,(PactExpression expr desc')) =
      PactExpression
      [text| (format "$str" $expr )|]
      (Just [text| (format "{}...{}" $desc)|])
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


foldTests :: GasUnitTests
foldTests = defGasUnitTests allExprs
  where
    foldExpr li =
      [text| (fold (constantly 0) 1 $li) |]
    allExprs = NEL.map (createPactExpr foldExpr) intListsExpr


filterTests :: GasUnitTests
filterTests = defGasUnitTests allExprs
  where
    filterExpr li =
      [text| (filter (constantly true) $li)|]
    allExprs = NEL.map (createPactExpr filterExpr) intListsExpr


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
      = NEL.map
        (\(desc, li) -> PactExpression (toText (MockList li)) (Just $ desc <> "EnforceList"))
        listOfEnforcesList

    allExprs
      = NEL.map (createPactExpr enforceOneExpr) listOfEnforcesListExpr


enforcePactVersionTests :: GasUnitTests
enforcePactVersionTests = defGasUnitTests allExprs
  where
    enforcePactVersionExpr =
      defPactExpression [text| (enforce-pact-version "3.0")|]
    allExprs = enforcePactVersionExpr :| []


-- TODO: Unable to currently test when enforce's
--       predicate function returns false.
enforceTests :: GasUnitTests
enforceTests = defGasUnitTests allExprs
  where
    allExprs =
      defPactExpression [text| (enforce true "some-error-message")|] :| []
 

dropTests :: GasUnitTests
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
    
    keysToDropArgs = NEL.zip escapedStrListsExpr strKeyIntValMapsExpr

    allExprs =
         NEL.map (createPactExpr dropFirstExpr) intListsExpr
      <> NEL.map (createPactExpr dropLastExpr) intListsExpr
      <> NEL.map dropKeysExpr keysToDropArgs
      <> NEL.map (createPactExpr dropSingleKeyExpr) strKeyIntValMapsExpr


namespaceTests :: GasUnitTests
namespaceTests = tests
  where
    namespaceExpr = defPactExpression [text| (namespace '$sampleNamespaceName) |]

    updateEnvWithSig =
      setEnv $ set eeMsgSigs (S.fromList samplePubKeys)

    tests = createGasUnitTests
            updateEnvWithSig
            updateEnvWithSig
            (namespaceExpr :| [])


defineNamespaceTests :: GasUnitTests
defineNamespaceTests = tests
  where
    tests = simpleDefTests <> rotateNamespaceTests
    
    simpleDefTests = defGasUnitTests (simpleDefExpr :| [])
      where
        simpleDefExpr =
          defPactExpression [text| (define-namespace 'some-other-namespace $sampleLoadedKeysetName) |]
 
    rotateNamespaceTests = rotateTests
      where
        rotateExprText = [text| (define-namespace '$sampleNamespaceName $sampleLoadedKeysetName) |]
        rotateExpr = 
          PactExpression
          rotateExprText
          (Just $ rotateExprText <> ": Defining namespace with the same name as one already defined.")

        updateMsgSig =
          setEnv $ set eeMsgSigs (S.fromList samplePubKeys)

        rotateTests =
          createGasUnitTests
          (updateMsgSig)
          (updateMsgSig)
          (rotateExpr :| [])


containsTests :: GasUnitTests
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

    listArgs = NEL.zip sizesExpr intListsExpr
    strArgs = NEL.zip sizesExpr escapedStringsExpr

    allExprs =
         NEL.map containsListExpr listArgs
      <> NEL.map (createPactExpr containsObjExpr) strKeyIntValMapsExpr
      <> NEL.map containsStrExpr strArgs


constantlyTests :: GasUnitTests
constantlyTests = defGasUnitTests allExprs
  where
    singleIgnoreExpr = 
      defPactExpression [text| (constantly 0 "firstIgnore") |]
    doubleIgnoreExpr = 
      defPactExpression [text| (constantly 0 "firstIgnore" "secondIgnore") |]
    tripleIgnoreExpr = 
      defPactExpression [text| (constantly 0 "firstIgnore" "secondIgnore" "thirdIgnore") |]
    allExprs =
      singleIgnoreExpr :| [doubleIgnoreExpr, tripleIgnoreExpr]


composeTests :: GasUnitTests
composeTests = defGasUnitTests allExprs
  where
    composeExpr =
      defPactExpression [text| (compose (+ 0) (+ 0) 0) |]
    allExprs = composeExpr :| []


chainDataTests :: GasUnitTests
chainDataTests = defGasUnitTests allExprs
  where
    allExprs = (defPactExpression [text| (chain-data) |]) :| []


atTests :: GasUnitTests
atTests = defGasUnitTests allExprs
  where
    atListExpr (PactExpression idx _, PactExpression li liDesc') =
      PactExpression
      [text| (at $idx $li) |]
      (Just [text| (at $idx $liDesc) |])
      where liDesc = fromMaybe li liDesc'

    atObjExpr obj =
      [text| (at "a1" $obj) |]

    listIndices = NEL.map
                  (\(desc,i) -> (PactExpression (toText $ MockInt $ pred i) (Just desc)))
                  sizes
    listArgs = NEL.zip listIndices escapedStrListsExpr

    allExprs = NEL.map atListExpr listArgs
      <> NEL.map (createPactExpr atObjExpr) strKeyIntValMapsExpr

bindTests :: GasUnitTests
bindTests = defGasUnitTests allExprs
  where
    bindExprText obj bind = [text| (bind $obj $bind a1) |]
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
