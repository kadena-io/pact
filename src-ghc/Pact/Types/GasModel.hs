{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Pact.Types.GasModel
  ( GasTest(..)
  , SomeGasTest(..)
  , GasUnitTests(..)

  , NoopNFData(..)
  
  , unitTests
  , allNatives
  , untestedNatives
  
  , parseCode
  , printResult
  , eitherDie

  , unitTestFromDef
  
  ) where

import Control.Lens               hiding ((.=),DefName)
import Control.DeepSeq            (NFData(..))
import Control.Exception          (throwIO)
import Data.Aeson                 (toJSON, ToJSON(..))
import Data.Default               (def)
import Data.List                  (foldl')
import NeatInterpolation          (text)
import Data.List.NonEmpty         (NonEmpty(..))


import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable       as F
import qualified Data.Text           as T
import qualified Data.List.NonEmpty  as NEL


-- Internal exports
--
import Pact.Native
import Pact.Types.Native
import Pact.Compile               (compileExps, mkTextInfo)
import Pact.MockDb
import Pact.Types.PactValue       (toPactValueLenient)

import Pact.Gas
import Pact.Interpreter
import Pact.Parse
import Pact.Types.Command
import Pact.Types.Lang
import Pact.Types.Runtime
import Pact.Types.SPV



data GasTest e = GasTest
  { _gasTestExpression :: !T.Text
  , _gasTestDescription :: !T.Text
  , _gasTestMockState :: !EvalState
  , _gasTestMockEnv :: !(IO (EvalEnv e))
  , _gasTestCleanup :: !(EvalEnv e -> IO ())
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



defGasTest :: T.Text -> GasTest ()
defGasTest expr =
  GasTest
  expr
  expr
  defEvalState
  createDefMockEnv
  (const $ return ())

defEvalState :: EvalState
defEvalState = def

defEvalEnv :: PactDbEnv e -> EvalEnv e
defEvalEnv db =
  setupEvalEnv db entity Transactional (initMsgData pactInitialHash)
  initRefStore freeGasEnv permissiveNamespacePolicy noSPVSupport def
  where entity = Just $ EntityName "entity"

createDefMockEnv :: IO (EvalEnv ())
createDefMockEnv = do
  db <- mkMockEnv def
  return $ defEvalEnv db

defGasUnitTests
  :: NEL.NonEmpty T.Text
  -> GasUnitTests
defGasUnitTests pactExprs
  = GasUnitTests $ NEL.map (SomeGasTest . defGasTest) pactExprs

--- Helper functions
---

parseCode :: T.Text -> IO [Term Name]
parseCode m = do
  parsedCode <- ParsedCode m <$> eitherDie m (parseExprs m)
  throwEither $ compileExps
                (mkTextInfo $ _pcCode parsedCode)
                (_pcExps parsedCode)

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


--- Sample Pact literals
---

sizes :: NEL.NonEmpty Integer
sizes =
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

-- | example: [ "[ a1 a2 a3 a4]" ]
unescapedStrListsExpr :: NEL.NonEmpty PactExpression
unescapedStrListsExpr = NEL.map makeExpr strLists
  where
    makeExpr li = toText $ MockList
                  $ map MockExpr (NEL.toList li)


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

-- | example: "a1a2a3a4"
unescapedStringsExpr :: NEL.NonEmpty PactExpression
unescapedStringsExpr = NEL.map (toText . MockExpr) strings


--- Gas unit tests
---
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
unitTestFromDef nativeName = case (asString nativeName) of
  -- | General native functions
  "at"           -> Just atTests
  "bind"         -> Just bindTests
  -- "chain-data" -> chainDataTests --TODO
  "compose"      -> Just composeTests
  "constantly"   -> Just constantlyTests
  "contains"     -> Just containsTests
  -- "define-namespace" -> Just defineNamespaceTests --TODO
  "drop"         -> Just dropTests
  "enforce"      -> Just enforceTests
  "enforce-one"  -> Just enforceOneTests
  -- "enforce-pact-version" -> Just enforcePactVersionTests --TODO
  "filter"       -> Just filterTests
  "fold"         -> Just foldTests
  "format"       -> Just formatTests
  "hash"         -> Just hashTests
  "identity"     -> Just identityTests
  "if"           -> Just ifTests
  "int-to-str"   -> Just intToStrTests
  "length"       -> Just lengthTests
  -- "list-module" -> Just listModuleTests --TODO
  "make-list"    -> Just makeListTests
  "map"          -> Just mapTests
  -- "namespace" -> Just namespaceTests --TODO
  -- "pact-id" -> Just pactIdTests --TODO
  -- "pact-version" -> Just pactVersionTests --TODO
  -- "read-decimal" -> Just readDecimalTests --TODO
  -- "read-integer" -> Just readIntegerTests --TODO
  -- "read-msg" -> Just readMsgTests --TODO
  "remove"       -> Just removeTests
  -- "resume" -> Just resumeTests --TODO
  "reverse"      -> Just reverseTests
  "sort"         -> Just sortTests
  "str-to-int"   -> Just strToIntTests
  "take"         -> Just takeTests
  "try"          -> Just tryTests
  --"tx-hash" -> Just txHashTests --TODO because even with something else, returns def tx hash
  "typeof"       -> Just typeOfTests
  "where"        -> Just whereTests
  --"yield" -> Just yieldTests --TODO

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
  
  _ -> Nothing


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

    allExprs = NEL.map eqExpr sizesExpr
      <> NEL.map eqDecimalExpr sizesExpr
      <> NEL.map eqExpr escapedStringsExpr
      <> NEL.map eqExpr strKeyIntValMapsExpr
      <> NEL.map eqExpr intListsExpr


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
