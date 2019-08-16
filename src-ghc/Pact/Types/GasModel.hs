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
  "at" -> Just atTests
  "bind" -> Just bindTests
  -- "chain-data" -> chainDataTests --TODO
  "compose" -> Just composeTests
  "constantly" -> Just constantlyTests
  "contains" -> Just containsTests
  -- "define-namespace" -> Just defineNamespaceTests --TODO
  "drop" -> Just dropTests
  "enforce" -> Just enforceTests
  "enforce-one" -> Just enforceOneTests
  -- "enforce-pact-version" -> Just enforcePactVersionTests --TODO
  "filter" -> Just filterTests
  "fold" -> Just foldTests
  "format" -> Just formatTests
  "hash" -> Just hashTests
  "identity" -> Just identityTests
  "if" -> Just ifTests
  "int-to-str" -> Just intToStrTests
  "length" -> Just lengthTests
  -- "list-module" -> Just listModuleTests --TODO
  "make-list" -> Just makeListTests
  "map" -> Just mapTests
  -- "namespace" -> Just namespaceTests --TODO
  -- "pact-id" -> Just pactIdTests --TODO
  -- "pact-version" -> Just pactVersionTests --TODO
  -- "public-chain-data" -> Just publicChainDataTests --TODO
  _ -> Nothing


-- | General native function tests
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
