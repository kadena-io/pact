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
  , atTests
  , bindTests
  
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

toText :: (Show a) => a -> T.Text
toText = T.pack . show

escapeText :: T.Text -> T.Text
escapeText n = "\"" <> n <> "\"" 

toPactMap :: HM.HashMap T.Text Integer -> T.Text
toPactMap m = "{ " <> allKeys <> " }"
  where
    allKeys = T.intercalate ", " $ map colonFormat (HM.toList m)
    colonFormat (key, val) = escapeText key <> ": " <> toText val

toPactBinding :: HM.HashMap T.Text b -> T.Text
toPactBinding  m = "{ " <> allKeys <> " }"
  where
    allKeys = T.intercalate ", " $ map bindingFormat (HM.toList m)
    bindingFormat (key, _) = escapeText key <> " := " <> key

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
unitTests = HM.empty


unitTestFromDef :: NativeDefName -> Maybe GasUnitTests
unitTestFromDef nativeName = case (asString nativeName) of
  "at" -> Just atTests
  "bind" -> Just bindTests
  _ -> Nothing


atTests :: GasUnitTests
atTests = GasUnitTests $ NEL.map (SomeGasTest . defGasTest) allExprs
  where
    atExpr idx t =
      [text| (at $idx $t)
      |]
    
    longNum = 100000
    medNum = 100
    smallNum = 10

    (longIdx, longList) = (longNum - 1, [1..longNum])
    (medIdx, medList) = (medNum - 1, [1..medNum])
    (smallIdx, smallList) = (smallNum - 1, [1..smallNum])

    createMap li = HM.fromList $ map (\n -> (toText n, n)) li
    longMap = createMap longList
    medMap = createMap medList
    smallMap = createMap smallList

    allExprs =
      ( atExpr (toText longIdx) (toText longList) ) :|
      [ atExpr (toText medIdx) (toText medList),
        atExpr (toText smallIdx) (toText smallList),
        atExpr (escapeText $ toText longNum) (toPactMap longMap),
        atExpr (escapeText $ toText medNum) (toPactMap medMap),
        atExpr (escapeText $ toText smallNum) (toPactMap smallMap)
      ]


bindTests :: GasUnitTests
bindTests = GasUnitTests $ NEL.map (SomeGasTest . defGasTest) allExprs
  where
    bindExpr obj binding =
      [text| (bind $obj $binding a1)
      |]

    longNum, medNum, smallNum :: Integer
    longNum = 100000
    medNum = 100
    smallNum = 10
        
    longList = [1..longNum]
    medList = [1..medNum]
    smallList = [1..smallNum]

    createMap li = HM.fromList $ map (\n -> ("a" <> toText n, n)) li
    longMap = createMap longList
    medMap = createMap medList
    smallMap = createMap smallList

    allExprs =
      ( bindExpr (toPactMap longMap) (toPactBinding longMap) ) :|
      [ bindExpr (toPactMap medMap) (toPactBinding medMap),
        bindExpr (toPactMap smallMap) (toPactBinding smallMap)
      ]


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
