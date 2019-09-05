{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Pact.GasModel.Utils
  (
    compileCode
  , printResult
  , eitherDie
  , dupe

  , sizes
  , sizesExpr
  
  , intLists
  , intListsExpr

  , strLists
  , escapedStrListsExpr

  , strKeyIntValMaps
  , strKeyIntValMapsExpr
  , strKeyIntValBindingsExpr

  , strings
  , escapedStringsExpr
  
  , PactExpression
  , MockPactType(..)
  , toText
  , intToStr
  , escapeText
  , toPactKeyset
  
  , acctModuleName
  , acctModuleNameText
  , accountsModule
  
  , acctRow

  , sampleLoadedKeysetName
  , sampleKeyset
  , samplePubKeys

  , sampleNamespaceName
  , sampleNamespace

  , someModuleName
  , someModuleHash
  , someStackFrame
  , someModuleData
  ) where


import Bound                      (abstract, Scope)
import Control.Exception          (throwIO)
import Data.Aeson                 (toJSON, ToJSON(..))
import Data.Default               (def)
import NeatInterpolation          (text)
import Data.List.NonEmpty         (NonEmpty(..))


import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet        as HS
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.List.NonEmpty  as NEL


import Pact.Compile               (compileExps, mkTextInfo)
import Pact.Types.PactValue       (toPactValueLenient, PactValue(..))

import Pact.Parse
import Pact.Types.Command
import Pact.Types.Lang
import Pact.Types.Runtime



-- | General helper functions
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



-- | Pact literals of different sizes and textual description
sizes :: NEL.NonEmpty (T.Text, Integer)
sizes =
    ("long", 10000) :|
  [ ("med", 100),
    ("small", 10)
  ]

-- example: "100"
sizesExpr :: NEL.NonEmpty (T.Text, PactExpression)
sizesExpr = NEL.map format sizes
  where
    format (desc, i) = (desc <> "Number",
                        toText (MockInt i))


-- List of integers of varying sizes
intLists :: NEL.NonEmpty (T.Text, NEL.NonEmpty Integer)
intLists = NEL.map format sizes
  where
    format (desc, i) = (desc, (1 :| [2..i]))


-- example: [ "[1 2 3 4]" ]
intListsExpr :: NEL.NonEmpty (T.Text, PactExpression)
intListsExpr = NEL.map format intLists
  where
    format (desc, li) = (desc <> "NumberList",
                         makeExpr li)
    makeExpr li =
      toText $ MockList $ map MockInt (NEL.toList li)


-- List of strings of varying sizes
strLists :: NEL.NonEmpty (T.Text, NEL.NonEmpty T.Text)
strLists = NEL.map format intLists
  where
    format (desc, li) = (desc,
                         (NEL.map (("a" <>) . intToStr) li))


-- example: [ "[ \"a1\" \"a2\" \"a3\" \"a4\" ]" ]
escapedStrListsExpr :: NEL.NonEmpty (T.Text, PactExpression)
escapedStrListsExpr = NEL.map format strLists
  where
    format (desc, li) = (desc <> "EscapedStrList",
                         makeExpr li)
    makeExpr li =
      toText $ MockList $ map MockString (NEL.toList li)


-- Maps of varying sizes.
strKeyIntValMaps :: NEL.NonEmpty (T.Text, (HM.HashMap T.Text Integer))
strKeyIntValMaps = NEL.map toMap allLists
  where allLists = NEL.zip strLists intLists
        toMap ((kDesc, kList), (_, vList)) = (kDesc, m)
          where
            m = HM.fromList
                $ zip (NEL.toList kList) (NEL.toList vList)


-- example: "{ \"a5\": 5, \"a3\": 3 }"
strKeyIntValMapsExpr :: NEL.NonEmpty (T.Text, PactExpression)
strKeyIntValMapsExpr = NEL.map format strKeyIntValMaps
  where
    format (desc, m) = (desc <> "OjectMap", toText (MockObject m))


-- example: "{ \"a5\" := a5, \"a3\" := a3 }"
strKeyIntValBindingsExpr :: NEL.NonEmpty (T.Text, PactExpression)
strKeyIntValBindingsExpr = NEL.map format strKeyIntValMaps
  where
    format (desc, m) = (desc <> "Binding", toText (MockBinding m))


-- Strings of varying sizes
strings :: NEL.NonEmpty (T.Text, T.Text)
strings = NEL.map format sizes
  where
    toString i = T.replicate (fromIntegral i) "a"
    format (desc, i) = (desc, toString i)
                        

-- example: "\"aaaaa\""
escapedStringsExpr :: NEL.NonEmpty (T.Text, PactExpression)
escapedStringsExpr = NEL.map format strings
  where
    format (desc, s) = (desc <> "String",
                        toText (MockString s))



-- | Helper functions and types for creating pact expressions
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

someModuleName :: ModuleName
someModuleName = ModuleName "some-module" Nothing

someModuleHash :: ModuleHash
someModuleHash = ModuleHash $ pactHash ""

someStackFrame :: StackFrame
someStackFrame =
  StackFrame "" def
  (Just ((FunApp def ""
           (Just someModuleName) Defun (funTypes $ FunType [] TyAny) Nothing)
        ,[])
  )

someModuleData :: ModuleData Ref
someModuleData = ModuleData modDef refMap
  where refMap = HM.empty
        ref = Direct $ TVar (Name "" def) def
        fst' :: Ref -> Maybe Int
        fst' = const Nothing
        scd' :: Term Ref
        scd' = TVar ref def
        
        scopeOfRef :: Scope Int Term Ref
        scopeOfRef = abstract fst' scd'
        
        defOfRef = Def (DefName "") someModuleName Defun (FunType [] TyAny) scopeOfRef def def
        modDef = MDModule mod'
        gov = Governance $ Right defOfRef
        mod' = Module someModuleName gov def (Code "") someModuleHash HS.empty [] []
