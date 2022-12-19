{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Pact.GasModel.Utils
  (
    compileCode
  , eitherDie

  , sizes
  , sizesExpr

  , intLists
  , intListsExpr
  , duplicateListsExpr

  , strLists
  , escapedStrListsExpr

  , strKeyIntValMaps
  , strKeyIntValMapsExpr
  , strKeyIntValBindingsExpr

  , strings
  , escapedStringsExpr

  , PactExpression(..)
  , defPactExpression
  , createPactExpr
  , MockPactType(..)
  , toText
  , intToStr
  , escapeText
  , toPactKeyset

  , acctModuleName
  , acctModuleNameText
  , accountsModule
  , regressionModule

  , acctRow

  , sampleLoadedKeysetName
  , sampleLoadedMultisigKeysetName
  , sampleKeyset
  , samplePubKeys
  , samplePubKeysWithCaps
  , sampleMultiPubKeys
  , sampleMultiSigKeyset

  , sampleNamespaceName
  , sampleNamespace

  , someModuleName
  , someModuleHash
  , someStackFrame
  , someModuleData
  ) where


import Bound (abstract, Scope)
import Control.Exception (throwIO)
import Data.Default (def)
import NeatInterpolation (text)


import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T


import Pact.Compile (compileExps, mkTextInfo)
import Pact.Types.Capability (SigCapability)
import Pact.Types.Command
import Pact.Types.Lang
import Pact.Types.PactValue (PactValue(..))
import Pact.Types.RowData
import Pact.Types.Runtime
import Pact.Types.Namespace
import Pact.Parse
import Pact.JSON.Legacy.Value


-- | General helper functions
compileCode :: T.Text -> IO [Term Name]
compileCode m = do
  parsedCode <- parseCode m
  eitherDie m $ compileExps def
                (mkTextInfo $ _pcCode parsedCode)
                (_pcExps parsedCode)

parseCode :: T.Text -> IO ParsedCode
parseCode m = do
  ParsedCode m <$> eitherDie m (parseExprs m)

eitherDie :: (Show b) => T.Text -> Either b a -> IO a
eitherDie annot
  = either (throwIO . userError . ((show annot ++ " : ") ++) . show) (return $!)



-- | Pact literals of different sizes and textual description
sizes :: [(T.Text, Integer)]
sizes =
  [ ("long", 10000)
  , ("med", 100)
  , ("small", 10)
  ]

-- example: "100"
sizesExpr :: [PactExpression]
sizesExpr = map format sizes
  where
    format (desc, i) = PactExpression
                       (toText (MockInt i))
                       (Just $ desc <> "Number")


-- List of integers of varying sizes
intLists :: [(T.Text, [Integer])]
intLists = map format sizes
  where
    format (desc, i) = (desc, [1..i])

duplicateListsExpr :: [PactExpression]
duplicateListsExpr = map format intLists
  where
    duplicate = foldr (\a b -> a : a : b) []

    format (desc, duplicate -> li) = PactExpression
      (makeExpr li)
      (Just $ desc <> "DuplicateNumberList")

    makeExpr li =
      toText $ MockList $ map MockInt li

-- example: [ "[1 2 3 4]" ]
intListsExpr :: [PactExpression]
intListsExpr = map format intLists
  where
    format (desc, li) = PactExpression
                        (makeExpr li)
                        (Just $ desc <> "NumberList")
    makeExpr li =
      toText $ MockList $ map MockInt li


-- List of strings of varying sizes
strLists :: [(T.Text, [T.Text])]
strLists = map format intLists
  where
    format (desc, li) = (desc,
                         map (("a" <>) . intToStr) li)


-- example: [ "[ \"a1\" \"a2\" \"a3\" \"a4\" ]" ]
escapedStrListsExpr :: [PactExpression]
escapedStrListsExpr = map format strLists
  where
    format (desc, li) = PactExpression
                        (makeExpr li)
                        (Just $ desc <> "EscapedStrList")
    makeExpr li =
      toText $ MockList $ map MockString li


-- Maps of varying sizes.
strKeyIntValMaps :: [(T.Text, (HM.HashMap T.Text Integer))]
strKeyIntValMaps = map toMap allLists
  where allLists = zip strLists intLists
        toMap ((kDesc, kList), (_, vList)) = (kDesc, m)
          where
            m = HM.fromList
                $ zip kList vList


-- example: "{ \"a5\": 5, \"a3\": 3 }"
strKeyIntValMapsExpr :: [PactExpression]
strKeyIntValMapsExpr = map format strKeyIntValMaps
  where
    format (desc, m) = PactExpression
                       (toText (MockObject m))
                       (Just $ desc <> "OjectMap")


-- example: "{ \"a5\" := a5, \"a3\" := a3 }"
strKeyIntValBindingsExpr :: [PactExpression]
strKeyIntValBindingsExpr = map format strKeyIntValMaps
  where
    format (desc, m) = PactExpression
                       (toText (MockBinding m))
                       (Just $ desc <> "Binding")


-- Strings of varying sizes
strings :: [(T.Text, T.Text)]
strings = map format sizes
  where
    toString i = T.replicate (fromIntegral i) "a"
    format (desc, i) = (desc, toString i)


-- example: "\"aaaaa\""
escapedStringsExpr :: [PactExpression]
escapedStringsExpr = map format strings
  where
    format (desc, s) = PactExpression
                       (toText (MockString s))
                       (Just $ desc <> "String")



-- | Helper functions and types for creating pact expressions
data PactExpression = PactExpression
  { _pactExpressionFull :: T.Text
  , _pactExpressionAbridged :: Maybe T.Text
  }

defPactExpression :: T.Text -> PactExpression
defPactExpression expr = PactExpression expr Nothing


createPactExpr
  :: (T.Text -> T.Text)
  -> PactExpression
  -> PactExpression
createPactExpr f (PactExpression arg abridged) =
  case abridged of
    Nothing -> PactExpression (f arg) (Just $ f arg)
    Just a -> PactExpression (f arg) (Just $ f a)


data MockPactType =
    MockObject (HM.HashMap T.Text Integer)
  | MockBinding (HM.HashMap T.Text Integer)
  | MockList [MockPactType]
  | MockBool Bool
  | MockInt Integer
  | MockString T.Text
  | MockExpr T.Text
  deriving (Show)

toText :: MockPactType -> T.Text
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

escapeText :: T.Text -> T.Text
escapeText n = "\"" <> n <> "\""

toPactMap
  :: HM.HashMap T.Text Integer
  -> T.Text
toPactMap m = "{ " <> allKeys <> " }"
  where
    allKeys = T.intercalate ", " $ map colonFormat (HM.toList m)
    colonFormat (key, val) = escapeText key <> ": " <> toText (MockInt val)

toPactBinding
  :: HM.HashMap T.Text Integer
  -> T.Text
toPactBinding  m = "{ " <> allKeys <> " }"
  where
    allKeys = T.intercalate ", " $ map bindingFormat (HM.toList m)
    bindingFormat (key, _) = escapeText key <> " := " <> key

-- Only use in testing: Pact.GasModel.GasTests
--
toPactKeyset :: T.Text -> T.Text -> Maybe T.Text -> LegacyValue
toPactKeyset ksName ksValue predicate =
  toLegacyJson $ HM.fromList [(ksName, A.object ["keys" A..= [ksValue], "pred" A..= pred'])]
  where pred' = maybe ">" id predicate


-- | Sample pact code and helper functions/values for testing
acctModuleName :: ModuleName
acctModuleName = ModuleName "accounts" def

acctModuleNameText :: T.Text
acctModuleNameText = asString acctModuleName

-- | This is the "default module" for putting various testing items in.
-- Changing this must not result in any regression.
accountsModule :: ModuleName -> T.Text
accountsModule moduleName = [text|
     (module $moduleNameText GOV

       (defcap GOV ()
         true)

       (defcap MANAGEDCAP (s t)
         @managed s managed-cap-fun
         true)

       (defun managed-cap-fun (s t)
         s)

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

       (defcap EVENT () @event true)

       (defun test-emit-event-func () (emit-event (EVENT)))

       ; table for testing `create-table`
       (deftable accounts-for-testing-table-creation:{account})
     ) |]
  where moduleNameText = asString moduleName

-- | This is used in a gas test and should not be changed.
regressionModule :: ModuleName -> T.Text
regressionModule moduleName = [text|
     (module $moduleNameText GOV

       (defcap GOV ()
         true)

       (defcap MANAGEDCAP (s t)
         @managed s managed-cap-fun
         true)

       (defun managed-cap-fun (s t)
         s)

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

acctRow :: RowData
acctRow = RowData RDV0 $ ObjectMap $ M.fromList
          [("balance", pactValueToRowData $ PLiteral (LDecimal 100.0))]

sampleLoadedKeysetName :: T.Text
sampleLoadedKeysetName = "some-loaded-keyset"

sampleLoadedMultisigKeysetName :: T.Text
sampleLoadedMultisigKeysetName = "some-loaded-multisig-keyset"

samplePubKeys :: [PublicKeyText]
samplePubKeys = [PublicKeyText "something"]

sampleMultiPubKeys :: [PublicKeyText]
sampleMultiPubKeys =
  [ PublicKeyText "key1"
  , PublicKeyText "key2"
  ]

samplePubKeysWithCaps :: [(PublicKeyText, S.Set SigCapability)]
samplePubKeysWithCaps = map (\p -> (p,S.empty)) samplePubKeys

sampleKeyset :: KeySet
sampleKeyset = mkKeySet samplePubKeys "keys-all"

sampleMultiSigKeyset :: KeySet
sampleMultiSigKeyset = mkKeySet sampleMultiPubKeys "keys-all"

sampleNamespaceName :: T.Text
sampleNamespaceName = "my-namespace"

sampleNamespace :: Namespace PactValue
sampleNamespace = Namespace
                  (NamespaceName sampleNamespaceName)
                  (GKeySet sampleKeyset)
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
someModuleData = ModuleData modDef refMap mempty
  where refMap = HM.empty
        ref = Direct $ TVar (Name $ BareName "" def) def
        fst' :: Ref -> Maybe Int
        fst' = const Nothing
        scd' :: Term Ref
        scd' = TVar ref def

        scopeOfRef :: Scope Int Term Ref
        scopeOfRef = abstract fst' scd'

        defOfRef = Def (DefName "") someModuleName Defun (FunType [] TyAny) scopeOfRef def def def
        modDef = MDModule mod'
        gov = Governance $ Right defOfRef
        mod' = Module someModuleName gov def (Code "") someModuleHash HS.empty [] []
