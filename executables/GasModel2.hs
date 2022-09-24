{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens hiding (lifted)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Data.Decimal
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.List
import Data.Time
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- import Test.Tasty

-- import Test.Tasty.Hedgehog

main :: IO ()
main = do
  x1 <-
    Gen.sample $
      flip runReaderT [] $
        genExpr TInt
  putStrLn "x1:"
  print x1
  putStrLn $ toLisp (expr x1)

  x2 <-
    Gen.sample $
      flip runReaderT [] $
        genBuiltin "at"
  putStrLn "x2:"
  print x2
  putStrLn $ toLisp (expr x2)

{-
main = defaultMain tests

tests :: TestTree
tests = undefined

basicExpr :: Property
basicExpr = undefined
-}

type Schema = [(String, ExprType)]

-- Although "any" is technically a valid type, we only generate values in this
-- module whose type we know at time of generation.
data ExprType
  = TStr
  | TInt
  | TDec
  | TBool
  | TTime
  | TKeyset
  | TList (Maybe ExprType)
  | TObject Schema
  | TTable Schema
  | TFunction
      { funName :: String,
        funArity :: Int
      }
  deriving (Eq, Show)

data LispExpr
  = EStr String
  | EInt Integer
  | EDec Decimal
  | EBool Bool
  | ETime UTCTime
  | EKeyset
  | EList [TypedExpr]
  | EObject Schema
  | ETable Schema
  | EModule
  | ESym String
  | EParens [TypedExpr]
  deriving (Eq, Show)

data TypedExpr = TypedExpr
  { exprType :: ExprType,
    expr :: LispExpr
  }
  deriving (Eq, Show)

typeOf :: TypedExpr -> ExprType
typeOf (TypedExpr ty _) = ty

listOf :: TypedExpr -> [TypedExpr]
listOf (TypedExpr _ (EList xs)) = xs
listOf t = error $ "Expected list, but got: " ++ show t

data Scope = Scope
  { symbols :: HashMap String TypedExpr
  }

toLisp :: LispExpr -> String
toLisp = \case
  -- jww (2022-09-20): Need to escape 's'
  EStr s -> "\"" ++ s ++ "\""
  EInt i -> show i
  EDec d -> show d
  EBool True -> "true"
  EBool False -> "false"
  ETime _ -> undefined
  EKeyset -> undefined
  EList xs -> "[" ++ intercalate ", " (map (toLisp . expr) xs) ++ "]"
  EObject _ -> undefined
  ETable _ -> undefined
  EModule -> undefined
  ESym s -> s
  EParens xs -> "(" ++ intercalate " " (map (toLisp . expr) xs) ++ ")"

type PactGen = ReaderT [Scope] Gen

type PactGenCPS a = ContT a PactGen (PactGen TypedExpr)

lifted :: PactGen a -> PactGenCPS a
lifted = ContT . const

runCPS :: PactGenCPS a -> (PactGen TypedExpr -> PactGen a) -> PactGen a
runCPS = runContT

pickField :: Schema -> Gen String
pickField fields = Gen.element (map fst fields)

genIdent :: Gen String
genIdent =
  (:)
    <$> Gen.alpha
    <*> Gen.string (Range.linear 0 16) Gen.alphaNum

genStr :: Gen TypedExpr
genStr = TypedExpr TStr . EStr <$> Gen.string (Range.linear 0 8192) Gen.alpha

genInt :: Gen TypedExpr
genInt =
  TypedExpr TInt . EInt
    <$> Gen.integral
      ( Range.linear
          (-1_000_000)
          1_000_000
      )

genDec :: Gen TypedExpr
genDec =
  TypedExpr TDec . EDec
    <$> Gen.realFrac_
      ( Range.linearFrac
          (-1_000_000)
          1_000_000
      )

genBool :: Gen TypedExpr
genBool = TypedExpr TBool . EBool <$> Gen.bool

-- A functional expression has the form (FUNCTION ARGS..), while a data
-- expression has the form (EXPR...). A generated symbol should always refer
-- to a bound name, but value arguments can appear anywhere.
data ExprKind = Functional | Value

genExpr :: ExprType -> PactGen TypedExpr
genExpr TStr = lift genStr
genExpr TInt = lift genInt
genExpr TDec = lift genDec
genExpr TBool = lift genBool
genExpr TTime = undefined
genExpr TKeyset = undefined
genExpr (TList Nothing) = genList
genExpr (TList (Just ty)) = genArray ty
genExpr (TObject _sch) = undefined
genExpr (TTable _sch) = undefined
genExpr (TFunction name _) = genBuiltin name

genAtomType :: Gen ExprType
genAtomType = Gen.element [TStr, TInt, TDec, TBool]

listRange :: Range Int
listRange = Range.linear 0 10

genArray :: ExprType -> PactGen TypedExpr
genArray t = do
  TypedExpr (TList (Just t)) . EList
    <$> Gen.list listRange (genExpr t)

genList :: PactGen TypedExpr
genList = do
  TypedExpr (TList Nothing) . EList
    <$> Gen.list
      listRange
      ( do
          t <- lift genAtomType
          genExpr t
      )

genBuiltin :: String -> PactGen TypedExpr
genBuiltin name = builtins ^?! ix name . _2

builtins :: HashMap String (ExprType, PactGen TypedExpr)
builtins = go [(builtin_at, at_impl)]
  where
    go =
      M.fromList
        . map
          ( \x@(t, _) -> case t of
              TFunction name _ -> (name, x)
              _ -> error "unexpected"
          )

-- General native functions
-- "at"                   -> Just $ atTests nativeName
-- "base64-decode"        -> Just $ base64DecodeTests nativeName
-- "base64-encode"        -> Just $ base64EncodeTests nativeName
-- "bind"                 -> Just $ bindTests nativeName
-- "chain-data"           -> Just $ chainDataTests nativeName
-- "compose"              -> Just $ composeTests nativeName
-- "concat"               -> Just $ concatTests nativeName
-- "constantly"           -> Just $ constantlyTests nativeName
-- "contains"             -> Just $ containsTests nativeName
-- "define-namespace"     -> Just $ defineNamespaceTests nativeName
-- "drop"                 -> Just $ dropTests nativeName
-- "enforce"              -> Just $ enforceTests nativeName
-- "enforce-one"          -> Just $ enforceOneTests nativeName
-- "enforce-pact-version" -> Just $ enforcePactVersionTests nativeName
-- "enumerate"            -> Just $ enumerateTests nativeName
-- "filter"               -> Just $ filterTests nativeName
-- "fold"                 -> Just $ foldTests nativeName
-- "format"               -> Just $ formatTests nativeName
-- "hash"                 -> Just $ hashTests nativeName
-- "identity"             -> Just $ identityTests nativeName
-- "if"                   -> Just $ ifTests nativeName
-- "int-to-str"           -> Just $ intToStrTests nativeName
-- "is-charset"           -> Just $ isCharsetTests nativeName
-- "length"               -> Just $ lengthTests nativeName
-- "list-modules"         -> Just $ listModulesTests nativeName
-- "make-list"            -> Just $ makeListTests nativeName
-- "map"                  -> Just $ mapTests nativeName
-- "zip"                  -> Just $ zipTests nativeName
-- "namespace"            -> Just $ namespaceTests nativeName
-- "pact-id"              -> Just $ pactIdTests nativeName
-- "pact-version"         -> Just $ pactVersionTests nativeName
-- "read-decimal"         -> Just $ readDecimalTests nativeName
-- "read-integer"         -> Just $ readIntegerTests nativeName
-- "read-msg"             -> Just $ readMsgTests nativeName
-- "read-string"          -> Just $ readStringTests nativeName
-- "remove"               -> Just $ removeTests nativeName
-- "resume"               -> Just $ resumeTests nativeName
-- "reverse"              -> Just $ reverseTests nativeName
-- "sort"                 -> Just $ sortTests nativeName
-- "str-to-int"           -> Just $ strToIntTests nativeName
-- "str-to-list"          -> Just $ strToListTests nativeName
-- "take"                 -> Just $ takeTests nativeName
-- "try"                  -> Just $ tryTests nativeName
-- "tx-hash"              -> Just $ txHashTests nativeName
-- "typeof"               -> Just $ typeOfTests nativeName
-- "distinct"             -> Just $ distinctTests nativeName
-- "where"                -> Just $ whereTests nativeName
-- "yield"                -> Just $ yieldTests nativeName

-- -- Operators native functions
-- "!="      -> Just $ notEqualOptTests nativeName
-- "&"       -> Just $ bitwiseOptTests nativeName
-- "*"       -> Just $ multOptTests nativeName
-- "+"       -> Just $ addOptTests nativeName
-- "-"       -> Just $ subOptTests nativeName
-- "/"       -> Just $ divOptTests nativeName
-- "<"       -> Just $ lessThanOptTests nativeName
-- "<="      -> Just $ lessThanEqualOptTests nativeName
-- "="       -> Just $ equalOptTests nativeName
-- ">"       -> Just $ greaterThanOptTests nativeName
-- ">="      -> Just $ greaterThanEqOptTests nativeName
-- "^"       -> Just $ raiseOptTests nativeName
-- "abs"     -> Just $ absOptTests nativeName
-- "and"     -> Just $ andOptTests nativeName
-- "and?"    -> Just $ andFuncOptTests nativeName
-- "ceiling" -> Just $ ceilingOptTests nativeName
-- "exp"     -> Just $ expOptTests nativeName
-- "floor"   -> Just $ floorOptTests nativeName
-- "ln"      -> Just $ lnOptTests nativeName
-- "log"     -> Just $ logOptTests nativeName
-- "mod"     -> Just $ modOptTests nativeName
-- "not"     -> Just $ notOptTests nativeName
-- "not?"    -> Just $ notFuncOptTests nativeName
-- "or"      -> Just $ orOptTests nativeName
-- "or?"     -> Just $ orFuncOptTests nativeName
-- "round"   -> Just $ roundOptTests nativeName
-- "shift"   -> Just $ shiftOptTests nativeName
-- "sqrt"    -> Just $ sqrtOptTests nativeName
-- "xor"     -> Just $ xorOptTests nativeName
-- "|"       -> Just $ bitwiseOrOptTests nativeName
-- "~"       -> Just $ reverseBitsOptTests nativeName

-- -- Time native functions
-- "add-time"    -> Just $ addTimeTests nativeName
-- "days"        -> Just $ daysTests nativeName
-- "diff-time"   -> Just $ diffTimeTests nativeName
-- "format-time" -> Just $ formatTimeTests nativeName
-- "hours"       -> Just $ hoursTests nativeName
-- "minutes"     -> Just $ minutesTests nativeName
-- "parse-time"  -> Just $ parseTimeTests nativeName
-- "time"        -> Just $ timeTests nativeName

-- -- Commitments native functions
-- "decrypt-cc20p1305" -> Just $ decryptCc20p1305Tests nativeName
-- "validate-keypair"  -> Just $ validateKeypairTests nativeName

-- -- Keyset native functions
-- "define-keyset"  -> Just $ defineKeysetTests nativeName
-- "enforce-keyset" -> Just $ enforceKeysetTests nativeName
-- "keys-2"         -> Just $ keys2Tests nativeName
-- "keys-all"       -> Just $ keysAllTests nativeName
-- "keys-any"       -> Just $ keysAnyTests nativeName
-- "read-keyset"    -> Just $ readKeysetTests nativeName

-- -- Database native functions
-- "create-table"       -> Just $ createTableTests nativeName
-- "describe-keyset"    -> Just $ describeKeysetTests nativeName
-- "describe-module"    -> Just $ describeModuleTests nativeName
-- "describe-table"     -> Just $ describeTableTests nativeName
-- "describe-namespace" -> Just $ describeNamespaceTests nativeName
-- "insert"             -> Just $ insertTests nativeName
-- "keylog"             -> Just $ keylogTests nativeName
-- "keys"               -> Just $ keysTests nativeName
-- "read"               -> Just $ readTests nativeName
-- "select"             -> Just $ selectTests nativeName
-- "txids"              -> Just $ txidsTests nativeName
-- "txlog"              -> Just $ txlogTests nativeName
-- "update"             -> Just $ updateTests nativeName
-- "with-default-read"  -> Just $ withDefaultReadTests nativeName
-- "with-read"          -> Just $ withReadTests nativeName
-- "write"              -> Just $ writeTests nativeName
-- "fold-db"            -> Just $ foldDBTests nativeName

-- -- Capabilities native functions
-- "compose-capability"  -> Just $ composeCapabilityTests nativeName
-- "create-module-guard" -> Just $ createModuleGuardTests nativeName
-- "create-pact-guard"   -> Just $ createPactGuardTests nativeName
-- "create-user-guard"   -> Just $ createUserGuardTests nativeName
-- "enforce-guard"       -> Just $ enforceGuardTests nativeName
-- "install-capability"  -> Just $ installCapabilityTests nativeName
-- "keyset-ref-guard"    -> Just $ keysetRefGuardTests nativeName
-- "require-capability"  -> Just $ requireCapabilityTests nativeName
-- "with-capability"     -> Just $ withCapabilityTests nativeName
-- "emit-event"          -> Just $ emitEventTests nativeName

-- -- Principal creation and validation
-- "create-principal"   -> Just $ createPrincipalTests nativeName
-- "validate-principal" -> Just $ validatePrincipalTests nativeName
-- "is-principal"       -> Just $ isPrincipalTests nativeName
-- "typeof-principal"   -> Just $ typeofPrincipalTests nativeName

-- -- Non-native concepts to benchmark
-- "use"       -> Just $ useTests nativeName
-- "module"    -> Just $ moduleTests nativeName
-- "interface" -> Just $ interfaceTests nativeName

builtin_at :: ExprType
builtin_at =
  TFunction
    { funName = "at",
      funArity = 2
    }

at_impl :: PactGen TypedExpr
at_impl = do
  Gen.frequency
    [ ( 1,
        do
          t <- lift genAtomType
          x <- genArray t
          i <- Gen.integral $ Range.linear 0 (genericLength (listOf x))
          pure $
            TypedExpr t $
              EParens
                [ TypedExpr builtin_at (ESym "at"),
                  TypedExpr TInt (EInt i),
                  x
                ]
      )
    ]
