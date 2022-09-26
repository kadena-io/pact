{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Lens hiding (lifted)
import Control.Monad
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
  putStrLn "x1:"
  xs1 <-
    Gen.sample $
      replicateM 10 $
        flip runReaderT defaultEnv $
          genExpr TInt
  forM_ xs1 $ \x1 -> do
    -- print x1
    putStrLn $ toLisp x1

  putStrLn "x2:"
  xs2 <-
    Gen.sample $
      replicateM 10 $
        flip runReaderT defaultEnv $
          genBuiltin "at" TInt
  forM_ xs2 $ \x2 -> do
    -- print x2
    putStrLn $ toLisp x2

  putStrLn "x3:"
  xs3 <-
    Gen.sample $
      replicateM 10 $
        flip runReaderT defaultEnv $
          genBuiltin "+" TAny
  forM_ xs3 $ \x3 -> do
    -- print x3
    putStrLn $ toLisp x3

{-
main = defaultMain tests

tests :: TestTree
tests = undefined

basicExpr :: Property
basicExpr = undefined
-}

type PactGen = ExprType -> ReaderT Env Gen LispExpr

type Scope = HashMap String LispExpr

-- jww (2022-09-26): More things needed here
data Env = Env
  { scopes :: [Scope]
  }

defaultEnv :: Env
defaultEnv = Env []

-- Although "any" is technically a valid type, we only generate values in this
-- module whose type we know at time of generation.
data ExprType
  = TAny
  | TStr
  | TInt
  | TDec
  | TBool
  | TTime
  | TKeyset
  | TList ExprType
  | TObj Schema
  | TTable Schema
  deriving (Eq, Show)

-- jww (2022-09-26): Is this all that a scheme needs to be?
type Schema = [(String, ExprType)]

isTAny :: ExprType -> Bool
isTAny TAny = True
isTAny _ = False

isTStr :: ExprType -> Bool
isTStr TStr = True
isTStr _ = False

isTInt :: ExprType -> Bool
isTInt TInt = True
isTInt _ = False

isTDec :: ExprType -> Bool
isTDec TDec = True
isTDec _ = False

isTBool :: ExprType -> Bool
isTBool TBool = True
isTBool _ = False

isTTime :: ExprType -> Bool
isTTime TTime = True
isTTime _ = False

isTKeyset :: ExprType -> Bool
isTKeyset TKeyset = True
isTKeyset _ = False

isTList :: ExprType -> Bool
isTList (TList _) = True
isTList _ = False

isTObj :: ExprType -> Bool
isTObj (TObj _) = True
isTObj _ = False

isTTable :: ExprType -> Bool
isTTable (TTable _) = True
isTTable _ = False

data LispExpr
  = EStr String
  | EInt Integer
  | EDec Decimal
  | EBool Bool
  | ETime UTCTime
  | EKeyset
  | EList [LispExpr]
  | EObject Schema -- jww (2022-09-26): should data corresponding to a scheme
  | ETable Schema
  | EModule
  | ESym String
  | EParens [LispExpr]
  deriving (Eq, Show)

toLisp :: LispExpr -> String
toLisp = \case
  -- jww (2022-09-20): Need to escape 's'
  EStr s -> "\"" ++ s ++ "\""
  EInt i -> show i
  EDec d -> show d
  EBool True -> "true"
  EBool False -> "false"
  ETime _ -> "!time!" -- jww (2022-09-26): TODO
  EKeyset -> "!keyset!" -- jww (2022-09-26): TODO
  EList xs -> "[" ++ intercalate ", " (map toLisp xs) ++ "]"
  EObject _ -> "!object!" -- jww (2022-09-26): TODO
  ETable _ -> "!table!" -- jww (2022-09-26): TODO
  EModule -> "!module!" -- jww (2022-09-26): TODO
  ESym s -> s
  EParens xs -> "(" ++ intercalate " " (map toLisp xs) ++ ")"

pickField :: MonadGen m => Schema -> m String
pickField = Gen.element . map fst

genIdent :: MonadGen m => m String
genIdent =
  (:)
    <$> Gen.alpha
    <*> Gen.string (Range.constant 0 16) Gen.alphaNum

genStr :: MonadGen m => m LispExpr
genStr = EStr <$> Gen.string (Range.linear 0 32) Gen.alpha

genInt :: MonadGen m => m LispExpr
genInt =
  EInt
    <$> Gen.integral_
      ( Range.linear
          (-1_000_000)
          1_000_000
      )

genDec :: MonadGen m => m LispExpr
genDec =
  EDec
    <$> Gen.realFrac_
      ( Range.linearFrac
          (-1_000_000)
          1_000_000
      )

genBool :: MonadGen m => m LispExpr
genBool = EBool <$> Gen.bool

genSchema :: MonadGen m => m Schema
genSchema = pure []

genType :: MonadGen m => m ExprType
genType =
  Gen.frequency
    [ (1, pure TAny),
      (1, pure TStr),
      (1, pure TInt),
      (1, pure TDec),
      (1, pure TBool),
      (1, pure TTime),
      (1, pure TKeyset),
      (1, TList <$> genType),
      (1, TObj <$> genSchema),
      (1, TTable <$> genSchema)
    ]

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
  day <- Gen.integral_ $ Range.linear 0 (10000 :: Integer)
  sec <- Gen.integral_ $ Range.linear 0 (10_000_000 :: Integer)
  pure $ UTCTime (ModifiedJulianDay day) (fromIntegral sec)

genExpr :: PactGen
genExpr = \case
  TAny -> genExpr =<< Gen.element [TStr, TInt, TDec, TBool]
  TStr -> genStr
  TInt -> genInt
  TDec -> genDec
  TBool -> genBool
  TTime -> ETime <$> genUTCTime
  TKeyset -> pure EKeyset -- jww (2022-09-26): TODO
  TList t -> genList t
  TObj sch -> pure $ EObject sch -- jww (2022-09-26): TODO
  TTable sch -> pure $ ETable sch -- jww (2022-09-26): TODO

listRange :: Range Int
listRange = Range.constant 0 10

genList :: PactGen
genList t = EList <$> Gen.list listRange (genExpr t)

genBuiltin :: String -> PactGen
genBuiltin name t = case M.lookup name builtins of
  Just gen -> gen t
  Nothing -> fail $ "Unknown builtin: " ++ name

------------------------------------------------------------------------
-- Builtins

-- The builtins map is a mapping from function names to generators and a
-- function that report what the return type will be for a given set of
-- arguments.
builtins :: HashMap String PactGen
builtins =
  M.fromList
    [ -- General native functions
      ("at", gen_at),
      ("base64-decode", gen_base64_decode),
      ("base64-encode", gen_base64_encode),
      ("bind", gen_bind),
      ("chain-data", gen_chain_data),
      ("compose", gen_compose),
      ("concat", gen_concat),
      ("constantly", gen_constantly),
      ("contains", gen_contains),
      ("define-namespace", gen_define_namespace),
      ("drop", gen_drop),
      ("enforce", gen_enforce),
      ("enforce-one", gen_enforce_one),
      ("enforce-pact-version", gen_enforce_pact_version),
      ("enumerate", gen_enumerate),
      ("filter", gen_filter),
      ("fold", gen_fold),
      ("format", gen_format),
      ("hash", gen_hash),
      ("identity", gen_identity),
      ("if", gen_if),
      ("int-to-str", gen_int_to_str),
      ("is-charset", gen_is_charset),
      ("length", gen_length),
      ("list-modules", gen_list_modules),
      ("make-list", gen_make_list),
      ("map", gen_map),
      ("zip", gen_zip),
      ("namespace", gen_namespace),
      ("pact-id", gen_pact_id),
      ("pact-version", gen_pact_version),
      ("read-decimal", gen_read_decimal),
      ("read-integer", gen_read_integer),
      ("read-msg", gen_read_msg),
      ("read-string", gen_read_string),
      ("remove", gen_remove),
      ("resume", gen_resume),
      ("reverse", gen_reverse),
      ("sort", gen_sort),
      ("str-to-int", gen_str_to_int),
      ("str-to-list", gen_str_to_list),
      ("take", gen_take),
      ("try", gen_try),
      ("tx-hash", gen_tx_hash),
      ("typeof", gen_typeof),
      ("distinct", gen_distinct),
      ("where", gen_where),
      ("yield", gen_yield),
      -- Operators native functions
      ("!=", gen_not_equal),
      ("&", gen_bitwise_and),
      ("*", gen_mult),
      ("+", gen_plus),
      ("-", gen_minus),
      ("/", gen_divide),
      ("<", gen_lt),
      ("<=", gen_lte),
      ("=", gen_eq),
      (">", gen_gt),
      (">=", gen_gte),
      ("^", gen_pow),
      ("abs", gen_abs),
      ("and", gen_and),
      ("and?", gen_and_question),
      ("ceiling", gen_ceiling),
      ("exp", gen_exp),
      ("floor", gen_floor),
      ("ln", gen_ln),
      ("log", gen_log),
      ("mod", gen_mod),
      ("not", gen_not),
      ("not?", gen_not_question),
      ("or", gen_or),
      ("or?", gen_or_question),
      ("round", gen_round),
      ("shift", gen_shift),
      ("sqrt", gen_sqrt),
      ("xor", gen_xor),
      ("|", gen_bitwise_or),
      ("~", gen_bitwise_complement)
      -- Time native functions
      -- "add-time"    -> Just $ addTimeTests nativeName
      -- "days"        -> Just $ daysTests nativeName
      -- "diff-time"   -> Just $ diffTimeTests nativeName
      -- "format-time" -> Just $ formatTimeTests nativeName
      -- "hours"       -> Just $ hoursTests nativeName
      -- "minutes"     -> Just $ minutesTests nativeName
      -- "parse-time"  -> Just $ parseTimeTests nativeName
      -- "time"        -> Just $ timeTests nativeName

      -- Commitments native functions
      -- "decrypt-cc20p1305" -> Just $ decryptCc20p1305Tests nativeName
      -- "validate-keypair"  -> Just $ validateKeypairTests nativeName

      -- Keyset native functions
      -- "define-keyset"  -> Just $ defineKeysetTests nativeName
      -- "enforce-keyset" -> Just $ enforceKeysetTests nativeName
      -- "keys-2"         -> Just $ keys2Tests nativeName
      -- "keys-all"       -> Just $ keysAllTests nativeName
      -- "keys-any"       -> Just $ keysAnyTests nativeName
      -- "read-keyset"    -> Just $ readKeysetTests nativeName

      -- Database native functions
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

      -- Capabilities native functions
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

      -- Principal creation and validation
      -- "create-principal"   -> Just $ createPrincipalTests nativeName
      -- "validate-principal" -> Just $ validatePrincipalTests nativeName
      -- "is-principal"       -> Just $ isPrincipalTests nativeName
      -- "typeof-principal"   -> Just $ typeofPrincipalTests nativeName

      -- Non-native concepts to benchmark
      -- "use"       -> Just $ useTests nativeName
      -- "module"    -> Just $ moduleTests nativeName
      -- "interface" -> Just $ interfaceTests nativeName
    ]

gen_at :: PactGen
gen_at t = do
  l@(EList xs) <- genList t
  guard $ length xs > 0
  i <- Gen.integral $ Range.constant 0 (pred (length xs))
  pure $ EParens [ESym "at", EInt (fromIntegral i), l]

gen_base64_decode :: PactGen
gen_base64_decode t@TStr = do
  x <- genExpr t
  pure $ EParens [ESym "base64-decode", x]
gen_base64_decode _ = mzero

gen_base64_encode :: PactGen
gen_base64_encode _ = mzero -- jww (2022-09-26): TODO

gen_bind :: PactGen
gen_bind _ = mzero -- jww (2022-09-26): TODO

gen_chain_data :: PactGen
gen_chain_data _ = mzero -- jww (2022-09-26): TODO

gen_compose :: PactGen
gen_compose _ = mzero -- jww (2022-09-26): TODO

gen_concat :: PactGen
gen_concat _ = mzero -- jww (2022-09-26): TODO

gen_constantly :: PactGen
gen_constantly _ = mzero -- jww (2022-09-26): TODO

gen_contains :: PactGen
gen_contains _ = mzero -- jww (2022-09-26): TODO

gen_define_namespace :: PactGen
gen_define_namespace _ = mzero -- jww (2022-09-26): TODO

gen_drop :: PactGen
gen_drop _ = mzero -- jww (2022-09-26): TODO

gen_enforce :: PactGen
gen_enforce _ = mzero -- jww (2022-09-26): TODO

gen_enforce_one :: PactGen
gen_enforce_one _ = mzero -- jww (2022-09-26): TODO

gen_enforce_pact_version :: PactGen
gen_enforce_pact_version _ = mzero -- jww (2022-09-26): TODO

gen_enumerate :: PactGen
gen_enumerate _ = mzero -- jww (2022-09-26): TODO

gen_filter :: PactGen
gen_filter _ = mzero -- jww (2022-09-26): TODO

gen_fold :: PactGen
gen_fold _ = mzero -- jww (2022-09-26): TODO

gen_format :: PactGen
gen_format _ = mzero -- jww (2022-09-26): TODO

gen_hash :: PactGen
gen_hash _ = mzero -- jww (2022-09-26): TODO

gen_identity :: PactGen
gen_identity _ = mzero -- jww (2022-09-26): TODO

gen_if :: PactGen
gen_if _ = mzero -- jww (2022-09-26): TODO

gen_int_to_str :: PactGen
gen_int_to_str _ = mzero -- jww (2022-09-26): TODO

gen_is_charset :: PactGen
gen_is_charset _ = mzero -- jww (2022-09-26): TODO

gen_length :: PactGen
gen_length _ = mzero -- jww (2022-09-26): TODO

gen_list_modules :: PactGen
gen_list_modules _ = mzero -- jww (2022-09-26): TODO

gen_make_list :: PactGen
gen_make_list _ = mzero -- jww (2022-09-26): TODO

gen_map :: PactGen
gen_map _ = mzero -- jww (2022-09-26): TODO

gen_zip :: PactGen
gen_zip _ = mzero -- jww (2022-09-26): TODO

gen_namespace :: PactGen
gen_namespace _ = mzero -- jww (2022-09-26): TODO

gen_pact_id :: PactGen
gen_pact_id _ = mzero -- jww (2022-09-26): TODO

gen_pact_version :: PactGen
gen_pact_version _ = mzero -- jww (2022-09-26): TODO

gen_read_decimal :: PactGen
gen_read_decimal _ = mzero -- jww (2022-09-26): TODO

gen_read_integer :: PactGen
gen_read_integer _ = mzero -- jww (2022-09-26): TODO

gen_read_msg :: PactGen
gen_read_msg _ = mzero -- jww (2022-09-26): TODO

gen_read_string :: PactGen
gen_read_string _ = mzero -- jww (2022-09-26): TODO

gen_remove :: PactGen
gen_remove _ = mzero -- jww (2022-09-26): TODO

gen_resume :: PactGen
gen_resume _ = mzero -- jww (2022-09-26): TODO

gen_reverse :: PactGen
gen_reverse _ = mzero -- jww (2022-09-26): TODO

gen_sort :: PactGen
gen_sort _ = mzero -- jww (2022-09-26): TODO

gen_str_to_int :: PactGen
gen_str_to_int _ = mzero -- jww (2022-09-26): TODO

gen_str_to_list :: PactGen
gen_str_to_list _ = mzero -- jww (2022-09-26): TODO

gen_take :: PactGen
gen_take _ = mzero -- jww (2022-09-26): TODO

gen_try :: PactGen
gen_try _ = mzero -- jww (2022-09-26): TODO

gen_tx_hash :: PactGen
gen_tx_hash _ = mzero -- jww (2022-09-26): TODO

gen_typeof :: PactGen
gen_typeof _ = mzero -- jww (2022-09-26): TODO

gen_distinct :: PactGen
gen_distinct _ = mzero -- jww (2022-09-26): TODO

gen_where :: PactGen
gen_where _ = mzero -- jww (2022-09-26): TODO

gen_yield :: PactGen
gen_yield _ = mzero -- jww (2022-09-26): TODO

gen_not_equal :: PactGen
gen_not_equal _ = mzero -- jww (2022-09-26): TODO

gen_bitwise_and :: PactGen
gen_bitwise_and _ = mzero -- jww (2022-09-26): TODO

gen_mult :: PactGen
gen_mult _ = mzero -- jww (2022-09-26): TODO

gen_plus :: PactGen
gen_plus TAny =
  gen_plus
    =<< Gen.choice
      [ pure TDec,
        pure TInt,
        pure TStr,
        TList <$> genType,
        TObj <$> genSchema
      ]
gen_plus TDec = do
  (n, m) <-
    Gen.choice
      [ (,) <$> genDec <*> genDec,
        (,) <$> genInt <*> genDec,
        (,) <$> genDec <*> genInt
      ]
  pure $ EParens [ESym "+", n, m]
gen_plus t@TInt = gen_plus_work t
gen_plus t@TStr = gen_plus_work t
gen_plus t@(TList _) = gen_plus_work t
gen_plus t@(TObj _) = gen_plus_work t
gen_plus _ = mzero

gen_plus_work :: PactGen
gen_plus_work t = do
  n <- genExpr t
  m <- genExpr t
  pure $ EParens [ESym "+", n, m]

gen_minus :: PactGen
gen_minus _ = mzero -- jww (2022-09-26): TODO

gen_divide :: PactGen
gen_divide _ = mzero -- jww (2022-09-26): TODO

gen_lt :: PactGen
gen_lt _ = mzero -- jww (2022-09-26): TODO

gen_lte :: PactGen
gen_lte _ = mzero -- jww (2022-09-26): TODO

gen_eq :: PactGen
gen_eq _ = mzero -- jww (2022-09-26): TODO

gen_gt :: PactGen
gen_gt _ = mzero -- jww (2022-09-26): TODO

gen_gte :: PactGen
gen_gte _ = mzero -- jww (2022-09-26): TODO

gen_pow :: PactGen
gen_pow _ = mzero -- jww (2022-09-26): TODO

gen_abs :: PactGen
gen_abs _ = mzero -- jww (2022-09-26): TODO

gen_and :: PactGen
gen_and _ = mzero -- jww (2022-09-26): TODO

gen_and_question :: PactGen
gen_and_question _ = mzero -- jww (2022-09-26): TODO

gen_ceiling :: PactGen
gen_ceiling _ = mzero -- jww (2022-09-26): TODO

gen_exp :: PactGen
gen_exp _ = mzero -- jww (2022-09-26): TODO

gen_floor :: PactGen
gen_floor _ = mzero -- jww (2022-09-26): TODO

gen_ln :: PactGen
gen_ln _ = mzero -- jww (2022-09-26): TODO

gen_log :: PactGen
gen_log _ = mzero -- jww (2022-09-26): TODO

gen_mod :: PactGen
gen_mod _ = mzero -- jww (2022-09-26): TODO

gen_not :: PactGen
gen_not _ = mzero -- jww (2022-09-26): TODO

gen_not_question :: PactGen
gen_not_question _ = mzero -- jww (2022-09-26): TODO

gen_or :: PactGen
gen_or _ = mzero -- jww (2022-09-26): TODO

gen_or_question :: PactGen
gen_or_question _ = mzero -- jww (2022-09-26): TODO

gen_round :: PactGen
gen_round _ = mzero -- jww (2022-09-26): TODO

gen_shift :: PactGen
gen_shift _ = mzero -- jww (2022-09-26): TODO

gen_sqrt :: PactGen
gen_sqrt _ = mzero -- jww (2022-09-26): TODO

gen_xor :: PactGen
gen_xor _ = mzero -- jww (2022-09-26): TODO

gen_bitwise_or :: PactGen
gen_bitwise_or _ = mzero -- jww (2022-09-26): TODO

gen_bitwise_complement :: PactGen
gen_bitwise_complement _ = mzero -- jww (2022-09-26): TODO
