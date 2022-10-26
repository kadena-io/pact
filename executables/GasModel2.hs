{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- #define GAS_MODEL 1

module Main where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.Decimal
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.List
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
#if defined(GAS_MODEL)
import Control.Exception (bracket)
import Criterion qualified as C
import Criterion.Types qualified as C
import Data.IORef
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Pact.GasModel.GasModel hiding (bench, benchesOnce, main)
import Pact.GasModel.Types
import Pact.GasModel.Utils
import Pact.Time
import Pact.Types.Exp qualified as Pact
import Pact.Types.Lang qualified as Pact
import Pact.Types.Runtime (EvalEnv (_eeGas))
import Pact.Types.Term (Gas)
import Statistics.Types (Estimate (..))
#endif

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
          genBuiltin "+" TInt
  forM_ xs3 $ \x3 -> do
    -- print x3
    putStrLn $ toLisp x3

  putStrLn "x4:"
  xs4 <-
    Gen.sample $
      replicateM 10 $
        flip runReaderT defaultEnv $
          genBuiltin "add-time" TTime
  forM_ xs4 $ \x4 -> do
    -- print x3
    putStrLn $ toLisp x4

#if defined(GAS_MODEL)
  let genTest name t = do
        (Pact.NativeDefName (T.pack name),)
            <$> fmap (gasTest name)
                  (Gen.sample $
                    replicateM 1 $
                      flip runReaderT defaultEnv $
                        genBuiltin name t)

  -- opt <- O.execParser options
  tests <- mapM (uncurry genTest) [("+", TInt), ("-", TInt), ("add-time", TTime), ("days", TInt), ("days", TDec)]

  -- Enforces that unit tests succeed
  putStrLn "Doing dry run of benchmark tests"
  mapM_ (mockRuns . snd) tests

  putStrLn "Running benchmark(s)"
  if True -- _oBenchOnly opt
    then
      let displayGasPrice (funName, t) = do
            res <- benchesOnce t
            putStrLn $
              (T.unpack (Pact.asString funName))
                ++ ": "
                ++ show (map _gasTestResultSqliteDb res)
            putStrLn ""
       in mapM_ displayGasPrice tests
    else do
      let testsSorted = sortOn fst tests
      allBenches <- mapM benchesMultiple testsSorted

      putStrLn "Exporting raw benchmarks data"
      writeRawCSV (concatMap snd allBenches)

      putStrLn "Exporting data-driven gas prices"
      writeGasPriceCSV allBenches

      putStrLn "Reporting coverage"
      coverageReport
#endif

#if defined(GAS_MODEL)

bench ::
  PactExpression ->
  GasSetup e ->
  IO (Gas, NanoSeconds)
bench expr dbSetup = do
  terms <- compileCode (_pactExpressionFull expr)
  putStrLn $ T.unpack (getDescription expr dbSetup)
  (gas, rep) <- bracket setup teardown $ \s@(NoopNFData (env, state)) -> do
    _ <- exec state env terms
    gas <- readIORef (_eeGas env)
    rep <- C.benchmark' (run terms s)
    pure (gas, rep)
  return
    ( gas,
      secToNs $
        estPoint $
          C.anMean $
            C.reportAnalysis rep
    )
  where
    setup = do
      s <- setupEnv dbSetup
      return $ NoopNFData s
    teardown (NoopNFData env) = do
      (gasSetupCleanup dbSetup) env
    run terms ~(NoopNFData (env, state)) =
      C.nfIO (exec state env terms)

benchesOnce ::
  GasUnitTests ->
  IO [GasTestResult (Gas, NanoSeconds)]
benchesOnce tests = runGasUnitTests tests bench mockFun
  where
    mockFun :: PactExpression -> GasSetup () -> IO (Gas, NanoSeconds)
    mockFun _ _ = pure (0, 0)

gasTest :: String -> [LispExpr] -> GasUnitTests
gasTest name exprs =
  defGasUnitTests
    ( NE.fromList
        ( map
            ( \expr ->
                PactExpression (T.pack (toLisp expr)) Nothing
            )
            exprs
        )
    )
    (Pact.NativeDefName (T.pack name))

#endif

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
  = TStr
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
  | EObject [(String, LispExpr)] -- jww (2022-09-26): should data corresponding to a scheme
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
  ETime t -> concat ["(time \"", T.unpack (Pact.formatLTime t), "\")"] -- jww (2022-09-26): TODO
  EKeyset -> "!keyset!" -- jww (2022-09-26): TODO
  EList xs -> "[" ++ intercalate ", " (map toLisp xs) ++ "]"
  EObject m -> "{ " <> intercalate ", " ["\"" <> k <> "\": " <> toLisp e | (k, e) <- m] <> " }" -- jww (2022-09-26): TODO
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

-- | from Pact.Types.Exp
genUTCTime :: MonadGen m => m UTCTime
genUTCTime = fromPosixTimestampMicros
    <$> Gen.int64 (Range.constant (-30610224000000000) 4133894400000000)

genTime :: MonadGen m => m LispExpr
genTime = ETime <$> genUTCTime

genSchema :: MonadGen m => m Schema
genSchema = 
  Gen.list (Range.exponential 0 4) ((,) <$> Gen.string (Range.linear 2 5) Gen.alpha <*> genType)

genLitType :: MonadGen m => m ExprType
genLitType = Gen.element [TStr, TInt, TDec, TBool, TTime]

genType :: MonadGen m => m ExprType
genType = go (2 :: Int)
  where
    go n =
      Gen.frequency
        [ (1, genLitType),
          -- Do not generate lists of lists greater than depth 2
          (if n > 0 then 1 else 0, TList <$> go (pred n)),
          -- jww (2022-09-26): TODO
          -- (1, pure TKeyset),
          (1, TObj <$> genSchema)
          -- (1, TTable <$> genSchema)
        ]

genExpr :: PactGen
genExpr = \case
  TStr -> genStr
  TInt -> genInt
  TDec -> genDec
  TBool -> genBool
  TTime -> genTime
  TKeyset -> pure EKeyset -- jww (2022-09-26): TODO
  TList t -> genList t
  TObj sch -> EObject <$> sequenceA [ (k,) <$> genExpr t | (k, t) <- sch ] -- jww (2022-09-26): TODO
  TTable _sch -> mzero -- pure $ ETable sch -- jww (2022-09-26): TODO

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
      ("!=", gen_neq),
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
      ("~", gen_bitwise_complement),
      -- Time native functions
      ("add-time", gen_add_time),
      ("days", gen_days) 
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

arity1 :: String -> PactGen
arity1 name t = do
  n <- genExpr t
  pure $ EParens [ESym name, n]

arity1_int_or_dec :: String -> PactGen
arity1_int_or_dec name _t =
  arity1 name =<< Gen.element [TInt, TDec]

arity2 :: String -> PactGen
arity2 name t = do
  n <- genExpr t
  m <- genExpr t
  pure $ EParens [ESym name, n, m]

arity2_int_or_dec :: String -> PactGen
arity2_int_or_dec name _t = do
  (n, m) <-
    Gen.choice
      [ (,) <$> genDec <*> genDec,
        (,) <$> genInt <*> genDec,
        (,) <$> genDec <*> genInt
      ]
  pure $ EParens [ESym name, n, m]

canEq :: ExprType -> Bool
canEq TList {} = True
canEq TObj {} = True
canEq TStr = True
canEq TInt = True
canEq TDec = True
canEq TBool = True
canEq TTime = True
canEq TTable {} = True
-- canEq TSchema {} TSchema {} = True
-- canEq TGuard {} TGuard {} = True
-- canEq TModRef {} TModRef {} = True
canEq _ = False

canCmp :: ExprType -> Bool
canCmp TStr = True
canCmp TInt = True
canCmp TDec = True
canCmp TBool = True
canCmp TTime = True
canCmp _ = False

gen_neq :: PactGen
gen_neq TBool = do
  t <- genType
  guard (canEq t)
  arity2 "!=" t
gen_neq _ = mzero

gen_bitwise_and :: PactGen
gen_bitwise_and t@TInt = arity2 "&" t
gen_bitwise_and _ = mzero

gen_mult :: PactGen
gen_mult t@TDec = arity2_int_or_dec "*" t
gen_mult t@TInt = arity2 "*" t
gen_mult _ = mzero

gen_plus :: PactGen
gen_plus t@TDec = arity2_int_or_dec "+" t
gen_plus t@TInt = arity2 "+" t
gen_plus t@TStr = arity2 "+" t
gen_plus t@(TList _) = arity2 "+" t
-- jww (2022-09-26): TODO
-- gen_plus t@(TObj _) = arity2 "+" t
gen_plus _ = mzero

gen_minus :: PactGen
gen_minus t@TDec = arity2_int_or_dec "-" t
gen_minus t@TInt = arity2 "-" t
gen_minus _ = mzero

gen_divide :: PactGen
gen_divide t@TDec = arity2_int_or_dec "/" t
gen_divide t@TInt = arity2 "/" t
gen_divide _ = mzero

gen_lt :: PactGen
gen_lt TBool = do
  t <- genType
  guard (canCmp t)
  arity2 "<" t
gen_lt _ = mzero

gen_lte :: PactGen
gen_lte TBool = do
  t <- genType
  guard (canCmp t)
  arity2 "<=" t
gen_lte _ = mzero

gen_eq :: PactGen
gen_eq TBool = do
  t <- genType
  guard (canEq t)
  arity2 "==" t
gen_eq _ = mzero

gen_gt :: PactGen
gen_gt TBool = do
  t <- genType
  guard (canCmp t)
  arity2 ">" t
gen_gt _ = mzero

gen_gte :: PactGen
gen_gte TBool = do
  t <- genType
  guard (canCmp t)
  arity2 ">=" t
gen_gte _ = mzero

gen_pow :: PactGen
gen_pow t@TInt = arity2 "^" t
gen_pow t@TDec = arity2_int_or_dec "^" t
gen_pow _ = mzero

gen_abs :: PactGen
gen_abs t@TInt = arity1 "abs" t
gen_abs t@TDec = arity1 "abs" t
gen_abs _ = mzero

gen_and :: PactGen
gen_and t@TBool = arity1 "and" t
gen_and _ = mzero

gen_and_question :: PactGen
gen_and_question _ = mzero -- jww (2022-09-26): TODO

gen_ceiling :: PactGen
gen_ceiling TInt = arity1 "ceiling" TDec
gen_ceiling _ = mzero

gen_exp :: PactGen
gen_exp t@TDec = arity1_int_or_dec "exp" t
gen_exp _ = mzero

gen_floor :: PactGen
gen_floor TInt = arity1 "floor" TDec
gen_floor _ = mzero

gen_ln :: PactGen
gen_ln t@TDec = arity1_int_or_dec "ln" t
gen_ln _ = mzero

gen_log :: PactGen
gen_log t@TInt = arity2 "log" t
gen_log t@TDec = arity2_int_or_dec "log" t
gen_log _ = mzero

gen_mod :: PactGen
gen_mod t@TInt = arity2 "mod" t
gen_mod _ = mzero

gen_not :: PactGen
gen_not t@TBool = arity1 "not" t
gen_not _ = mzero

gen_not_question :: PactGen
gen_not_question _ = mzero -- jww (2022-09-26): TODO

gen_or :: PactGen
gen_or t@TBool = arity2 "or" t
gen_or _ = mzero

gen_or_question :: PactGen
gen_or_question _ = mzero -- jww (2022-09-26): TODO

gen_round :: PactGen
gen_round TInt = arity1 "round" TDec
gen_round _ = mzero

gen_shift :: PactGen
gen_shift t@TInt = arity2 "shift" t
gen_shift _ = mzero

gen_sqrt :: PactGen
gen_sqrt t@TInt = arity1 "sqrt" t
gen_sqrt t@TDec = arity1 "sqrt" t
gen_sqrt _ = mzero

gen_xor :: PactGen
gen_xor t@TInt = arity2 "xor" t
gen_xor _ = mzero

gen_bitwise_or :: PactGen
gen_bitwise_or t@TInt = arity2 "|" t
gen_bitwise_or _ = mzero

gen_bitwise_complement :: PactGen
gen_bitwise_complement t@TInt = arity1 "~" t
gen_bitwise_complement _ = mzero

gen_add_time :: PactGen
gen_add_time TTime = do
  t <- genTime
  s <- genInt
  return $ EParens [ESym "add-time", t, s]
gen_add_time _ = mzero

gen_days :: PactGen
gen_days t@TInt = arity1 "days" t
gen_days t@TDec = arity1 "days" t
gen_days _ = mzero
