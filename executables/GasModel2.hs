{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Trans.Reader
import Criterion qualified as C
import Criterion.Types qualified as C
import Data.Decimal
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.IORef
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pact.GasModel.GasModel hiding (bench, benchesOnce, main)
import Pact.GasModel.Types
import Pact.GasModel.Utils
import Pact.Types.Lang qualified as Pact
import Pact.Types.Runtime (EvalEnv (_eeGas))
import Pact.Types.Term (Gas)
import Pact.Types.Util
import Statistics.Types (Estimate (..))

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
          genBuiltinByName "at" TInt
  forM_ xs2 $ \x2 -> do
    -- print x2
    putStrLn $ toLisp x2

  putStrLn "x3:"
  xs3 <-
    Gen.sample $
      replicateM 10 $
        flip runReaderT defaultEnv $
          genBuiltinByName "+" TInt
  forM_ xs3 $ \x3 -> do
    -- print x3
    putStrLn $ toLisp x3

  let genTest name = do
        (Pact.NativeDefName (T.pack name),)
            <$> fmap (gasTest name)
                  (Gen.sample $
                    replicateM 1 $
                      flip runReaderT defaultEnv $
                        genBuiltinByName name TInt)

  -- opt <- O.execParser options
  tests <- mapM genTest ["+", "-"]

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

single :: PactGen -> ExprType -> IO ()
single g t = do
  expr <- Gen.sample $ runReaderT (g t) defaultEnv
  putStrLn $ toLisp expr

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

type PactGen = ExprType -> ReaderT Env Gen LispExpr

type Scope = HashMap String LispExpr

data Env = Env
  { scopes :: [Scope],
    depth :: Int -- how deeply can expressions be nested?
  }

defaultEnv :: Env
defaultEnv = Env [] 3

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
  | TArrow [ExprType] ExprType
  deriving (Eq, Show)

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
  | EObj [(String, LispExpr)]
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
  EObj sch ->
    "{" ++ emitFields sch ++ " }"
    where
      emitFields [] = ""
      emitFields ((f,x):[]) =
        " \"" ++ f ++ "\": " ++ toLisp x
      emitFields ((f,x):fs) =
        " \"" ++ f ++ "\": " ++ toLisp x ++ "," ++ emitFields fs
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
genInt = do
  b <- Gen.bool
  if b
    then
      EInt
        <$> Gen.integral_
          ( Range.linear
              0
              1_000_000
          )
    else
      EInt
        <$> Gen.integral_
          ( Range.linear
              (-1_000_000)
              1_000_000
          )

genDec :: MonadGen m => m LispExpr
genDec = do
  b <- Gen.bool
  if b
    then
      EDec
        <$> Gen.realFrac_
          ( Range.linearFrac
              0
              1_000_000
          )
    else
      EDec
        <$> Gen.realFrac_
          ( Range.linearFrac
              (-1_000_000)
              1_000_000
          )

genBool :: MonadGen m => m LispExpr
genBool = EBool <$> Gen.bool

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = do
  day <- Gen.integral_ $ Range.linear 0 (10000 :: Integer)
  sec <- Gen.integral_ $ Range.linear 0 (10_000_000 :: Integer)
  pure $ UTCTime (ModifiedJulianDay day) (fromIntegral sec)

genTime :: MonadGen m => m LispExpr
genTime = ETime <$> genUTCTime

genSchema :: MonadGen m => m Schema
genSchema =
  Gen.list (Range.exponential 0 4)
    ((,) <$> Gen.string (Range.linear 2 5) Gen.alpha <*> genType)

genObj :: Schema -> ReaderT Env Gen LispExpr
genObj sch = ReaderT $ \env ->
  EObj <$> traverse (\(fld, ty) -> (fld,) <$> runReaderT (genExpr ty) env) sch

genLitType :: MonadGen m => m ExprType
genLitType = Gen.element [TStr, TInt, TDec, TBool]

genType :: MonadGen m => m ExprType
genType = go (2 :: Int)
  where
    go n =
      Gen.frequency
        [ (1, genLitType),
          -- Do not generate lists of lists greater than depth 2
          (if n > 0 then 1 else 0, TList <$> go (pred n)),
          -- (1, pure TKeyset), -- jww (2022-09-26): TODO
          (1, TObj <$> genSchema)
          -- (1, TTable <$> genSchema) -- jww (2022-09-26): TODO
        ]

genAtom :: PactGen
genAtom = \case
  TStr -> genStr
  TInt -> genInt
  TDec -> genDec
  TBool -> genBool
  TTime -> genTime
  TKeyset -> pure EKeyset -- jww (2022-09-26): TODO
  TList t -> genList t
  TObj sch -> genObj sch
  -- elp (2022-10-26): should this even exist?
  TTable sch -> pure $ ETable sch -- jww (2022-09-26): TODO
  TArrow doms cod -> genArrow doms cod

genExpr :: PactGen
genExpr t = do
  env <- ask
  if depth env == 0
    then genAtom t
    else do
      EBool b <- genBool
      if b
        then genAtom t
        else local (\e -> e { depth = depth e - 1 }) $
          genBuiltin t

listRange :: Range Int
listRange = Range.constant 0 10

genList :: PactGen
genList t = EList <$> Gen.list listRange (genExpr t)

genArrow :: [ExprType] -> PactGen
genArrow _doms _cod = error "jww (2022-12-06): genArrow NYI"

genBuiltinByName :: String -> PactGen
genBuiltinByName name t = case M.lookup name builtins of
  Just gen -> gen t
  Nothing -> fail $ "Unknown builtin: " ++ name

genBuiltin :: PactGen
genBuiltin t = case t of
  TStr ->
    Gen.choice
      [ gen_at t
      , gen_base64_decode t
      , gen_base64_encode t
      , gen_concat t
      , gen_constantly t
      , gen_drop t
      , gen_fold t
      , gen_format t
      , gen_hash t
      , gen_identity t
      , gen_if t
      , gen_int_to_str t
      , gen_namespace t
      , gen_pact_id t
      , gen_pact_version t
      , gen_read_msg t
      , gen_read_string t
      , gen_take t
      , gen_try t
      , gen_tx_hash t
      , gen_typeof t
      --
      , gen_plus t
      ]
  TInt ->
    Gen.choice
      [ gen_at t
      , gen_constantly t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_length t
      , gen_str_to_int t
      , gen_read_msg t
      , gen_try t
      --
      , gen_bitwise_and t
      , gen_mult t
      , gen_plus t
      , gen_minus t
      , gen_divide t
      , gen_pow t
      , gen_abs t
      , gen_ceiling t
      , gen_exp t
      , gen_floor t
      , gen_ln t
      , gen_log t
      , gen_mod t
      , gen_round t
      , gen_shift t
      , gen_sqrt t
      , gen_xor t
      , gen_bitwise_or t
      , gen_bitwise_complement t
      , gen_days t
      ]
  TDec ->
    Gen.choice
      [ gen_at t
      , gen_constantly t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_read_decimal t
      , gen_read_msg t
      , gen_try t
      --
      , gen_mult t
      , gen_plus t
      , gen_minus t
      , gen_divide t
      , gen_pow t
      , gen_abs t
      , gen_exp t
      , gen_ln t
      , gen_log t
      , gen_sqrt t
      , gen_days t
      ]
  TBool ->
    Gen.choice
      [ gen_at t
      , gen_constantly t
      , gen_contains t
      , gen_enforce t
      , gen_enforce_one t
      , gen_enforce_pact_version t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_is_charset t
      , gen_read_msg t
      , gen_try t
      , gen_not t
      --
      , gen_neq t
      , gen_lt t
      , gen_lte t
      , gen_eq t
      , gen_gt t
      , gen_gte t
      , gen_and t
      , gen_or t
      ]
  TTime ->
    Gen.choice
      [ gen_at t
      , gen_constantly t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_read_msg t
      , gen_try t
      --
      , gen_add_time t
      ]
  TKeyset ->
    Gen.choice
      [ gen_at t
      , gen_constantly t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_read_msg t
      , gen_try t
      ]
  TList _ ->
    Gen.choice $
      [ gen_at t
      , gen_constantly t
      , gen_drop t
      , gen_filter t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_make_list t
      , gen_map t
      , gen_zip t
      , gen_read_msg t
      , gen_reverse t
      , gen_sort t
      , gen_take t
      , gen_try t
      , gen_distinct t
      --
      , gen_plus t
      ]
      ++ [ gen_enumerate t    | t == TInt ]
      ++ [ gen_list_modules t | t == TStr ]
      ++ [ gen_str_to_list t  | t == TStr ]
  TObj _ ->
    Gen.choice
      [ gen_at t
      , gen_constantly t
      , gen_bind t
      , gen_chain_data t
      , gen_define_namespace t
      , gen_drop t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_read_msg t
      , gen_remove t
      , gen_sort t
      , gen_take t
      , gen_try t
      ]
  TTable _ ->
    Gen.choice
      [ gen_at t
      , gen_constantly t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_read_msg t
      , gen_try t
      ]
  TArrow _ _ ->
    Gen.choice
      [ gen_compose t
      , gen_constantly t
      , gen_fold t
      , gen_identity t
      , gen_if t
      , gen_read_msg t
      , gen_try t
      ]

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

--NOTED
gen_at :: PactGen
gen_at t = do
  l@(EList xs) <- genExpr (TList t)
  guard $ length xs > 0
  i <- Gen.integral $ Range.constant 0 (pred (length xs))
  pure $ EParens [ESym "at", EInt (fromIntegral i), l]

--NOTED
gen_base64_decode :: PactGen
gen_base64_decode t@TStr = do
  -- elp (2022-10-24):
  --
  -- In order to properly stress this, we have to generate 2 strings: one (the
  -- first part) encoded as valid base64, and then the second being
  -- (potential) garbage so that we can stress the error path more
  -- effectively.
  EStr x <- genExpr t
  EStr y <- genExpr t
  let z =
        Text.unpack
          . toB64UrlUnpaddedText
          . Text.encodeUtf8
          . Text.pack
          $ x
  pure $ EParens [ESym "base64-decode", EStr $ z ++ y]
gen_base64_decode _ = mzero

--NOTED
gen_base64_encode :: PactGen
gen_base64_encode t@TStr = do
  x <- genExpr t
  pure $ EParens [ESym "base64-encode", x]
gen_base64_encode _ = mzero

--NOTED
gen_bind :: PactGen
gen_bind t@TObj {} = do
  x <- genExpr t
  y <- genExpr t
  z <- genExpr =<< genType
  pure $ EParens [ESym "bind", x, y, z]
gen_bind _ = mzero

public_chain_data :: Schema
public_chain_data = error "jww (2022-12-07): public_chain_data NYI"

--NOTED
gen_chain_data :: PactGen
gen_chain_data (TObj sch) | sch == public_chain_data =
  pure $ EParens [ESym "chain-data"]
gen_chain_data _ = mzero

--NOTED
gen_compose :: PactGen
gen_compose (TArrow [a] c) = do
  b :: ExprType <- genType
  g <- genExpr (TArrow [a] b)
  f <- genExpr (TArrow [b] c)
  pure $ EParens [ESym "compose", f, g]
gen_compose _ = mzero

--NOTED
gen_concat :: PactGen
gen_concat t@TStr = do
  x <- genExpr (TList t)
  y <- genExpr (TList t)
  pure $ EParens [ESym "concat", x, y]
gen_concat _ = mzero

--NOTED
gen_constantly :: PactGen
gen_constantly t = do
  x <- genExpr t
  y <- genType >>= genExpr
  pure $ EParens [ESym "constantly", x, y]

--NOTED
gen_contains :: PactGen
gen_contains TBool = do
  x <- genType >>= genExpr
  a <- genType >>= genExpr
  pure $ EParens [ESym "contains", a, x]
gen_contains _ = mzero

--NOTED
gen_define_namespace :: PactGen
gen_define_namespace (TObj _sch) = do
  n <- genExpr TStr
  g1 <- error "jww (2022-12-06): NYI" -- genGuard
  g2 <- error "jww (2022-12-06): NYI" -- genGuard
  pure $ EParens [ESym "define-namespace", n, g1, g2]
gen_define_namespace _ = mzero

--NOTED
gen_drop :: PactGen
gen_drop TStr = do
  x@(EStr s) <- genExpr TStr
  EBool b <- genBool
  n <- if b
      then Gen.integral $ Range.constant 0 (length s)
      else Gen.integral $ Range.constant (- length s) (length s)
  pure $ EParens [ESym "drop", EInt (fromIntegral n), x]
gen_drop (TList t) = do
  x@(EList l) <- genExpr (TList t)
  EBool b <- genBool
  n <- if b
      then Gen.integral $ Range.constant 0 (length l)
      else Gen.integral $ Range.constant (- length l) (length l)
  pure $ EParens [ESym "drop", EInt (fromIntegral n), x]
gen_drop (TObj sch) = do
  o <- genExpr (TObj sch)
  let fields = map fst sch
  s <- Gen.subsequence fields
  pure $ EParens [ESym "drop", EList (map EStr s), o]
gen_drop _ = mzero

--NOTED
gen_enforce :: PactGen
gen_enforce t@TBool = do
  x <- genExpr t
  msg <- genExpr TStr
  pure $ EParens [ESym "enforce", x, msg]
gen_enforce _ = mzero

--NOTED
gen_enforce_one :: PactGen
gen_enforce_one t@TBool = do
  x <- genExpr TStr
  y <- genExpr (TList t)
  pure $ EParens [ESym "enforce-one", x, y]
gen_enforce_one _ = mzero

--NOTED
gen_enforce_pact_version :: PactGen
gen_enforce_pact_version TBool = do
  ver <- genExpr TStr -- jww (2022-09-26): TODO
  pure $ EParens [ESym "enforce-pact-version", ver]
gen_enforce_pact_version _ = mzero

--NOTED
gen_enumerate :: PactGen
gen_enumerate (TList TInt) = do
  x <- genExpr TInt
  y <- genExpr TInt
  EBool b <- genBool
  if b
    then do
      z <- genExpr TInt
      pure $ EParens [ESym "enumerate", x, y, z]
    else do
      pure $ EParens [ESym "enumerate", x, y]
gen_enumerate _ = mzero

--NOTED
gen_filter :: PactGen
gen_filter (TList t) = do
  f <- genExpr (TArrow [t] TBool)
  l <- genExpr (TList t)
  pure $ EParens [ESym "filter", f, l]
gen_filter _ = mzero

--NOTED
gen_fold :: PactGen
gen_fold a = do
  b <- genType
  f <- genExpr (TArrow [a, b] a)
  z <- genExpr a
  l <- genExpr (TList b)
  pure $ EParens [ESym "fold", f, z, l]

--NOTED
gen_format :: PactGen
gen_format TStr = do
  x@(EList l) <- genExpr (TList TStr)
  let i = length l
      -- creates a string of the shape "{}-{}-{}-{}-{}-{}"
      -- etc for the length of the list.
      s = intercalate "-" $ replicate i "{}"
  pure $ EParens [ESym "format", EStr s, x]
gen_format _ = mzero

--NOTED
gen_hash :: PactGen
gen_hash TStr = do
  x <- genType >>= genExpr
  pure $ EParens [ESym "hash", x]
gen_hash _ = mzero

--NOTED
gen_identity :: PactGen
gen_identity t = do
  x <- genExpr t
  pure $ EParens [ESym "identity", x]

--NOTED
gen_if :: PactGen
gen_if t = do
  b <- genExpr TBool
  x <- genExpr t
  y <- genExpr t
  pure $ EParens [ESym "if", b, x, y]

--NOTED
gen_int_to_str :: PactGen
gen_int_to_str TStr = do
  x <- genExpr TInt
  -- defaulting to 16 for the base conversion since
  -- it should be the upper bound in terms of
  -- computational cost
  pure $ EParens [ESym "int-to-str", EInt 16, x]
gen_int_to_str _ = mzero

--NOTED
gen_is_charset :: PactGen
gen_is_charset TBool = do
  x <- genExpr TStr
  -- latin1 is the upperbound in terms of complexity for
  -- this native.
  pure $ EParens [ESym "is-charset", EStr "CHARSET_LATIN1", x]
gen_is_charset _ = mzero

--NOTED
gen_length :: PactGen
gen_length TInt = do
  x <- genType >>= genExpr . TList
  pure $ EParens [ESym "length", x]
gen_length _ = mzero

--NOTED
gen_list_modules :: PactGen
gen_list_modules (TList TStr) = do
  pure $ EParens [ESym "list-modules"]
gen_list_modules _ = mzero

--NOTED
gen_make_list :: PactGen
gen_make_list (TList t) = do
  n <- Gen.integral listRange
  x <- genExpr t
  pure $ EParens [ESym "make-list", EInt (fromIntegral n), x]
gen_make_list _ = mzero

--NOTED
gen_map :: PactGen
gen_map (TList a) = do
  b <- genType
  f <- genExpr (TArrow [b] a)
  l <- genExpr (TList b)
  pure $ EParens [ESym "map", f, l]
gen_map _ = mzero

--NOTED
gen_zip :: PactGen
gen_zip (TList c) = do
  a <- genType
  b <- genType
  f <- genExpr (TArrow [a, b] c)
  la <- genExpr (TList a)
  lb <- genExpr (TList b)
  pure $ EParens [ESym "zip", f, la, lb]
gen_zip _ = mzero

--NOTED
gen_namespace :: PactGen
gen_namespace TStr = do
  x <- genExpr TStr
  pure $ EParens [ESym "namespace", x]
gen_namespace _ = mzero

--NOTED
gen_pact_id :: PactGen
gen_pact_id TStr = pure $ EParens [ESym "pact-id"]
gen_pact_id _ = mzero

--NOTED
gen_pact_version :: PactGen
gen_pact_version TStr = pure $ EParens [ESym "pact-version"]
gen_pact_version _ = mzero

--NOTED
gen_read_decimal :: PactGen
gen_read_decimal TDec = do
  key <- genExpr TStr
  pure $ EParens [ESym "read-decimal", key]
gen_read_decimal _ = mzero

--NOTED
gen_read_integer :: PactGen
gen_read_integer TInt = do
  key <- genExpr TStr
  pure $ EParens [ESym "read-decimal", key]
gen_read_integer _ = mzero

--NOTED
gen_read_msg :: PactGen
gen_read_msg _t = do
  EBool b <- genBool
  if b
   then do
     key <- genExpr TStr
     pure $ EParens [ESym "read-msg", key]
   else do
     pure $ EParens [ESym "read-msg"]

--NOTED
gen_read_string :: PactGen
gen_read_string TStr = do
  key <- genExpr TStr
  pure $ EParens [ESym "read-string", key]
gen_read_string _ = mzero

--NOTED
gen_remove :: PactGen
gen_remove (TObj sch) = do
  x <- genExpr (TObj sch)
  (a, _) <- Gen.element sch
  pure $ EParens [ESym "remove", EStr a, x]
gen_remove _ = mzero

--TODO
gen_resume :: PactGen
gen_resume _ = mzero -- jww (2022-12-07): TODO

--NOTED
gen_reverse :: PactGen
gen_reverse (TList t) = do
  x <- genExpr (TList t)
  pure $ EParens [ESym "reverse", x]
gen_reverse _ = mzero

--NOTED
gen_sort :: PactGen
gen_sort (TList t) = do
  x <- genExpr (TList t)
  pure $ EParens [ESym "sort", x]
gen_sort t@(TObj sch) = do
  l <- genExpr (TList t)
  let fields = map fst sch
  s <- Gen.subsequence fields
  pure $ EParens [ESym "sort", EList (map EStr s), l]
gen_sort _ = mzero

--NOTED
gen_str_to_int :: PactGen
gen_str_to_int TInt = do
  x <- genExpr TStr
  pure $ EParens [ESym "str-to-int", x]
gen_str_to_int _ = mzero

--NOTED
gen_str_to_list :: PactGen
gen_str_to_list (TList TStr) = do
  x <- genExpr TStr
  pure $ EParens [ESym "str-to-list", x]
gen_str_to_list _ = mzero

--NOTED
gen_take :: PactGen
gen_take TStr = do
  x@(EStr s) <- genExpr TStr
  EBool b <- genBool
  n <- if b
      then Gen.integral $ Range.constant 0 (length s)
      else Gen.integral $ Range.constant (- length s) (length s)
  pure $ EParens [ESym "take", EInt (fromIntegral n), x]
gen_take (TList t) = do
  x@(EList l) <- genExpr (TList t)
  EBool b <- genBool
  n <- if b
      then Gen.integral $ Range.constant 0 (length l)
      else Gen.integral $ Range.constant (- length l) (length l)
  pure $ EParens [ESym "take", EInt (fromIntegral n), x]
gen_take (TObj sch) = do
  o <- genExpr (TObj sch)
  let fields = map fst sch
  s <- Gen.subsequence fields
  pure $ EParens [ESym "take", EList (map EStr s), o]
gen_take _ = mzero

--NOTED
gen_try :: PactGen
gen_try t = do
  x <- genExpr t
  y <- genExpr t
  pure $ EParens [ESym "try", x, y]

--NOTED
gen_tx_hash :: PactGen
gen_tx_hash TStr = pure $ EParens [ESym "tx-hash"]
gen_tx_hash _ = mzero

--NOTED
gen_typeof :: PactGen
gen_typeof TStr = do
  x <- genType >>= genExpr
  pure $ EParens [ESym "typeof", x]
gen_typeof _ = mzero

--NOTED
gen_distinct :: PactGen
gen_distinct (TList t) = do
  x <- genExpr (TList t)
  pure $ EParens [ESym "distinct", x]
gen_distinct _ = mzero

--TODO
gen_where :: PactGen
gen_where _ = mzero -- jww (2022-09-26): TODO

--TODO
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
      [ (,) <$> genExpr TDec <*> genExpr TDec,
        (,) <$> genExpr TInt <*> genExpr TDec,
        (,) <$> genExpr TDec <*> genExpr TInt
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

--NOTED
gen_neq :: PactGen
gen_neq TBool = do
  t <- genType
  guard (canEq t)
  arity2 "!=" t
gen_neq _ = mzero

--NOTED
gen_bitwise_and :: PactGen
gen_bitwise_and t@TInt = arity2 "&" t
gen_bitwise_and _ = mzero

--NOTED
gen_mult :: PactGen
gen_mult t@TDec = arity2_int_or_dec "*" t
gen_mult t@TInt = arity2 "*" t
gen_mult _ = mzero

--NOTED
gen_plus :: PactGen
gen_plus t@TDec = arity2_int_or_dec "+" t
gen_plus t@TInt = arity2 "+" t
gen_plus t@TStr = arity2 "+" t
gen_plus t@(TList _) = arity2 "+" t
gen_plus t@(TObj _) = arity2 "+" t
gen_plus _ = mzero

--NOTED
gen_minus :: PactGen
gen_minus t@TDec = arity2_int_or_dec "-" t
gen_minus t@TInt = arity2 "-" t
gen_minus _ = mzero

--NOTED
gen_divide :: PactGen
gen_divide t@TDec = arity2_int_or_dec "/" t
gen_divide t@TInt = arity2 "/" t
gen_divide _ = mzero

--NOTED
gen_lt :: PactGen
gen_lt TBool = do
  t <- genType
  guard (canCmp t)
  arity2 "<" t
gen_lt _ = mzero

--NOTED
gen_lte :: PactGen
gen_lte TBool = do
  t <- genType
  guard (canCmp t)
  arity2 "<=" t
gen_lte _ = mzero

--NOTED
gen_eq :: PactGen
gen_eq TBool = do
  t <- genType
  guard (canEq t)
  arity2 "==" t
gen_eq _ = mzero

--NOTED
gen_gt :: PactGen
gen_gt TBool = do
  t <- genType
  guard (canCmp t)
  arity2 ">" t
gen_gt _ = mzero

--NOTED
gen_gte :: PactGen
gen_gte TBool = do
  t <- genType
  guard (canCmp t)
  arity2 ">=" t
gen_gte _ = mzero

--NOTED
gen_pow :: PactGen
gen_pow t@TInt = arity2 "^" t
gen_pow t@TDec = arity2_int_or_dec "^" t
gen_pow _ = mzero

--NOTED
gen_abs :: PactGen
gen_abs t@TInt = arity1 "abs" t
gen_abs t@TDec = arity1 "abs" t
gen_abs _ = mzero

--NOTED
gen_and :: PactGen
gen_and t@TBool = arity1 "and" t
gen_and _ = mzero

--TODO
gen_and_question :: PactGen
gen_and_question _ = mzero -- jww (2022-09-26): TODO

--NOTED
gen_ceiling :: PactGen
gen_ceiling TInt = arity1 "ceiling" TDec
gen_ceiling _ = mzero

--NOTED
gen_exp :: PactGen
gen_exp t@TInt = arity1 "exp" t
gen_exp t@TDec = arity1 "exp" t
gen_exp _ = mzero

--NOTED
gen_floor :: PactGen
gen_floor TInt = arity1 "floor" TDec
gen_floor _ = mzero

--NOTED
gen_ln :: PactGen
gen_ln t@TInt = arity1 "ln" t
gen_ln t@TDec = arity1 "ln" t
gen_ln _ = mzero

--NOTED
gen_log :: PactGen
gen_log t@TInt = arity2 "log" t
gen_log t@TDec = arity2_int_or_dec "log" t
gen_log _ = mzero

--NOTED
gen_mod :: PactGen
gen_mod t@TInt = arity2 "mod" t
gen_mod _ = mzero

--NOTED
gen_not :: PactGen
gen_not t@TBool = arity1 "not" t
gen_not _ = mzero

--TODO
gen_not_question :: PactGen
gen_not_question _ = mzero -- jww (2022-09-26): TODO

--NOTED
gen_or :: PactGen
gen_or t@TBool = arity2 "or" t
gen_or _ = mzero

--TODO
gen_or_question :: PactGen
gen_or_question _ = mzero -- jww (2022-09-26): TODO

--NOTED
gen_round :: PactGen
gen_round TInt = arity1 "round" TDec
gen_round _ = mzero

--NOTED
gen_shift :: PactGen
gen_shift t@TInt = arity2 "shift" t
gen_shift _ = mzero

--NOTED
gen_sqrt :: PactGen
gen_sqrt t@TInt = arity1 "sqrt" t
gen_sqrt t@TDec = arity1 "sqrt" t
gen_sqrt _ = mzero

--NOTED
gen_xor :: PactGen
gen_xor t@TInt = arity2 "xor" t
gen_xor _ = mzero

--NOTED
gen_bitwise_or :: PactGen
gen_bitwise_or t@TInt = arity2 "|" t
gen_bitwise_or _ = mzero

--NOTED
gen_bitwise_complement :: PactGen
gen_bitwise_complement t@TInt = arity1 "~" t
gen_bitwise_complement _ = mzero

--NOTED
gen_add_time :: PactGen
gen_add_time TTime = do
  t <- genExpr TTime
  s <- genExpr TInt
  return $ EParens [ESym "add-time", t, s]
gen_add_time _ = mzero

--NOTED
gen_days :: PactGen
gen_days t@TInt = arity1 "days" t
gen_days t@TDec = arity1 "days" t
gen_days _ = mzero

