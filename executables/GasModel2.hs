{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.DeepSeq
import Control.Exception (bracket)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Reader
import Criterion qualified as C
import qualified Data.Aeson as A
import Criterion.Types qualified as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64
import Data.Csv qualified as Csv
import Data.Decimal
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as M
import Data.IORef
import Data.List
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time
import Data.Time.Format.ISO8601
import Data.Vector qualified as V
import GHC.Generics
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Pact.GasModel.GasModel hiding (bench, benchesOnce, main)
import Pact.GasModel.Types
import Pact.GasModel.Utils
import Pact.Types.Lang qualified as Pact
import Pact.Types.Runtime (EvalEnv (_eeGas), eeMsgBody,
                           evalCallStack, evalPactExec)
import Pact.Types.Term (Gas(..))
import Statistics.Types (Estimate (..))

instance Csv.FromField Gas where
  parseField s = Gas <$> Csv.parseField s
instance Csv.ToField Gas where
  toField (Gas s) = Csv.toField s

deriving instance Csv.FromRecord Gas
deriving instance Csv.ToRecord Gas

data GasResult = GasResult {
  testName :: String,
  gasCost :: Gas,
  timeSpent :: NanoSeconds,
  gasRate :: NanoSeconds,
  pactExpr :: T.Text
  }
  deriving (Show, Generic,
            Csv.FromRecord, Csv.ToRecord,
            Csv.FromNamedRecord, Csv.ToNamedRecord)

main :: IO ()
main = do
  putStrLn "Checking that generation works:"
  xs1 <-
    Gen.sample $
      replicateM 10 $
        flip runReaderT defaultEnv $
          genExpr TInt
  forM_ xs1 $ \x1 -> do
    -- print x1
    putStrLn $ toLisp x1

  let mkGasTest !name = do
        expr <- Gen.sample $ runReaderT (genBuiltin =<< genType) defaultEnv
        return $! expr `deepseq` gasTest name expr

  putStrLn "Establishing gas baseline"
  baselineReport <- benchesOnce $ gasTest "baseline" (ESym "true")
  let [(baselineGas, baselineTime)] =
        map _gasTestResultSqliteDb baselineReport

  -- Enforces that unit tests succeed
  putStrLn "Doing dry run of benchmark tests"
  tests <- forM ([1..5_000] :: [Int]) $ \i -> do
    t <- mkGasTest (show i)
    mockRuns t
    pure $! (Pact.NativeDefName (T.pack (show i)), t)
  putStrLn "Doing dry run of benchmark tests...done"

  putStrLn "Running benchmark(s)"
  if True -- _oBenchOnly opt
    then do
      let baseline = GasResult {
           testName = "baseline",
           gasCost = baselineGas,
           timeSpent = baselineTime,
           gasRate = 0.0,
           pactExpr = "true"
         }
      print baseline
      let displayGasPrice (funName, gt@(GasUnitTests [t])) = do
            res <- benchesOnce gt
            let [(gas, time)] = map _gasTestResultSqliteDb res
            let Gas gas' = gas - baselineGas
            let time' = time - baselineTime
            let result = GasResult {
                      testName = T.unpack (Pact.asString funName),
                      gasCost = Gas gas',
                      timeSpent = time',
                      gasRate = if gas' > 0
                                then time' / fromIntegral gas'
                                else time',
                      pactExpr = _pactExpressionFull (_gasTestExpression t)
                    }
            print result
            pure result
      results <- mapM displayGasPrice tests

      BL.putStr $ Csv.encodeByName
        (V.fromList ["testName","gasCost","timeSpent","gasRate","pactExpr"])
        (baseline : results)
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
    _   <- terms `deepseq` exec state env terms
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

gasTest :: String -> LispExpr -> GasUnitTests
gasTest name expr =
  createGasUnitTests
    (updateWithPactExec . updateStackFrame . updateEnv)
    (updateWithPactExec . updateStackFrame . updateEnv)
    [PactExpression (T.pack (toLisp expr)) Nothing]
    (Pact.NativeDefName (T.pack name))
  where
    updateStackFrame = setState (set evalCallStack [someStackFrame])

    updateWithPactExec = setState $ set evalPactExec $ Just $
      Pact.PactExec 2 Nothing Nothing 0 (Pact.PactId "somePactId")
      (Pact.PactContinuation (Pact.Name $ Pact.BareName "some-defpact-func" def) [])
      False
      mempty

    updateEnv = setEnv $ set eeMsgBody $ A.object
      [ "ks1" A..= A.object
        [ "keys" A..= ["76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      , "ks2" A..= A.object
        [ "keys" A..=
          [ "76d458b3aa1b0d11a5be8385be2646d799ab898d863dc74e6b78c4726e7f4e8d" :: T.Text
          , "011b1bb033d77f0ef7fe0c09f7b10ed91c7f432f6fdc1ba68acdc776fa53d99c" :: T.Text
          ]
        , "pred" A..= ("keys-all" :: T.Text)
        ]
      , "msg" A..= ("hello" :: T.Text)
      , "int" A..= (123 :: Int)
      , "dec" A..= (456.0 :: Float)
      ]

type PactGen = ExprType -> ReaderT Env Gen LispExpr

type Scope = HashMap String LispExpr

data Env = Env
  { scopes :: [Scope],
    depth :: Int -- how deeply can expressions be nested?
  }

defaultEnv :: Env
defaultEnv = Env [] 6

-- Although "any" is technically a valid type, we only generate values in this
-- module whose type we know at time of generation.
data ExprType
  = TStr
  | TInt
  | TDec
  | TBool
  | TTime
  | TKeyset
  | TList !ExprType
  | TObj !Schema
  | TTable !Schema
  | TArrow ![ExprType] !ExprType
  deriving (Eq, Show, Generic, NFData)

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
  = EStr !String
  | EInt !Integer
  | EDec !Decimal
  | EBool !Bool
  | ETime !UTCTime
  | EKeyset
  | EList ![LispExpr]
  | EObj ![(String, LispExpr)]
  | ETable !Schema
  | EModule
  | ESym !String
  | EParens ![LispExpr]
  deriving (Eq, Show, Generic, NFData)

toLisp :: LispExpr -> String
toLisp = \case
  EStr s -> show s
  EInt i -> show i
  EDec d -> show d
  EBool True -> "true"
  EBool False -> "false"
  ETime t ->
    -- jww (2022-12-15): Stuart says that extra precision can affect storage,
    -- so we need to generate those as well.
    let t' = t { utctDayTime = fromIntegral (round (utctDayTime t) :: Integer) }
    in "(time \"" ++ iso8601Show t' ++ "\")"
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
  Gen.list (Range.exponential 1 5)
    ((,) <$> Gen.string (Range.linear 2 5) Gen.alpha <*> genType)

genObjBy :: PactGen -> Schema -> ReaderT Env Gen LispExpr
genObjBy gen sch =
  local (\e -> e { depth = depth e - 1 }) $ do
    env <- ask
    if depth env <= 0
      then pure $ EObj []
      else ReaderT $ \_ ->
        EObj <$> traverse (\(fld, ty) -> (fld,) <$> runReaderT (gen ty) env) sch

genObj :: Schema -> ReaderT Env Gen LispExpr
genObj = genObjBy genExpr

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
  TList t -> genListBy genAtom t
  TObj sch -> genObjBy genAtom sch
  -- elp (2022-10-26): should this even exist?
  TTable sch -> pure $ ETable sch -- jww (2022-09-26): TODO
  TArrow doms cod -> genArrow doms cod

genExpr :: PactGen
genExpr t = do
  env <- ask
  if depth env <= 0
    then genAtom t
    else do
      EBool b <- genBool
      if b
        then genAtom t
        else local (\e -> e { depth = depth e - 1 }) $
          genBuiltin t

listRange :: Int -> Range Int
listRange = Range.constant 0

genListBy :: PactGen -> PactGen
genListBy gen t =
  local (\e -> e { depth = depth e - 1 }) $ do
    env <- ask
    if depth env <= 0
      then pure $ EList []
      else EList <$> Gen.list (listRange (len (depth env))) (gen t)
  where
    -- These numbers determine how long lists can be at various recursion
    -- depths.
    len n | n < 1 = 8
          | n < 2 = 6
          | n < 3 = 4
          | otherwise = 2

genList :: PactGen
genList = genListBy genExpr

genArrow :: [ExprType] -> PactGen
genArrow _doms _cod = mzero -- jww (2022-12-06): genArrow TODO

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
    [ -- Language constructs
      ("let",                  gen_let),

      -- General native functions
      ("at",                   gen_at),
      ("base64-decode",        gen_base64_decode),
      ("base64-encode",        gen_base64_encode),
      ("bind",                 gen_bind),
      ("chain-data",           gen_chain_data),
      ("compose",              gen_compose),
      ("concat",               gen_concat),
      ("constantly",           gen_constantly),
      ("contains",             gen_contains),
      ("define-namespace",     gen_define_namespace),
      ("drop",                 gen_drop),
      ("enforce",              gen_enforce),
      ("enforce-one",          gen_enforce_one),
      ("enforce-pact-version", gen_enforce_pact_version),
      ("enumerate",            gen_enumerate),
      ("filter",               gen_filter),
      ("fold",                 gen_fold),
      ("format",               gen_format),
      ("hash",                 gen_hash),
      ("identity",             gen_identity),
      ("if",                   gen_if),
      ("int-to-str",           gen_int_to_str),
      ("is-charset",           gen_is_charset),
      ("length",               gen_length),
      ("list-modules",         gen_list_modules),
      ("make-list",            gen_make_list),
      ("map",                  gen_map),
      ("zip",                  gen_zip),
      ("namespace",            gen_namespace),
      ("pact-id",              gen_pact_id),
      ("pact-version",         gen_pact_version),
      ("read-decimal",         gen_read_decimal),
      ("read-integer",         gen_read_integer),
      ("read-msg",             gen_read_msg),
      ("read-string",          gen_read_string),
      ("remove",               gen_remove),
      ("resume",               gen_resume),
      ("reverse",              gen_reverse),
      ("sort",                 gen_sort),
      ("str-to-int",           gen_str_to_int),
      ("str-to-list",          gen_str_to_list),
      ("take",                 gen_take),
      ("try",                  gen_try),
      ("tx-hash",              gen_tx_hash),
      ("typeof",               gen_typeof),
      ("distinct",             gen_distinct),
      ("where",                gen_where),
      ("yield",                gen_yield),

      -- Operators native functions
      ("!=",                   gen_neq),
      ("&",                    gen_bitwise_and),
      ("*",                    gen_mult),
      ("+",                    gen_plus),
      ("-",                    gen_minus),
      ("/",                    gen_divide),
      ("<",                    gen_lt),
      ("<=",                   gen_lte),
      ("=",                    gen_eq),
      (">",                    gen_gt),
      (">=",                   gen_gte),
      ("^",                    gen_pow),
      ("abs",                  gen_abs),
      ("and",                  gen_and),
      ("and?",                 gen_and_question),
      ("ceiling",              gen_ceiling),
      ("exp",                  gen_exp),
      ("floor",                gen_floor),
      ("ln",                   gen_ln),
      ("log",                  gen_log),
      ("mod",                  gen_mod),
      ("not",                  gen_not),
      ("not?",                 gen_not_question),
      ("or",                   gen_or),
      ("or?",                  gen_or_question),
      ("round",                gen_round),
      ("shift",                gen_shift),
      ("sqrt",                 gen_sqrt),
      ("xor",                  gen_xor),
      ("|",                    gen_bitwise_or),
      ("~",                    gen_bitwise_complement),

      -- Time native functions
      ("add-time",             gen_add_time),
      ("days",                 gen_days),
      ("diff-time",            gen_diff_time),
      ("format-time",          gen_format_time),
      ("hours",                gen_hours),
      ("minutes",              gen_minutes),
      ("parse-time",           gen_parse_time),
      ("time",                 gen_time),

      -- Commitments native functions
      ("decrypt-cc20p1305",    gen_decrypt_cc20p1305),
      ("validate-keypair",     gen_validate_keypair),

      -- Keyset native functions
      ("define-keyset",        gen_define_keyset),
      ("enforce-keyset",       gen_enforce_keyset),
      ("keys-2",               gen_keys_2),
      ("keys-all",             gen_keys_all),
      ("keys-any",             gen_keys_any),
      ("read-keyset",          gen_read_keyset),

      -- Database native functions
      ("create-table",         gen_create_table),
      ("describe-keyset",      gen_describe_keyset),
      ("describe-module",      gen_describe_module),
      ("describe-table",       gen_describe_table),
      ("describe-namespace",   gen_describe_namespace),
      ("insert",               gen_insert),
      ("keylog",               gen_keylog),
      ("keys",                 gen_keys),
      ("read",                 gen_read),
      ("select",               gen_select),
      ("txids",                gen_txids),
      ("txlog",                gen_txlog),
      ("update",               gen_update),
      ("with-default-read",    gen_with_default_read),
      ("with-read",            gen_with_read),
      ("write",                gen_write),
      ("fold-db",              gen_fold_db),

      -- Capabilities native functions
      ("compose-capability",   gen_compose_capability),
      ("create-module-guard",  gen_create_module_guard),
      ("create-pact-guard",    gen_create_pact_guard),
      ("create-user-guard",    gen_create_user_guard),
      ("enforce-guard",        gen_enforce_guard),
      ("install-capability",   gen_install_capability),
      ("keyset-ref-guard",     gen_keyset_ref_guard),
      ("require-capability",   gen_require_capability),
      ("with-capability",      gen_with_capability),
      ("emit-event",           gen_emit_event),

      -- Principal creation and validation
      ("create-principal",     gen_create_principal),
      ("validate-principal",   gen_validate_principal),
      ("is-principal",         gen_is_principal),
      ("typeof-principal",     gen_typeof_principal),

      -- Non-native concepts to benchmark
      ("use",                  gen_use),
      ("module",               gen_module),
      ("interface",            gen_interface)
    ]

gen_let :: PactGen
gen_let t = do
  -- Note that the list here cannot be a call to a builtin, because then we
  -- wouldn't know the length in order to construct a valid index.
  x <- genExpr t
  -- jww (2022-12-16): We need to synthesize some names and values, add them
  -- to the Reader environment, and then allow `genExpr` to pick symbols from
  -- this environment.
  pure $ EParens [ESym "let", EParens [EParens [ESym "x", EInt 0]], x]

gen_at :: PactGen
gen_at t = do
  -- Note that the list here cannot be a call to a builtin, because then we
  -- wouldn't know the length in order to construct a valid index.
  l@(EList xs) <- genList t
  guard $ length xs > 0
  i <- Gen.integral $ Range.constant 0 (pred (length xs))
  pure $ EParens [ESym "at", EInt (fromIntegral i), l]

gen_base64_decode :: PactGen
gen_base64_decode TStr = do
  -- jww (2022-12-09): In order to properly stress this, we have to generate 2
  -- strings: one (the first part) encoded as valid base64, and then the
  -- second being (potential) garbage so that we can stress the error path
  -- more effectively.
  EStr x <- genStr
  let z =
        Text.unpack . Text.decodeUtf8
          . B64.encode
          . Text.encodeUtf8 . Text.pack $ x
  pure $ EParens [ESym "base64-decode", EStr z]
gen_base64_decode _ = mzero

gen_base64_encode :: PactGen
gen_base64_encode t@TStr = do
  x <- genExpr t
  pure $ EParens [ESym "base64-encode", x]
gen_base64_encode _ = mzero

gen_bind :: PactGen
-- gen_bind t@TObj {} = do
--   x <- genExpr t
--   y <- genExpr t
--   z <- genExpr =<< genType
--   pure $ EParens [ESym "bind", x, y, z]
gen_bind _ = mzero -- jww (2022-12-15): TODO

public_chain_data :: Schema
public_chain_data =
  [ ("chain-id", TStr)
  , ("block-height", TInt)
  , ("block-time", TTime)
  , ("prev-block-hash", TStr)
  , ("sender", TStr)
  , ("gas-limit", TInt)
  , ("gas-price", TDec)
  ]

gen_chain_data :: PactGen
gen_chain_data (TObj sch) | sch == public_chain_data =
  pure $ EParens [ESym "chain-data"]
gen_chain_data _ = mzero

gen_compose :: PactGen
gen_compose (TArrow [a] c) = do
  b :: ExprType <- genType
  g <- genExpr (TArrow [a] b)
  f <- genExpr (TArrow [b] c)
  pure $ EParens [ESym "compose", f, g]
gen_compose _ = mzero

gen_concat :: PactGen
gen_concat t@TStr = do
  x <- genExpr (TList t)
  pure $ EParens [ESym "concat", x]
gen_concat _ = mzero

gen_constantly :: PactGen
gen_constantly t = do
  x <- genExpr t
  y <- genType >>= genExpr
  pure $ EParens [ESym "constantly", x, y]

gen_contains :: PactGen
gen_contains TBool = do
  EBool b1 <- genBool
  if b1
    then do -- value <a> list [<a>] → bool
      t <- genType
      x <- genExpr t
      l <- genExpr (TList t)
      pure $ EParens [ESym "contains", x, l]
    else do
      EBool b2 <- genBool
      if b2
        then do -- key <a> object object:<{o}> → bool
          EBool b3 <- genBool
          if b3
            then do -- key <a> object object:<{o}> → bool
              k <- genExpr TStr
              sch <- genSchema
              o <- genExpr (TObj sch)
              pure $ EParens [ESym "contains", k, o]
            else do -- key <a> object object:<{o}> → bool
              EStr k <- genStr
              sch <- genSchema
              o <- genObj sch
              guard $ isJust (lookup k sch)
              pure $ EParens [ESym "contains", EStr k, o]
        else do -- value string string string → bool
          EBool b3 <- genBool
          if b3
            then do -- value string string string → bool
              x <- genExpr TStr
              y <- genExpr TStr
              pure $ EParens [ESym "contains", x, y]
            else do -- value string string string → bool
              EStr x <- genStr
              EStr y <- genStr
              guard $ x `isInfixOf` y
              pure $ EParens [ESym "contains", EStr x, EStr y]
gen_contains _ = mzero

gen_define_namespace :: PactGen
-- gen_define_namespace (TObj _sch) = do
--   n <- genExpr TStr
--   g1 <- genGuard
--   g2 <- genGuard
--   pure $ EParens [ESym "define-namespace", n, g1, g2]
gen_define_namespace _ = mzero -- jww (2022-12-15): TODO

gen_drop :: PactGen
gen_drop TStr = do
  -- Note that the string here cannot be a call to a builtin, because then we
  -- wouldn't know the length in order to construct a valid index.
  x@(EStr s) <- genStr
  EBool b <- genBool
  n <- if b
      then Gen.integral $ Range.constant 0 (length s)
      else Gen.integral $ Range.constant (- length s) (length s)
  pure $ EParens [ESym "drop", EInt (fromIntegral n), x]
gen_drop (TList t) = do
  -- Note that the list here cannot be a call to a builtin, because then we
  -- wouldn't know the length in order to construct a valid index.
  x@(EList l) <- genList t
  EBool b <- genBool
  n <- if b
      then Gen.integral $ Range.constant 0 (length l)
      else Gen.integral $ Range.constant (- length l) (length l)
  pure $ EParens [ESym "drop", EInt (fromIntegral n), x]
gen_drop (TObj sch) = do
  sch2 <- genSchema
  forM_ sch2 $ \(k, _) ->
    guard $ isNothing $ lookup k sch
  EBool b <- genBool
  if b
    then do
      o <- genExpr (TObj (sch ++ sch2))
      pure $ EParens [ESym "drop", EList (map (EStr . fst) sch2), o]
    else do
      o <- genExpr (TObj sch)
      pure $ EParens [ESym "drop", EList (map (EStr . fst) sch2), o]
gen_drop _ = mzero

gen_enforce :: PactGen
gen_enforce t@TBool = do
  x <- genExpr t
  msg <- genExpr TStr
  pure $ EParens [ESym "enforce", EParens [ESym "or", x, ESym "true"], msg]
gen_enforce _ = mzero

gen_enforce_one :: PactGen
gen_enforce_one TBool = do
  s <- genExpr TStr
  -- jww (2022-12-14): TODO
  -- y <- genExpr (TList t)
  EList y <- genList TBool
  guard $ length y > 0
  pure $ EParens [ESym "enforce-one", s,
                  EList (map (\b -> EParens [ESym "or", b, ESym "true"]) y)]
gen_enforce_one _ = mzero

gen_enforce_pact_version :: PactGen
-- gen_enforce_pact_version TBool = do
--   ver <- genExpr TStr
--   pure $ EParens [ESym "enforce-pact-version", ver]
gen_enforce_pact_version _ = mzero -- jww (2022-12-15): TODO

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

gen_filter :: PactGen
gen_filter (TList t) = do
  f <- genExpr (TArrow [t] TBool)
  l <- genExpr (TList t)
  pure $ EParens [ESym "filter", f, l]
gen_filter _ = mzero

gen_fold :: PactGen
gen_fold a = do
  b <- genType
  f <- genExpr (TArrow [a, b] a)
  z <- genExpr a
  l <- genExpr (TList b)
  pure $ EParens [ESym "fold", f, z, l]

gen_format :: PactGen
gen_format TStr = do
  -- Note that the list here cannot be a call to a builtin, because then we
  -- wouldn't know the length in order to construct a valid index.
  x@(EList l) <- genList TStr
  let i = length l
      -- creates a string of the shape "{}-{}-{}-{}-{}-{}"
      -- etc for the length of the list.
      s = intercalate "-" $ replicate i "{}"
  pure $ EParens [ESym "format", EStr s, x]
gen_format _ = mzero

gen_hash :: PactGen
gen_hash TStr = do
  x <- genType >>= genExpr
  pure $ EParens [ESym "hash", x]
gen_hash _ = mzero

gen_identity :: PactGen
gen_identity t = do
  x <- genExpr t
  pure $ EParens [ESym "identity", x]

gen_if :: PactGen
gen_if t = do
  b <- genExpr TBool
  x <- genExpr t
  y <- genExpr t
  pure $ EParens [ESym "if", b, x, y]

gen_int_to_str :: PactGen
gen_int_to_str TStr = do
  x <- genExpr TInt
  -- defaulting to 16 for the base conversion since
  -- it should be the upper bound in terms of
  -- computational cost
  pure $ EParens [ESym "int-to-str", EInt 16, EParens [ESym "abs", x]]
gen_int_to_str _ = mzero

gen_is_charset :: PactGen
gen_is_charset TBool = do
  c <- Gen.element ["CHARSET_ASCII", "CHARSET_LATIN1"]
  x <- genExpr TStr
  -- latin1 is the upperbound in terms of complexity for
  -- this native.
  pure $ EParens [ESym "is-charset", ESym c, x]
gen_is_charset _ = mzero

gen_length :: PactGen
gen_length TInt = do
  x <- genType >>= genExpr . TList
  pure $ EParens [ESym "length", x]
gen_length _ = mzero

gen_list_modules :: PactGen
gen_list_modules (TList TStr) = do
  pure $ EParens [ESym "list-modules"]
gen_list_modules _ = mzero

gen_make_list :: PactGen
gen_make_list (TList t) = do
  n <- Gen.integral (listRange 10)
  x <- genExpr t
  pure $ EParens [ESym "make-list", EInt (fromIntegral n), x]
gen_make_list _ = mzero

gen_map :: PactGen
gen_map (TList a) = do
  b <- genType
  f <- genExpr (TArrow [b] a)
  l <- genExpr (TList b)
  pure $ EParens [ESym "map", f, l]
gen_map _ = mzero

gen_zip :: PactGen
gen_zip (TList c) = do
  a <- genType
  b <- genType
  f <- genExpr (TArrow [a, b] c)
  la <- genExpr (TList a)
  lb <- genExpr (TList b)
  pure $ EParens [ESym "zip", f, la, lb]
gen_zip _ = mzero

gen_namespace :: PactGen
-- gen_namespace TStr = do
--   x <- genExpr TStr
--   pure $ EParens [ESym "namespace", x]
gen_namespace _ = mzero -- jww (2022-12-09): TODO

gen_pact_id :: PactGen
gen_pact_id TStr = pure $ EParens [ESym "pact-id"]
gen_pact_id _ = mzero

gen_pact_version :: PactGen
-- gen_pact_version TStr = pure $ EParens [ESym "pact-version"]
gen_pact_version _ = mzero -- jww (2022-12-15): TODO

gen_read_decimal :: PactGen
gen_read_decimal TDec = do
  -- jww (2022-12-14): This should really be a key chosen from a map
  -- provided in the environment.
  -- key <- genExpr TStr
  let key = EStr "dec"
  pure $ EParens [ESym "read-decimal", key]
gen_read_decimal _ = mzero

gen_read_integer :: PactGen
gen_read_integer TInt = do
  -- jww (2022-12-14): This should really be a key chosen from a map
  -- provided in the environment.
  -- key <- genExpr TStr
  let key = EStr "int"
  pure $ EParens [ESym "read-integer", key]
gen_read_integer _ = mzero

-- jww (2022-12-09): This needs to read from an environment map that gets
-- setup as part of the Pact exe environment. For now, we only provide a
-- single string value under the key "msg".
gen_read_msg :: PactGen
gen_read_msg TStr = do
  -- jww (2022-12-14): `read-msg` with no arguments is the "type inference"
  -- version of read-msg.
  -- EBool b <- genBool
  let b = True
  if b
    then do
      -- jww (2022-12-14): This should really be a key chosen from a map
      -- provided in the environment.
      -- key <- genExpr TStr
      let key = EStr "msg"
      pure $ EParens [ESym "read-msg", key]
    else do
      pure $ EParens [ESym "read-msg"]
gen_read_msg _ = mzero

gen_read_string :: PactGen
gen_read_string TStr = do
  -- jww (2022-12-14): This should really be a key chosen from a map
  -- provided in the environment.
  -- key <- genExpr TStr
  let key = EStr "msg"
  pure $ EParens [ESym "read-string", key]
gen_read_string _ = mzero

gen_remove :: PactGen
gen_remove (TObj sch) = do
  t <- genType
  EStr e <- genStr
  guard $ length e > 0
  guard $ isNothing $ lookup e sch
  x <- genExpr (TObj ((e,t):sch))
  pure $ EParens [ESym "remove", EStr e, x]
gen_remove _ = mzero

gen_resume :: PactGen
gen_resume _ = mzero -- jww (2022-12-07): TODO

gen_reverse :: PactGen
gen_reverse (TList t) = do
  x <- genExpr (TList t)
  pure $ EParens [ESym "reverse", x]
gen_reverse _ = mzero

gen_sort :: PactGen
gen_sort (TList (TObj sch)) = do
  EList l <- genList (TObj sch)
  guard $ length l > 1
  let fields = map fst sch
  s <- Gen.subsequence fields
  guard $ length s > 0
  pure $ EParens [ESym "sort", EList (map EStr s), EList l]
gen_sort (TList t) = do
  x <- genExpr (TList t)
  pure $ EParens [ESym "sort", x]
gen_sort _ = mzero

gen_str_to_int :: PactGen
gen_str_to_int TInt = do
  EBool b <- genBool
  if b
    then do
      EStr s <- genStr
      guard $ all (`elem` ("0123456789" :: String)) s
      guard $ length s > 0
      pure $ EParens [ESym "str-to-int", EStr s]
    else do
      n <- Gen.element [2, 8, 10, 16]
      EStr s <- genStr
      case n of
          2  -> guard $ all (`elem` ("01" :: String)) s
          8  -> guard $ all (`elem` ("01234567" :: String)) s
          10 -> guard $ all (`elem` ("0123456789" :: String)) s
          16 -> guard $ all (`elem` ("0123456789abcdef" :: String)) s
          _  -> error "Impossible"
      guard $ length s > 0
      pure $ EParens [ESym "str-to-int", EInt n, EStr s]
gen_str_to_int _ = mzero

gen_str_to_list :: PactGen
gen_str_to_list (TList TStr) = do
  x <- genExpr TStr
  pure $ EParens [ESym "str-to-list", x]
gen_str_to_list _ = mzero

gen_take :: PactGen
gen_take TStr = do
  -- Note that the string here cannot be a call to a builtin, because then we
  -- wouldn't know the length in order to construct a valid index.
  x@(EStr s) <- genStr
  EBool b <- genBool
  n <- if b
      then Gen.integral $ Range.constant 0 (length s)
      else Gen.integral $ Range.constant (- length s) (length s)
  pure $ EParens [ESym "take", EInt (fromIntegral n), x]
gen_take (TList t) = do
  -- Note that the list here cannot be a call to a builtin, because then we
  -- wouldn't know the length in order to construct a valid index.
  x@(EList l) <- genList t
  EBool b <- genBool
  n <- if b
      then Gen.integral $ Range.constant 0 (length l)
      else Gen.integral $ Range.constant (- length l) (length l)
  pure $ EParens [ESym "take", EInt (fromIntegral n), x]
gen_take (TObj sch) = do
  sch2 <- genSchema
  forM_ sch2 $ \(k, _) ->
    guard $ isNothing $ lookup k sch
  o <- genExpr (TObj (sch ++ sch2))
  pure $ EParens [ESym "take", EList (map (EStr . fst) sch), o]
gen_take _ = mzero

gen_try :: PactGen
gen_try t = do
  x <- genExpr t
  y <- genExpr t
  pure $ EParens [ESym "try", x, y]

gen_tx_hash :: PactGen
gen_tx_hash TStr = pure $ EParens [ESym "tx-hash"]
gen_tx_hash _ = mzero

gen_typeof :: PactGen
gen_typeof TStr = do
  x <- genType >>= genExpr
  pure $ EParens [ESym "typeof", x]
gen_typeof _ = mzero

gen_distinct :: PactGen
gen_distinct (TList t) = do
  x <- genExpr (TList t)
  pure $ EParens [ESym "distinct", x]
gen_distinct _ = mzero

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
gen_plus t@(TObj _) = arity2 "+" t
gen_plus _ = mzero

gen_minus :: PactGen
gen_minus t@TDec = arity2_int_or_dec "-" t
gen_minus t@TInt = arity2 "-" t
gen_minus _ = mzero

gen_divide :: PactGen
gen_divide TDec = do
  x <- genExpr TDec
  y <- Gen.choice [ do EInt y <- genInt
                       guard $ y /= 0
                       pure $ EInt y
                  , do EDec y <- genDec
                       guard $ y /= 0
                       pure $ EDec y
                  ]
  pure $ EParens [ESym "/", x, y]
gen_divide TInt = do
  x <- genExpr TInt
  EInt y <- genInt
  guard $ y /= 0
  pure $ EParens [ESym "/", x, EInt y]
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
  arity2 "=" t
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
gen_pow t@TInt = do
  EParens [sym, n, _] <- arity2 "^" t
  EInt m <- genInt
  guard $ m > 0 && m < 100
  pure $ EParens [sym, n, EInt m]
gen_pow t@TDec = do
  EParens [sym, n, _] <- arity2_int_or_dec "^" t
  m <- Gen.choice [genInt, genDec]
  case m of
      EInt m' -> guard $ m' > 0 && m' < 100
      EDec m' -> guard $ m' > 0 && m' < 100
      _ -> error "Impossible"
  pure $ EParens [sym, n, m]
gen_pow _ = mzero

gen_abs :: PactGen
gen_abs t@TInt = arity1 "abs" t
gen_abs t@TDec = arity1 "abs" t
gen_abs _ = mzero

gen_and :: PactGen
gen_and t@TBool = arity2 "and" t
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
gen_ln TDec = do
  n <- Gen.choice [genInt, genDec]
  case n of
      EInt n' -> guard $ n' > 0
      EDec n' -> guard $ n' > 0
      _ -> error "Impossible"
  pure $ EParens [ESym "ln", n]
gen_ln _ = mzero

gen_log :: PactGen
gen_log TInt = do
  n <- EInt <$> Gen.element [2, 8, 10, 16, 64]
  EInt x <- genInt
  guard $ x > 0
  pure $ EParens [ESym "log", n, EInt x]
gen_log TDec = do
  n <- EInt <$> Gen.element [2, 8, 10, 16, 64]
  EDec x <- genDec
  guard $ x > 0
  pure $ EParens [ESym "log", n, EDec x]
gen_log _ = mzero

gen_mod :: PactGen
gen_mod TInt = do
  x <- genExpr TInt
  EInt y <- genInt
  guard $ y /= 0
  pure $ EParens [ESym "mod", x, EInt y]
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
gen_sqrt t@TDec = do
  EParens [sym, n] <- arity1 "sqrt" t
  pure $ EParens [sym, EParens [ESym "abs", n]]
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
  t <- genExpr TTime
  s <- genExpr TInt
  return $ EParens [ESym "add-time", t, s]
gen_add_time _ = mzero

gen_days :: PactGen
gen_days t@TDec = arity1_int_or_dec "days" t
gen_days _ = mzero

gen_diff_time :: PactGen
gen_diff_time _ = mzero -- jww (2022-12-15): TODO

gen_format_time :: PactGen
gen_format_time _ = mzero -- jww (2022-12-15): TODO

gen_hours :: PactGen
gen_hours _ = mzero -- jww (2022-12-15): TODO

gen_minutes :: PactGen
gen_minutes _ = mzero -- jww (2022-12-15): TODO

gen_parse_time :: PactGen
gen_parse_time _ = mzero -- jww (2022-12-15): TODO

gen_time :: PactGen
gen_time _ = mzero -- jww (2022-12-15): TODO

gen_decrypt_cc20p1305 :: PactGen
gen_decrypt_cc20p1305 _ = mzero -- jww (2022-12-15): TODO

gen_validate_keypair :: PactGen
gen_validate_keypair _ = mzero -- jww (2022-12-15): TODO

gen_define_keyset :: PactGen
gen_define_keyset _ = mzero -- jww (2022-12-15): TODO

gen_enforce_keyset :: PactGen
gen_enforce_keyset _ = mzero -- jww (2022-12-15): TODO

gen_keys_2 :: PactGen
gen_keys_2 _ = mzero -- jww (2022-12-15): TODO

gen_keys_all :: PactGen
gen_keys_all _ = mzero -- jww (2022-12-15): TODO

gen_keys_any :: PactGen
gen_keys_any _ = mzero -- jww (2022-12-15): TODO

gen_read_keyset :: PactGen
gen_read_keyset _ = mzero -- jww (2022-12-15): TODO

gen_create_table :: PactGen
gen_create_table _ = mzero -- jww (2022-12-15): TODO

gen_describe_keyset :: PactGen
gen_describe_keyset _ = mzero -- jww (2022-12-15): TODO

gen_describe_module :: PactGen
gen_describe_module _ = mzero -- jww (2022-12-15): TODO

gen_describe_table :: PactGen
gen_describe_table _ = mzero -- jww (2022-12-15): TODO

gen_describe_namespace :: PactGen
gen_describe_namespace _ = mzero -- jww (2022-12-15): TODO

gen_insert :: PactGen
gen_insert _ = mzero -- jww (2022-12-15): TODO

gen_keylog :: PactGen
gen_keylog _ = mzero -- jww (2022-12-15): TODO

gen_keys :: PactGen
gen_keys _ = mzero -- jww (2022-12-15): TODO

gen_read :: PactGen
gen_read _ = mzero -- jww (2022-12-15): TODO

gen_select :: PactGen
gen_select _ = mzero -- jww (2022-12-15): TODO

gen_txids :: PactGen
gen_txids _ = mzero -- jww (2022-12-15): TODO

gen_txlog :: PactGen
gen_txlog _ = mzero -- jww (2022-12-15): TODO

gen_update :: PactGen
gen_update _ = mzero -- jww (2022-12-15): TODO

gen_with_default_read :: PactGen
gen_with_default_read _ = mzero -- jww (2022-12-15): TODO

gen_with_read :: PactGen
gen_with_read _ = mzero -- jww (2022-12-15): TODO

gen_write :: PactGen
gen_write _ = mzero -- jww (2022-12-15): TODO

gen_fold_db :: PactGen
gen_fold_db _ = mzero -- jww (2022-12-15): TODO

gen_compose_capability :: PactGen
gen_compose_capability _ = mzero -- jww (2022-12-15): TODO

gen_create_module_guard :: PactGen
gen_create_module_guard _ = mzero -- jww (2022-12-15): TODO

gen_create_pact_guard :: PactGen
gen_create_pact_guard _ = mzero -- jww (2022-12-15): TODO

gen_create_user_guard :: PactGen
gen_create_user_guard _ = mzero -- jww (2022-12-15): TODO

gen_enforce_guard :: PactGen
gen_enforce_guard _ = mzero -- jww (2022-12-15): TODO

gen_install_capability :: PactGen
gen_install_capability _ = mzero -- jww (2022-12-15): TODO

gen_keyset_ref_guard :: PactGen
gen_keyset_ref_guard _ = mzero -- jww (2022-12-15): TODO

gen_require_capability :: PactGen
gen_require_capability _ = mzero -- jww (2022-12-15): TODO

gen_with_capability :: PactGen
gen_with_capability _ = mzero -- jww (2022-12-15): TODO

gen_emit_event :: PactGen
gen_emit_event _ = mzero -- jww (2022-12-15): TODO

gen_create_principal :: PactGen
gen_create_principal _ = mzero -- jww (2022-12-15): TODO

gen_validate_principal :: PactGen
gen_validate_principal _ = mzero -- jww (2022-12-15): TODO

gen_is_principal :: PactGen
gen_is_principal _ = mzero -- jww (2022-12-15): TODO

gen_typeof_principal :: PactGen
gen_typeof_principal _ = mzero -- jww (2022-12-15): TODO

gen_use :: PactGen
gen_use _ = mzero -- jww (2022-12-15): TODO

gen_module :: PactGen
gen_module _ = mzero -- jww (2022-12-15): TODO

gen_interface :: PactGen
gen_interface _ = mzero -- jww (2022-12-15): TODO
