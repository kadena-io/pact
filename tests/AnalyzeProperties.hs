{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NumDecimals          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
module AnalyzeProperties where

import           Bound                       (closed)
import           Control.Exception           (ArithException (DivideByZero))
import           Control.Monad               ((<=<))
import           Control.Monad.Catch         (catch)
import           Control.Monad.Except        (runExcept)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Morph         (generalize, hoist)
import           Control.Monad.Reader        (ReaderT (runReaderT))
import           Control.Monad.RWS.Strict    (runRWST)
import           Control.Monad.State.Strict  (runStateT)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Maybe
import qualified Data.Decimal                as Decimal
import qualified Data.Default                as Default
import qualified Data.Map.Strict             as Map
import           Data.SBV                    (unliteral)
import qualified Data.SBV.Internals          as SBVI
import qualified Data.Text                   as T
import           Data.Type.Equality          ((:~:) (Refl))
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Numeric.Interval
import           Numeric.Interval.Exception  (EmptyInterval)
import           Test.Hspec                  (Spec, describe, it, pending)

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval           (runAnalyze)
import           Pact.Analyze.Eval.Term      (evalETerm)
import           Pact.Analyze.Translate      (TranslateM (..),
                                              TranslateState (..),
                                              maybeTranslateType,
                                              mkTranslateEnv, translateNode)
import           Pact.Analyze.Types          hiding (Term)
import qualified Pact.Analyze.Types          as Analyze
import           Pact.Analyze.Types.Eval     (mkAnalyzeEnv,
                                              mkInitialAnalyzeState)
import           Pact.Analyze.Util           (dummyInfo)

import           Pact.Eval                   (liftTerm, reduce)
import           Pact.Native                 (lengthDef)
import           Pact.Native.Ops
import           Pact.Native.Time
import           Pact.Repl                   (initPureEvalEnv)
import           Pact.Typechecker            (typecheckTopLevel)
import           Pact.Types.Exp              (Literal (..))
import           Pact.Types.Native           (NativeDef)
import           Pact.Types.Runtime          (PactError (..),
                                              PactErrorType (EvalError),
                                              runEval)
import           Pact.Types.Term             (Term (TApp, TConst, TLiteral))
import qualified Pact.Types.Term             as Pact
import qualified Pact.Types.Type             as Pact
import qualified Pact.Types.Typecheck        as Pact

-- Note [EmptyInterval]: For both genDecimal and genInteger, it's possible that
-- the range is too small to generate even one number. When this is true,
-- these'll throw EmptyInterval. This is caught and discarded by the
-- properties.

genDecimal :: MonadGen m => NumSize -> m Decimal
genDecimal size = do
  places   <- Gen.word8 Range.constantBounded
  mantissa <- genInteger (size * 10 ^ places)
  pure $ mkDecimal $ Decimal.Decimal places mantissa

genInteger :: MonadGen m => NumSize -> m Integer
genInteger size = Gen.integral $
  Range.exponentialFrom
    (round   (midpoint size))
    (ceiling (inf size))
    (floor   (sup size))

genArithOp :: MonadGen m => m ArithOp
genArithOp = Gen.element [Add, Sub, Mul, Div] -- Pow, Log

genUnaryArithOp :: MonadGen m => m UnaryArithOp
genUnaryArithOp = Gen.element [Negate, Abs] -- Sqrt, Ln, Exp, Signum

genRoundingLikeOp :: MonadGen m => m RoundingLikeOp
genRoundingLikeOp = Gen.element [Round, Ceiling, Floor]

genComparisonOp :: MonadGen m => m ComparisonOp
genComparisonOp = Gen.element [Gt, Lt, Gte, Lte, Eq, Neq]

type ECore = Existential (Core Analyze.Term)

-- TODO: we might want to generalize this to a union of open intervals

-- 'NumSize' bounds the values that a generated expression can evaluate to. We
-- use this to avoid generating large numbers in computationally expensive
-- spots.
type NumSize = Interval Double

data SizedType where
  SizedInt     :: NumSize -> SizedType
  SizedDecimal :: NumSize -> SizedType
  SizedString  :: Int     -> SizedType
  SizedTime    ::            SizedType
  SizedBool    ::            SizedType
  SizedKeySet  ::            SizedType
  -- TODO: objects

-- sizedLog :: NumSize -> (NumSize, NumSize)
-- sizedLog size =
--   let newExp  = logBase 10 size
--       newBase = size ** (1 / newExp)
--   in (newExp, newBase)

arithSize :: ArithOp -> NumSize -> (NumSize, NumSize)
arithSize op size = case op of
  Add -> (size, size)
  Sub -> (size, size)
  Mul -> (sqrt size, sqrt size)
  Div -> (size, size)
  Pow -> error "not yet implemented: we don't symbolically interpret this operator"
  Log -> error "not yet implemented: we don't symbolically interpret this operator"

unaryArithSize :: UnaryArithOp -> NumSize -> NumSize
unaryArithSize op size = case op of
  Negate -> - size
  Sqrt   -> size ** 2
  Abs    -> size -- ?
  Ln     -> error "not yet implemented: we don't symbolically interpret this operator"
  Exp    -> error "not yet implemented: we don't symbolically interpret this operator"
  Signum -> error "not yet implemented: we don't symbolically interpret this operator"

mkInt :: MonadGen m => Core Analyze.Term Integer -> m ETerm
mkInt = pure . ESimple TInt . Inj

mkDec :: MonadGen m => Numerical Analyze.Term Decimal -> m ETerm
mkDec = pure . ESimple TDecimal . Inj . Numerical

class Extract a where
  extract :: ETerm -> Analyze.Term a

instance Extract Integer where
  extract = \case
    ESimple TInt x -> x
    other -> error (show other)

instance Extract Decimal where
  extract = \case
    ESimple TDecimal x -> x
    other -> error (show other)

instance Extract String where
  extract = \case
    ESimple TStr x -> x
    other -> error (show other)

instance Extract Bool where
  extract = \case
    ESimple TBool x -> x
    other -> error (show other)

instance Extract Time where
  extract = \case
    ESimple TTime x -> x
    other -> error (show other)

-- TODO: Var, objects
-- TODO: we might want to reweight these by using `Gen.frequency`.
genCore :: MonadGen m => SizedType -> m ETerm
genCore (SizedInt size) = Gen.recursive Gen.choice [
    ESimple TInt . CoreTerm . Lit <$> genInteger size
  ] [
    Gen.subtermM2 (genCore (SizedInt size)) (genCore (SizedInt (1 ... 1e3))) $
      \x y -> mkInt $ Numerical $ ModOp (extract x) (extract y)
  , do op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (SizedInt size1)) (genCore (SizedInt size2)) $
         \x y -> mkInt $ Numerical $ IntArithOp op (extract x) (extract y)
  , do op <- genUnaryArithOp
       let size' = unaryArithSize op size
       Gen.subtermM (genCore (SizedInt size')) $
         mkInt . Numerical . IntUnaryArithOp op . extract
  , Gen.subtermM (genCore (SizedDecimal size)) $ \x -> do
    op <- genRoundingLikeOp
    mkInt $ Numerical $ RoundingLikeOp1 op (extract x)
  , Gen.subtermM (genCore strSize) $ mkInt . StrLength . extract
  ]
genCore sizeD@(SizedDecimal size) = Gen.recursive Gen.choice [
    ESimple TDecimal . CoreTerm . Lit <$> genDecimal size
  ] [
    do op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (SizedDecimal size1)) (genCore (SizedDecimal size2)) $
         \x y -> mkDec $ DecArithOp op (extract x) (extract y)
  , do
       op <- genUnaryArithOp
       let size' = unaryArithSize op size
       Gen.subtermM (genCore (SizedDecimal size')) $
         mkDec . DecUnaryArithOp op . extract
  , do op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (SizedDecimal size1)) (genCore (SizedInt size2)) $
         \x y -> mkDec $ DecIntArithOp op (extract x) (extract y)
  , do
       op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (SizedInt size1)) (genCore (SizedDecimal size2)) $
         \x y -> mkDec $ IntDecArithOp op (extract x) (extract y)
  , Gen.subtermM2 (genCore sizeD) (genCore (SizedInt (0 +/- 255))) $ \x y -> do
      op <- genRoundingLikeOp
      mkDec $ RoundingLikeOp2 op (extract x) (extract y)
  ]
genCore (SizedString len) = Gen.recursive Gen.choice [
    -- TODO: use unicodeAll?
    ESimple TStr . CoreTerm . Lit <$> Gen.string (Range.exponential 1 len) Gen.unicode
  ] [
    Gen.subtermM2
      (genCore (SizedString (len `div` 2)))
      (genCore (SizedString (len `div` 2))) $ \x y ->
        pure $ ESimple TStr $ Inj $ StrConcat (extract x) (extract y)
  ]
genCore SizedBool = Gen.recursive Gen.choice [
    ESimple TBool . CoreTerm . Lit <$> Gen.bool
  ] [
    do op <- genComparisonOp
       Gen.subtermM2 (genCore intSize) (genCore intSize) $ \x y ->
         pure $ ESimple TBool $ Inj $ IntegerComparison op (extract x) (extract y)
  , do op <- genComparisonOp
       Gen.subtermM2 (genCore decSize) (genCore decSize) $ \x y ->
         pure $ ESimple TBool $ Inj $ DecimalComparison op (extract x) (extract y)
  , do op <- genComparisonOp
       Gen.subtermM2 (genCore SizedTime) (genCore SizedTime) $ \x y ->
         pure $ ESimple TBool $ Inj $ TimeComparison op (extract x) (extract y)
  , do op <- genComparisonOp
       Gen.subtermM2 (genCore strSize) (genCore strSize) $ \x y ->
         pure $ ESimple TBool $ Inj $ StringComparison op (extract x) (extract y)
  , do op <- Gen.element [Eq, Neq]
       Gen.subtermM2 (genCore SizedBool) (genCore SizedBool) $ \x y ->
         pure $ ESimple TBool $ Inj $ BoolComparison op (extract x) (extract y)
  , do op <- Gen.element [AndOp, OrOp]
       Gen.subtermM2 (genCore SizedBool) (genCore SizedBool) $ \x y ->
         pure $ ESimple TBool $ Inj $ Logical op [extract x, extract y]
  , Gen.subtermM (genCore SizedBool) $ \x ->
      pure $ ESimple TBool $ Inj $ Logical NotOp [extract x]
  ]
genCore SizedTime = Gen.recursive Gen.choice [
    ESimple TTime . CoreTerm . Lit <$> Gen.enumBounded -- Gen.int64
  ] [
    Gen.subtermM2 (genCore SizedTime) (genCore (SizedInt 1e9)) $ \x y ->
      pure $ ESimple TTime $ Inj $ IntAddTime (extract x) (extract y)
  , Gen.subtermM2 (genCore SizedTime) (genCore (SizedDecimal 1e9)) $ \x y ->
      pure $ ESimple TTime $ Inj $ DecAddTime (extract x) (extract y)
  ]
genCore SizedKeySet = ESimple TKeySet . CoreTerm . Lit . KeySet
  <$> genInteger (0 ... 100)

intSize, decSize, strSize :: SizedType
intSize = SizedInt     (0 +/- 1e25)
decSize = SizedDecimal (0 +/- 1e25)
strSize = SizedString  1000

-- TODO
-- generic:
-- # IfThenElse
-- # Let
-- # Sequence
-- bool:
-- # Enforce / EnforceOne
-- # *Authorized
-- object:
-- # Read
-- string:
-- # Write
-- # PactVersion
-- # Format
-- # FormatTime
-- # Hash
-- time:
-- # ParseTime
-- keyset:
-- # ReadKeySet
-- decimal:
-- # ReadDecimal
genTerm :: MonadGen m => m ETerm
genTerm = Gen.choice
  [ genCore intSize
  , genCore decSize
  , genCore strSize
  , genCore SizedBool
  , genCore SizedTime
  -- , genCore SizedKeySet
  -- , genTermSpecific SizedBool
  ]

genTermSpecific :: MonadGen m => SizedType -> m ETerm
genTermSpecific SizedBool          = undefined
  -- Enforce
  -- EnforceOne
  -- KsAuthorized
  -- NameAuthorized
  -- Let
  -- Sequence
  -- IfThenElse
genTermSpecific (SizedString _len) = undefined
  -- Write
  -- PactVersion
  -- Format
  -- FormatTime
  -- Hash
  -- Let
  -- Sequence
  -- IfThenElse

toPact :: ETerm -> Maybe (Pact.Term Pact.Ref)
toPact = \case
  ESimple TDecimal (Inj (DecArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple TDecimal x, ESimple TDecimal y]

  ESimple TDecimal (Inj (DecUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [ESimple TDecimal x]

  ESimple TInt (Inj (IntArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple TInt x, ESimple TInt y]

  ESimple TInt (Inj (IntUnaryArithOp op x)) ->
    mkApp (unaryArithOpToDef op) [ESimple TInt x]

  ESimple TInt (Inj (RoundingLikeOp1 op x)) ->
    mkApp (roundingLikeOpToDef op) [ESimple TDecimal x]

  ESimple TDecimal (Inj (RoundingLikeOp2 op x y)) ->
    mkApp (roundingLikeOpToDef op) [ESimple TDecimal x, ESimple TInt y]

  ESimple TDecimal (Inj (DecIntArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple TDecimal x, ESimple TInt y]

  ESimple TDecimal (Inj (IntDecArithOp op x y)) ->
    mkApp (arithOpToDef op) [ESimple TInt x, ESimple TDecimal y]

  ESimple TInt (Inj (ModOp x y)) ->
    mkApp modDef [ESimple TInt x, ESimple TInt y]

  ESimple TInt (Inj (StrLength x)) ->
    mkApp lengthDef [ESimple TStr x]

  ESimple TStr (Inj (StrConcat x y)) ->
    mkApp addDef [ESimple TStr x, ESimple TStr y]

  ESimple TBool (Inj (IntegerComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TInt x, ESimple TInt y]

  ESimple TBool (Inj (DecimalComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TDecimal x, ESimple TDecimal y]

  ESimple TBool (Inj (StringComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TStr x, ESimple TStr y]

  ESimple TBool (Inj (BoolComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TBool x, ESimple TBool y]

  ESimple TBool (Inj (TimeComparison op x y)) ->
    mkApp (comparisonOpToDef op) [ESimple TTime x, ESimple TTime y]

  ESimple TTime (Inj (IntAddTime x y)) ->
    mkApp defAddTime [ESimple TTime x, ESimple TInt y]

  ESimple TTime (Inj (DecAddTime x y)) ->
    mkApp defAddTime [ESimple TTime x, ESimple TDecimal y]

  ESimple TBool (Inj (Logical op args)) ->
    mkApp (logicalOpToDef op) (ESimple TBool <$> args)

  ESimple TInt     (CoreTerm (Lit x))
    -> Just $ TLiteral (LInteger x) dummyInfo
  ESimple TDecimal (CoreTerm (Lit x))
    -> Just $ TLiteral (LDecimal (unMkDecimal x)) dummyInfo
  ESimple TStr     (CoreTerm (Lit x))
    -> Just $ TLiteral (LString (T.pack x)) dummyInfo
  ESimple TBool    (CoreTerm (Lit x))
    -> Just $ TLiteral (LBool x) dummyInfo
  ESimple TTime    (CoreTerm (Lit x))
    -> Just $ TLiteral (LTime (unMkTime x)) dummyInfo

  ESimple TKeySet  (CoreTerm (Lit _x))
    -> error "how do we translate keysets?"

  tm -> error $ "TODO: toPact " ++ show tm

  where
    mkApp :: NativeDef -> [ETerm] -> Maybe (Pact.Term Pact.Ref)
    mkApp (_, defTm) args = do
      args' <- traverse toPact args
      Just $ TApp (liftTerm defTm) args' dummyInfo

    arithOpToDef :: ArithOp -> NativeDef
    arithOpToDef = \case
      Add -> addDef
      Sub -> subDef
      Mul -> mulDef
      Div -> divDef
      Pow -> powDef
      Log -> logDef

    unaryArithOpToDef :: UnaryArithOp -> NativeDef
    unaryArithOpToDef = \case
      Negate -> subDef
      Sqrt   -> sqrtDef
      Ln     -> lnDef
      Exp    -> expDef
      Abs    -> absDef
      Signum -> error "not yet implemented: we don't generate this operator"

    roundingLikeOpToDef :: RoundingLikeOp -> NativeDef
    roundingLikeOpToDef = \case
      Round   -> roundDef
      Ceiling -> ceilDef
      Floor   -> floorDef

    comparisonOpToDef :: ComparisonOp -> NativeDef
    comparisonOpToDef = \case
      Gt  -> gtDef
      Lt  -> ltDef
      Gte -> gteDef
      Lte -> lteDef
      Eq  -> eqDef
      Neq -> neqDef

    logicalOpToDef :: LogicalOp -> NativeDef
    logicalOpToDef = \case
      AndOp -> andDef
      OrOp  -> orDef
      NotOp -> notDef

toPact' :: Applicative m => ETerm -> MaybeT m (Pact.Term Pact.Ref)
toPact' = MaybeT . pure . toPact

toAnalyze :: Pact.Type (Pact.Term Pact.Ref) -> Pact.Term Pact.Ref -> MaybeT IO ETerm
toAnalyze ty tm = do
  let cnst = TConst (Pact.Arg "tm" ty dummyInfo) "module" (Pact.CVRaw tm) (Pact.Meta Nothing Nothing) dummyInfo
      ref = Pact.Ref cnst
  maybeConst <- lift $ Pact.runTC 0 False $ typecheckTopLevel ref
  (_cTy, ast) <- case maybeConst of
    (Pact.TopConst _info _name constTy constAst _meta, _tcState)
      -> pure (constTy, constAst)
    _ -> MaybeT $ pure Nothing

  -- TODO: this is all copied from Translate.hs
  -- TODO: good chance we'll have to change some of these like the initial var
  --       id
  let vertex0 = 0
      nextVertex = succ vertex0
      path0 = Path 0
      nextTagId = succ $ _pathTag path0
      graph0 = pure vertex0
      state0 = TranslateState nextTagId 0 graph0 vertex0 nextVertex Map.empty
        mempty path0 Map.empty

      translateEnv = mkTranslateEnv dummyInfo []

  hoist generalize $
    exceptToMaybeT $
      fmap fst $
        flip runStateT state0 $
          runReaderT
            (unTranslateM (translateNode ast))
            translateEnv

-- This is limited to simple types for now
reverseTranslateType :: Type a -> Pact.Type b
reverseTranslateType = \case
  TBool    -> Pact.TyPrim Pact.TyBool
  TDecimal -> Pact.TyPrim Pact.TyDecimal
  TInt     -> Pact.TyPrim Pact.TyInteger
  TStr     -> Pact.TyPrim Pact.TyString
  TTime    -> Pact.TyPrim Pact.TyTime
  TKeySet  -> Pact.TyPrim Pact.TyKeySet
  TAny     -> Pact.TyAny

genType :: MonadGen m => m EType
genType = Gen.element
  [ EType TInt, EType TDecimal, EType TBool, EType TStr, EType TTime
  -- , EType TKeySet
  ]

describeAnalyzeFailure :: AnalyzeFailure -> String
describeAnalyzeFailure (AnalyzeFailure info err) = unlines
  [ show info
  , T.unpack (describeAnalyzeFailureNoLoc err)
  ]


prop_evaluation :: Property
prop_evaluation = property $ do
  etm@(ESimple ty _tm) <- forAll genTerm

  -- pact setup
  let Just pactTm = toPact etm
      evalState = Default.def
  evalEnv <- liftIO initPureEvalEnv

  -- analyze setup
  let tables = []
      args = Map.empty
      tags = ModelTags Map.empty Map.empty Map.empty Map.empty Map.empty
        -- this 'Located TVal' is never forced so we don't provide it
        undefined Map.empty
      state0 = mkInitialAnalyzeState tables
  Just aEnv <- pure $ mkAnalyzeEnv tables args tags dummyInfo

  (do
      -- evaluate via pact, convert to analyze term
      (pactVal, _) <- liftIO $ runEval evalState evalEnv (reduce pactTm)
      Just pactVal' <- pure $ closed pactVal
      Just (ESimple ty' (CoreTerm (Lit pactVal''))) <- lift $ runMaybeT $
        toAnalyze (reverseTranslateType ty) pactVal'

      -- evaluate via analyze
      analyzeVal <- case runExcept $ runRWST (runAnalyze (evalETerm etm)) aEnv state0 of
        Right (analyzeVal, _, ()) -> pure analyzeVal
        Left err                  -> error $ describeAnalyzeFailure err

      -- compare results
      case typeEq ty ty' of
        Just Refl -> case analyzeVal of
          AVal _ sval -> case unliteral (SBVI.SBV sval) of
            Just sval' -> sval' === pactVal''
            Nothing    -> error $ "couldn't unliteral: " ++ show sval
          _ -> error $ "not AVAl: " ++ show analyzeVal
        Nothing -> EType ty === EType ty' -- this'll fail
    )
      -- discard division by zero, on either the pact or analysis side
      --
      -- future work here is to make sure that if one side throws, the other
      -- does as well.
      `catch` (\(DivideByZero :: ArithException) -> discard)
      `catch` (\((PactError err _ _ msg) :: PactError) ->
        case err of
          EvalError ->
            if "Division by 0" `T.isPrefixOf` msg ||
               "Negative precision not allowed" `T.isPrefixOf` msg
            then discard
            else footnote (T.unpack msg) >> failure
          _ -> footnote (T.unpack msg) >> failure)

      -- see note [EmptyInterval]
      `catch` (\(_e :: EmptyInterval)  -> discard)

prop_round_trip_type :: Property
prop_round_trip_type = property $ do
  ety@(EType ty) <- forAll genType
  maybeTranslateType (reverseTranslateType ty) === Just ety

prop_round_trip_term :: Property
prop_round_trip_term = property $ (do
  etm@(ESimple ty _tm) <- forAll genTerm

  etm' <- lift $ runMaybeT $
    (toAnalyze (reverseTranslateType ty) <=< toPact') etm
  etm' === Just etm)
    `catch` (\(_e :: EmptyInterval)  -> discard)

spec :: Spec
spec = describe "analyze properties" $ do
  -- We first check that our translation of types works in both directions.
  -- This is a pre-requisite to...
  it "should round-trip types" $ require prop_round_trip_type

  -- We check that we can translate terms in both directions. This is a
  -- pre-requisite to...
  it "should round-trip terms" $ require prop_round_trip_term

  -- We should be able to evaluate a term both normally and symbolically, and
  -- get the same result in both places.
  it "should evaluate to the same" $ require prop_evaluation

  it "show round-trip userShow / parse" pending

  it "userShow should have the same result on both the pact and analyze side" pending

-- Usually we run via `spec`, but these are useful for running tests
-- sequentially (so logs from different threads don't clobber each other)
sequentialChecks :: IO Bool
sequentialChecks = checkSequential $ Group "checks"
  [ ("prop_round_trip_type", prop_round_trip_type)
  , ("prop_round_trip_term", prop_round_trip_term)
  , ("prop_evaluation", prop_evaluation)
  ]
