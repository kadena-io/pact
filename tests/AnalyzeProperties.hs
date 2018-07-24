{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumDecimals         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Test.Hspec                  (Spec, describe, it)

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
import           Pact.Native.Ops
import           Pact.Repl                   (initPureEvalEnv)
import           Pact.Typechecker            (typecheckTopLevel)
import           Pact.Types.Exp              (Literal (..))
import           Pact.Types.Native           (NativeDef)
import           Pact.Types.Runtime          (PactError (..),
                                              PactErrorType (EvalError),
                                              runEval)
import           Pact.Types.Term             (Term(TApp, TLiteral, TConst))
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

type ECore = Existential (Core Analyze.Term)

-- TODO: we might want to generalize this to a union of open intervals

-- 'NumSize' bounds the values that a generated expression can evaluate to. We
-- use this to avoid generating large numbers in computationally expensive
-- spots.
type NumSize = Interval Double

data SizedType where
  SizedInt     :: NumSize -> SizedType
  SizedDecimal :: NumSize -> SizedType
  SizedString  ::            SizedType
  SizedTime    ::            SizedType
  SizedBool    ::            SizedType

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
  Pow -> undefined
  Log -> undefined

unaryArithSize :: UnaryArithOp -> NumSize -> NumSize
unaryArithSize op size = case op of
  Negate -> - size
  Sqrt   -> size ** 2
  Abs    -> size -- ?
  Ln     -> undefined
  Exp    -> undefined
  Signum -> undefined

genCore :: MonadGen m => SizedType -> m ECore
genCore (SizedInt size) = Gen.recursive Gen.choice [
    ESimple TInt . Lit <$> genInteger size
  ] [
    Gen.subtermM2 (genCore (SizedInt size)) (genCore (SizedInt (1 ... 1e3))) $
      \(ESimple TInt x) (ESimple TInt y) ->
      pure $ ESimple TInt $ Numerical $ ModOp (Inj x) (Inj y)
  , do
       op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (SizedInt size1)) (genCore (SizedInt size2)) $
         \(ESimple TInt x) (ESimple TInt y) -> do
           pure $ ESimple TInt $ Numerical $ IntArithOp op (Inj x) (Inj y)
  , do
       op <- genUnaryArithOp
       let size' = unaryArithSize op size
       Gen.subtermM (genCore (SizedInt size')) $ \(ESimple TInt x) ->
         pure $ ESimple TInt $ Numerical $ IntUnaryArithOp op (Inj x)
  , Gen.subtermM (genCore (SizedDecimal size)) $ \(ESimple TDecimal x) -> do
    op <- genRoundingLikeOp
    pure $ ESimple TInt $ Numerical $ RoundingLikeOp1 op (Inj x)
  ]
genCore sizeD@(SizedDecimal size) = Gen.recursive Gen.choice [
    ESimple TDecimal . Lit <$> genDecimal size
  ] [
    do
       op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (SizedDecimal size1)) (genCore (SizedDecimal size2)) $
         \(ESimple TDecimal x) (ESimple TDecimal y) ->
         pure $ ESimple TDecimal $ Numerical $ DecArithOp op (Inj x) (Inj y)
  , do
       op <- genUnaryArithOp
       let size' = unaryArithSize op size
       Gen.subtermM (genCore (SizedDecimal size')) $ \(ESimple TDecimal x) ->
         pure $ ESimple TDecimal $ Numerical $ DecUnaryArithOp op (Inj x)
  , do
       op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (SizedDecimal size1)) (genCore (SizedInt size2)) $
         \(ESimple TDecimal x) (ESimple TInt y) ->
           pure $ ESimple TDecimal $ Numerical $ DecIntArithOp op (Inj x) (Inj y)
  , do
       op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (SizedInt size1)) (genCore (SizedDecimal size2)) $
         \(ESimple TInt x) (ESimple TDecimal y) ->
           pure $ ESimple TDecimal $ Numerical $ IntDecArithOp op (Inj x) (Inj y)
  , Gen.subtermM2 (genCore sizeD) (genCore (SizedInt (0 +/- 255))) $
      \(ESimple TDecimal x) (ESimple TInt y) -> do
        op <- genRoundingLikeOp
        pure $ ESimple TDecimal $ Numerical $ RoundingLikeOp2 op (Inj x) (Inj y)
  ]
genCore SizedString = undefined
genCore SizedTime = undefined
genCore SizedBool = undefined

genTerm :: MonadGen m => m ETerm
genTerm = Gen.choice
  [ transformExistential Inj <$> genCore (SizedInt     (0 +/- 1e25))
  , transformExistential Inj <$> genCore (SizedDecimal (0 +/- 1e25))
  ]

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
  Signum -> undefined

roundingLikeOpToDef :: RoundingLikeOp -> NativeDef
roundingLikeOpToDef = \case
  Round   -> roundDef
  Ceiling -> ceilDef
  Floor   -> floorDef

toPact :: ETerm -> Maybe (Pact.Term Pact.Ref)
toPact = \case
  ESimple TDecimal (Inj (DecArithOp op x y)) -> do
    x' <- toPact $ ESimple TDecimal x
    y' <- toPact $ ESimple TDecimal y
    let (_, defTm) = arithOpToDef op
    Just $ TApp (liftTerm defTm) [x', y'] dummyInfo

  ESimple TDecimal (Inj (DecUnaryArithOp op x)) -> do
    x' <- toPact $ ESimple TDecimal x
    let (_, defTm) = unaryArithOpToDef op
    Just $ TApp (liftTerm defTm) [x'] dummyInfo

  ESimple TInt (Inj (IntArithOp op x y)) -> do
    x' <- toPact $ ESimple TInt x
    y' <- toPact $ ESimple TInt y
    let (_, defTm) = arithOpToDef op
    Just $ TApp (liftTerm defTm) [x', y'] dummyInfo

  ESimple TInt (Inj (IntUnaryArithOp op x)) -> do
    x' <- toPact $ ESimple TInt x
    let (_, defTm) = unaryArithOpToDef op
    Just $ TApp (liftTerm defTm) [x'] dummyInfo

  ESimple TInt (Inj (RoundingLikeOp1 op x)) -> do
    x' <- toPact $ ESimple TDecimal x
    let (_, defTm) = roundingLikeOpToDef op
    Just $ TApp (liftTerm defTm) [x'] dummyInfo

  ESimple TDecimal (Inj (RoundingLikeOp2 op x y)) -> do
    x' <- toPact $ ESimple TDecimal x
    y' <- toPact $ ESimple TInt y
    let (_, defTm) = roundingLikeOpToDef op
    Just $ TApp (liftTerm defTm) [x', y'] dummyInfo

  ESimple TDecimal (Inj (DecIntArithOp op x y)) -> do
    x' <- toPact $ ESimple TDecimal x
    y' <- toPact $ ESimple TInt y
    let (_, defTm) = arithOpToDef op
    Just $ TApp (liftTerm defTm) [x', y'] dummyInfo

  ESimple TDecimal (Inj (IntDecArithOp op x y)) -> do
    x' <- toPact $ ESimple TInt x
    y' <- toPact $ ESimple TDecimal y
    let (_, defTm) = arithOpToDef op
    Just $ TApp (liftTerm defTm) [x', y'] dummyInfo

  ESimple TInt (Inj (ModOp x y)) -> do
    x' <- toPact $ ESimple TInt x
    y' <- toPact $ ESimple TInt y
    let (_, modTm) = modDef
    Just $ TApp (liftTerm modTm) [x', y'] dummyInfo

  ESimple TInt     (PureTerm (Lit x))
    -> Just $ TLiteral (LInteger x) dummyInfo
  ESimple TDecimal (PureTerm (Lit x))
    -> Just $ TLiteral (LDecimal (unMkDecimal x)) dummyInfo
  _ -> error "TODO"

toPact' :: Applicative m => ETerm -> MaybeT m (Pact.Term Pact.Ref)
toPact' = MaybeT . pure . toPact

toAnalyze :: Pact.Type (Pact.Term Pact.Ref) -> Pact.Term Pact.Ref -> MaybeT IO ETerm
toAnalyze ty tm = do
  let cnst = TConst (Pact.Arg "tm" ty dummyInfo) "module" (Pact.CVRaw tm) Nothing dummyInfo
      ref = Pact.Ref cnst
  maybeConst <- lift $ Pact.runTC 0 False $ typecheckTopLevel ref
  (_cTy, ast) <- case maybeConst of
    (Pact.TopConst _info _name constTy constAst _meta, _tcState)
      -> pure (constTy, constAst)
    _ -> MaybeT $ pure Nothing
  hoist generalize $
    exceptToMaybeT $
      fmap fst $
        flip runStateT (TranslateState [] 0 0) $
          runReaderT
            (unTranslateM (translateNode ast))
            (dummyInfo, mkTranslateEnv [])

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
  [ EType TInt, EType TDecimal, EType TBool, EType TStr, EType TTime ]

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
      tags = ModelTags Map.empty Map.empty Map.empty Map.empty
        -- this 'Located TVal' is never forced so we don't provide it
        undefined
      state0 = mkInitialAnalyzeState tables
  Just aEnv <- pure $ mkAnalyzeEnv tables args tags dummyInfo

  (do
      -- evaluate via pact, convert to analyze term
      (pactVal, _) <- liftIO $ runEval evalState evalEnv (reduce pactTm)
      Just pactVal' <- pure $ closed pactVal
      Just (ESimple ty' (PureTerm (Lit pactVal''))) <- lift $ runMaybeT $
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
      `catch` (\((PactError EvalError _ _ msg) :: PactError)      ->
        if "Division by 0" `T.isPrefixOf` msg ||
           "Negative precision not allowed" `T.isPrefixOf` msg
        then discard
        else footnote (T.unpack msg) >> failure)

      -- see note [EmptyInterval]
      `catch` (\(_e :: EmptyInterval)  -> discard)

spec :: Spec
spec = describe "analyze properties" $ do
  it "should round-trip terms" $ require $ property $ (do
    etm@(ESimple ty _tm) <- forAll genTerm
    etm' <- lift $ runMaybeT $
      (toAnalyze (reverseTranslateType ty) <=< toPact') etm
    etm' === Just etm)
      `catch` (\(_e :: EmptyInterval)  -> discard)

  it "should round-trip types" $ require $ property $ do
    ety@(EType ty) <- forAll genType
    maybeTranslateType (reverseTranslateType ty) === Just ety

  it "should evaluate to the same" $ require prop_evaluation
