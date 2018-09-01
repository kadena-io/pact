{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NumDecimals          #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
module AnalyzeProperties where

import Data.Aeson (toJSON, Value(Object))
import           Bound                       (closed)
import qualified Data.HashMap.Strict as HM
import           GHC.Natural               (Natural)
import           Control.Exception           (ArithException (DivideByZero))
import Control.Lens hiding ((...), op)
import           Control.Monad               ((<=<))
import           Control.Monad.Catch         (catch)
import           Control.Monad.Except        (runExcept)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Morph         (generalize, hoist)
import           Control.Monad.Reader        (ReaderT (runReaderT), MonadReader)
import           Control.Monad.RWS.Strict    (runRWST)
import           Control.Monad.State.Strict  (runStateT, MonadState)
import           Control.Monad.Trans.Class   (MonadTrans (lift))
import           Control.Monad.Trans.Maybe
import qualified Data.Decimal                as Decimal
import qualified Data.Default                as Default
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.SBV                    (unliteral, writeArray, literal)
import qualified Data.SBV.Internals          as SBVI
import qualified Data.Text                   as T
import           Data.Type.Equality          ((:~:) (Refl))
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog hiding (Update)
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range
import           Numeric.Interval
import           Numeric.Interval.Exception  (EmptyInterval)
import           Test.Hspec                  (Spec, describe, it, pending)

import           Pact.Analyze.Errors
-- import           Pact.Analyze.Model (allocModelTags)
import           Pact.Analyze.Eval           (runAnalyze)
import           Pact.Analyze.Eval.Term      (evalETerm)
import           Pact.Analyze.Translate      (TranslateM (..),
                                              TranslateState (..),
                                              maybeTranslateType,
                                              mkTranslateEnv, translateNode, IsTest(IsTest))
import           Pact.Analyze.Types          hiding (Term, Object)
import qualified Pact.Analyze.Types          as Analyze
import           Pact.Analyze.Types.Eval     (mkAnalyzeEnv,
                                              mkInitialAnalyzeState, aeKeySets, aeDecimals)
import           Pact.Analyze.Util           (dummyInfo)

import           Pact.Eval                   (liftTerm, reduce)
import           Pact.Native                 (enforceDef, enforceOneDef, lengthDef, pactVersionDef, formatDef, hashDef, ifDef)
import           Pact.Native.Ops
import           Pact.Native.Time
import           Pact.Native.Keysets
import           Pact.Repl                   (initPureEvalEnv)
import           Pact.Repl.Types (LibState)
import           Pact.Typechecker            (typecheckTopLevel)
import           Pact.Types.Exp              (Literal (..), Name(Name))
import           Pact.Types.Persistence (WriteType)
import           Pact.Types.Native           (NativeDef)
import           Pact.Types.Runtime          (PactError (..),
                                              PactErrorType (EvalError),
                                              runEval, EvalEnv, eeMsgBody)
import           Pact.Types.Term             (Term (TApp, TConst, TLiteral), Meta(Meta))
import qualified Pact.Types.Term             as Pact
import qualified Pact.Types.Type             as Pact
import qualified Pact.Types.Typecheck        as Pact

import TimeGen


data GenEnv = GenEnv
  { _envTables        :: ![(TableName, Schema)]
  , _envKeysets       :: ![(Pact.KeySet, KeySet)]
  }

data GenState = GenState
  { _idGen         :: !TagId
  , _namedKeysets  :: !(Map String (Pact.KeySet, KeySet))
  , _namedDecimals :: !(Map String Decimal)
  } deriving Show

makeLenses ''GenEnv
makeLenses ''GenState

-- Note [EmptyInterval]: For both genDecimal and genInteger, it's possible that
-- the range is too small to generate even one number. When this is true,
-- these'll throw EmptyInterval. This is caught and discarded by the
-- properties.

genDecimal :: MonadGen m => NumSize -> m Decimal
genDecimal size = do
  places   <- Gen.word8 Range.constantBounded
  mantissa <- genInteger (size * 10 ^ places)
  pure $ fromPact decimalIso $ Decimal.Decimal places mantissa

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

-- | When we know what type we'll be receiving from an existential we can
-- unsafely extract it.
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

instance Extract KeySet where
  extract = \case
    ESimple TKeySet x -> x
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
  <$> genInteger (0 ... 2)

intSize, decSize, strSize :: SizedType
intSize = SizedInt     (0 +/- 1e25)
decSize = SizedDecimal (0 +/- 1e25)
strSize = SizedString  1000

-- TODO
-- generic:
-- # Let
-- object:
-- # Read
-- string:
-- # Write
-- time:
-- # ParseTime
genAnyTerm
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m)
  => m ETerm
genAnyTerm = Gen.choice
  [ genTerm intSize
  , genTerm decSize
  , genTerm strSize
  , genTerm SizedBool
  , genTerm SizedTime
  -- , genTerm SizedKeySet
  ]

genTerm
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m)
  => SizedType -> m ETerm
genTerm size = Gen.choice [genCore size, genTermSpecific size]

genTermSpecific
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m)
  => SizedType -> m ETerm
genTermSpecific size@SizedInt{} = genTermSpecific' size
genTermSpecific SizedBool          = Gen.choice
  [ ESimple TBool . Enforce (Just 0) . extract <$> genTerm SizedBool
  , do xs <- Gen.list (Range.linear 0 4) (genTerm SizedBool)
       pure $ ESimple TBool $ EnforceOne $ Right $ fmap (((Path 0, Path 0),) . extract) xs
  -- TODO:
  -- , do tagId <- genTagId
  --      ESimple TBool . KsAuthorized tagId . extract <$> genTerm SizedKeySet
  -- , do tagId <- genTagId
  --      ESimple TBool . NameAuthorized tagId . extract <$> genTerm strSize
  , genTermSpecific' SizedBool
  ]
genTermSpecific size@(SizedString _len) = Gen.choice
  -- [ do
  --      tables <- view envTables
  --      (table, schema) <- Gen.element tables
  --      writeType <- genWriteType
  --      tagId <- genTagId
  --      Write writeType tagId table schema
  -- Write
  [ pure (ESimple TStr PactVersion)
  , do
       (str, tms) <- Gen.choice
         [ do
              tm <- genAnyTerm
              pure (lit "{}", [tm])
         , do
              tm1 <- genAnyTerm
              tm2 <- genAnyTerm
              str <- Gen.element ["{} {}", "{} / {}", "{} - {}"]
              pure (lit str, [tm1, tm2])
         , do
              tm1 <- genAnyTerm
              tm2 <- genAnyTerm
              tm3 <- genAnyTerm
              str <- Gen.element ["{} {} {}", "{} / {} / {}", "{} - {} - {}"]
              pure (lit str, [tm1, tm2, tm3])
         ]
       pure $ ESimple TStr $ Format str tms
  , do
       -- just generate literal format strings here so this tests something
       -- interesting
       format           <- genFormat
       ESimple TTime t2 <- genTerm SizedTime
       pure $ ESimple TStr $ FormatTime (lit (showTimeFormat format)) t2
  , ESimple TStr . Hash <$> genAnyTerm
  , genTermSpecific' size
  ]
genTermSpecific SizedKeySet =
  ESimple TKeySet . ReadKeySet . lit <$> genKeySetName
genTermSpecific (SizedDecimal len) =
  ESimple TKeySet . ReadKeySet . lit <$> genDecimalName len
genTermSpecific SizedTime = Gen.choice
  [ do
       -- We simplify a bit here and
       format  <- genFormat
       timeStr <- genTimeOfFormat format
       pure $ ESimple TTime $ ParseTime (Just (lit (showTimeFormat format))) $
         lit timeStr
  , do
       timeStr <- genTimeOfFormat standardTimeFormat
       pure $ ESimple TTime $ ParseTime Nothing $ lit timeStr
  ]

genWriteType :: MonadGen m => m WriteType
genWriteType = Gen.enumBounded

genTagId :: MonadState GenState m => m TagId
genTagId = do
  idGen %= succ
  use idGen

genKeySetName
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m)
  => m String
genKeySetName = do
  idGen %= succ
  TagId nat <- use idGen
  keysets   <- view envKeysets
  -- keysetIx  <- Gen.integral (Range.linear 0 (length keysets))
  -- let keyset = keysets ^?! ix keysetIx
  keyset    <- Gen.element keysets
  let k = show nat
  namedKeysets . at k ?= keyset
  pure k

genDecimalName
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m)
  => NumSize -> m String
genDecimalName size = do
  idGen %= succ
  TagId nat <- use idGen
  d         <- genDecimal size
  let k = show nat
  namedDecimals . at k ?= d
  pure k

genNatural :: MonadGen m => Range Natural -> m Natural
genNatural = Gen.integral

-- Generate a term of a specific type with a generic construct
-- (Let, Sequence, IfThenElse)
genTermSpecific'
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m)
  => SizedType -> m ETerm
genTermSpecific' sizedTy = Gen.choice
  -- TODO: Let
  -- [ do
  --      eTm <- genAnyTerm
  --      ESimple ty tm <- genTerm sizedTy
  --      pure $ ESimple ty $ Sequence eTm tm
  [ do
       ESimple TBool b <- genTerm SizedBool
       ESimple tyt1 t1 <- genTerm sizedTy
       ESimple tyt2 t2 <- genTerm sizedTy
       case typeEq tyt1 tyt2 of
         Just Refl -> pure $ ESimple tyt1 $ IfThenElse b (Path 0, t1) (Path 0, t2)
         Nothing   -> error "t1 and t2 must have the same type"
  ]

toPactTm :: ETerm -> ReaderT (GenEnv, GenState) Maybe (Pact.Term Pact.Ref)
toPactTm = \case
  -- core terms:

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
    -> pure $ TLiteral (LInteger x) dummyInfo
  ESimple TDecimal (CoreTerm (Lit x))
    -> pure $ TLiteral (LDecimal (toPact decimalIso x)) dummyInfo
  ESimple TStr     (CoreTerm (Lit x))
    -> pure $ TLiteral (LString (T.pack x)) dummyInfo
  ESimple TBool    (CoreTerm (Lit x))
    -> pure $ TLiteral (LBool x) dummyInfo
  ESimple TTime    (CoreTerm (Lit x))
    -> pure $ TLiteral (LTime (toPact timeIso x)) dummyInfo

  ESimple TKeySet  (CoreTerm (Lit (KeySet x))) -> do
    keysets <- view (_1 . envKeysets)
    case keysets ^? ix (fromIntegral x) of
      Just (ks, _) -> pure $ Pact.TKeySet ks dummyInfo
      Nothing -> error $ "no keysets found at index " ++ show x

  -- term-specific terms:
  ESimple TBool (Enforce _ x)
    -> mkApp enforceDef [ESimple TBool x]
  ESimple TBool (EnforceOne Left{})
    -> mkApp enforceOneDef []
  ESimple TBool (EnforceOne (Right xs))
    -> mkApp enforceOneDef (ESimple TBool . snd <$> xs)
  -- ESimple TBool (KsAuthorized x)
  -- ESimple TBool (NameAuthorized x)

  ESimple TStr PactVersion -> mkApp pactVersionDef []
  ESimple TStr (Format x ys)
    -> mkApp formatDef (ESimple TStr x : ys)
  ESimple TStr (FormatTime x y)
    -> mkApp defFormatTime [ESimple TStr x, ESimple TTime y]
  ESimple TStr (Hash x) -> mkApp hashDef [x]

  ESimple TKeySet (ReadKeySet x) -> mkApp readKeysetDef [ESimple TStr x]

  ESimple TTime (ParseTime Nothing x) ->
    mkApp parseTimeDef [ESimple TStr x]

  ESimple TTime (ParseTime (Just x) y) ->
    mkApp parseTimeDef [ESimple TStr x, ESimple TStr y]

  -- ESimple ty (Sequence etm tm) -> do
  --   t1 <- toPactTm etm
  --   t2 <- toPactTm (ESimple ty tm)
  --   pure $ TList [t1, t2] (Pact.TyList Pact.TyAny) dummyInfo

--   ESimple ty (Let name _vid etm bodyTm) -> do
--     t1 <- toPactTm etm
--     t2 <- toPactTm (ESimple ty bodyTm)
--     pure $ TBinding [(Pact.Arg name undefined dummyInfo, t1)]
--       t2 undefined dummyInfo

  ESimple ty (IfThenElse t1 (_, t2) (_, t3)) ->
    mkApp ifDef [ESimple TBool t1, ESimple ty t2, ESimple ty t3]

  tm -> error $ "TODO: toPactTm " ++ show tm

  where
    mkApp :: NativeDef -> [ETerm]
      -> ReaderT (GenEnv, GenState) Maybe (Pact.Term Pact.Ref)
    mkApp (_, defTm) args = do
      args' <- traverse toPactTm args
      pure $ TApp (liftTerm defTm) args' dummyInfo

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

toPactTm'
  -- :: Applicative m
  :: Monad m
  => (GenEnv, GenState) -> ETerm -> MaybeT m (Pact.Term Pact.Ref)
toPactTm' envState etm = MaybeT $ do
  pure $ runReaderT (toPactTm etm) envState

toAnalyze :: Pact.Type (Pact.Term Pact.Ref) -> Pact.Term Pact.Ref -> MaybeT IO ETerm
toAnalyze ty tm = do
  let cnst = TConst
        (Pact.Arg "tm" ty dummyInfo)
        "module"
        (Pact.CVRaw tm)
        (Meta Nothing Nothing)
        dummyInfo
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

      translateEnv = mkTranslateEnv IsTest dummyInfo []

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

genAnyTerm' :: Gen (ETerm, GenState)
genAnyTerm' = runReaderT
  (runStateT genAnyTerm (GenState 0 Map.empty Map.empty))
  genEnv

alice, bob :: Pact.PublicKey
alice = "7d0c9ba189927df85c8c54f8b5c8acd76c1d27e923abbf25a957afdf25550804"
bob   = "ac69d9856821f11b8e6ca5cdd84a98ec3086493fd6407e74ea9038407ec9eba9"

genEnv :: GenEnv
genEnv = GenEnv
  [("accounts", Schema $ Map.fromList
    [ ("balance", EType TInt)
    , ("name",    EType TStr)
    ])]
  [ (Pact.KeySet [alice, bob] (Name "keys-all" dummyInfo), KeySet 0)
  , (Pact.KeySet [alice, bob] (Name "keys-any" dummyInfo), KeySet 1)
  , (Pact.KeySet [alice, bob] (Name "keys-2" dummyInfo), KeySet 2)
  ]

fromPactVal :: EType -> Pact.Term Pact.Ref -> IO (Maybe ETerm)
fromPactVal (EType ty) = runMaybeT . toAnalyze (reverseTranslateType ty)
fromPactVal EObjectTy{} = const (pure Nothing) -- TODO

-- Evaluate a term via Pact
pactEval
  :: Pact.Term Pact.Ref
  -> EvalEnv LibState
  -> IO (Either String (Maybe (Pact.Term Pact.Ref)))
pactEval pactTm evalEnv = (do
    let evalState = Default.def
    -- evaluate via pact, convert to analyze term
    (pactVal, _) <- runEval evalState evalEnv (reduce pactTm)
    pactVal' <- pure $ closed pactVal

    pure $ Right pactVal'
  )
    -- discard division by zero, on either the pact or analysis side
    --
    -- future work here is to make sure that if one side throws, the other
    -- does as well.
    `catch` (\(DivideByZero :: ArithException) -> pure $ Right Nothing)
    `catch` (\((PactError err _ _ msg) :: PactError) ->
      case err of
        EvalError ->
          if "Division by 0" `T.isPrefixOf` msg ||
             "Negative precision not allowed" `T.isPrefixOf` msg
          then pure $ Right Nothing
          else pure $ Left (T.unpack msg)
        _ -> pure $ Left (T.unpack msg))

-- Evaluate a term symbolically
analyzeEval :: ETerm -> GenState -> IO (Either String ETerm)
analyzeEval etm@(ESimple ty _tm) (GenState _ keysets decimals) = do
  -- analyze setup
  let tables = []
      args = Map.empty
      state0 = mkInitialAnalyzeState tables

      tags = ModelTags Map.empty Map.empty Map.empty Map.empty Map.empty
        -- this 'Located TVal' is never forced so we don't provide it
        undefined
        Map.empty

  -- tags <- liftIO $ allocModelTags undefined graph

  Just aEnv <- pure $ mkAnalyzeEnv tables args tags dummyInfo
  -- TODO: also write aeKsAuths
  let writeArray' k v env = writeArray env k v
  let aEnv' = foldr (\(k, v) -> aeKeySets
          %~ writeArray' (literal (KeySetName (T.pack k))) (literal v))
        aEnv (Map.toList (fmap snd keysets))
  let aEnv'' = foldr
          (\(k, v) -> aeDecimals %~ writeArray' (literal k) (literal v))
        aEnv' (Map.toList decimals)

  -- evaluate via analyze
  analyzeVal <- case runExcept $ runRWST (runAnalyze (evalETerm etm)) aEnv'' state0 of
    Right (analyzeVal, _, ()) -> pure analyzeVal
    Left err                  -> error $ describeAnalyzeFailure err

  case analyzeVal of
    AVal _ sval -> case unliteral (SBVI.SBV sval) of
      Just sval' -> pure $ Right $ ESimple ty $ CoreTerm $ Lit sval'
      Nothing    -> pure $ Left $ "couldn't unliteral: " ++ show sval
    _ -> pure $ Left $ "not AVAl: " ++ show analyzeVal
analyzeEval EObject{} _ = pure (Left "TODO: analyzeEval EObject")

mkEvalEnv :: GenState -> IO (EvalEnv LibState)
mkEvalEnv (GenState _ keysets decimals) = do
  evalEnv <- liftIO initPureEvalEnv
  let keysets' = HM.fromList
        $ fmap (\(k, (pks, _ks)) -> (T.pack k, toJSON pks))
        $ Map.toList keysets
      decimals' = HM.fromList
        $ fmap (\(k, v) -> (T.pack k, toJSON (show (toPact decimalIso v))))
        $ Map.toList decimals
      body = Object $ keysets' `HM.union` decimals'
  pure $ evalEnv & eeMsgBody .~ body

prop_evaluation :: Property
prop_evaluation = property $ do
  (etm@(ESimple ty _tm), gState) <- forAll genAnyTerm'
  evalEnv <- liftIO $ mkEvalEnv gState

  -- pact setup
  -- TODO: look at what this reads from gState. does it read the named things?
  let Just pactTm = runReaderT (toPactTm etm) (genEnv, gState)

  (do
      -- evaluate via pact, convert to analyze term
      mPactVal <- liftIO $ pactEval pactTm evalEnv
      pactVal <- case mPactVal of
        Left err             -> footnote err >> failure
        Right Nothing        -> discard
        Right (Just pactVal) -> pure pactVal
      Just (ESimple ty' (CoreTerm (Lit pactSval)))
        <- lift $ fromPactVal (EType ty) pactVal

      sval <- liftIO $ analyzeEval etm gState
      ESimple ty'' (CoreTerm (Lit sval')) <- case sval of
        Left err    -> footnote err >> failure
        Right sval' -> pure sval'

      -- compare results
      case typeEq ty' ty'' of
        Just Refl -> sval' === pactSval
        Nothing   -> EType ty' === EType ty'' -- this'll fail
    )
      -- see note [EmptyInterval]
      `catch` (\(_e :: EmptyInterval)  -> discard)

prop_round_trip_type :: Property
prop_round_trip_type = property $ do
  ety@(EType ty) <- forAll genType
  maybeTranslateType (reverseTranslateType ty) === Just ety

prop_round_trip_term :: Property
prop_round_trip_term = property (do
  (etm@(ESimple ty _tm), gState) <- forAll genAnyTerm'

  etm' <- lift $ runMaybeT $
    (toAnalyze (reverseTranslateType ty) <=< toPactTm' (genEnv, gState)) etm

  etm' === Just etm)
    -- XXX
    -- `catch` (\(_e :: EmptyInterval)  -> discard)

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

-- tm' = Format (CoreTerm (Lit "{}")) [ ESimple TInt (CoreTerm (Lit 0)) ]
-- ty' = TStr

-- gState' = GenState 0 Map.empty Map.empty
-- etm' = ESimple ty' tm'
