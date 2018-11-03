{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module Analyze.Gen where

import           Control.DeepSeq
import           Control.Lens               hiding (op, (...))
import           Control.Monad.Catch        (MonadCatch (catch))
import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT))
import           Control.Monad.State.Strict (MonadState, runStateT)
import qualified Data.Decimal               as Decimal
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Data.Type.Equality         ((:~:) (Refl))
import           GHC.Natural                (Natural)
import           GHC.Stack                  (HasCallStack)
import           Hedgehog                   hiding (Update)
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           Numeric.Interval           (Interval, inf, midpoint, sup,
                                             (+/-), (...))
import           Numeric.Interval.Exception (EmptyInterval)
import           Pact.Analyze.Errors
import           Pact.Analyze.Types         hiding (Object, Term)
import qualified Pact.Analyze.Types         as Analyze
import           Pact.Analyze.Util          (dummyInfo)

import           Pact.Types.Persistence     (WriteType)
import qualified Pact.Types.Term            as Pact
import           Pact.Types.Term            (Name(Name))

import           Analyze.TimeGen


-- Note [EmptyInterval]: For both genDecimal and genInteger, it's possible that
-- the range is too small to generate even one number. When this is true,
-- these'll throw EmptyInterval. This is caught and discarded by the
-- properties.

data GenEnv = GenEnv
  { _envTables  :: ![(TableName, Schema)]
  , _envKeysets :: ![(Pact.KeySet, KeySet)]
  }

data GenState = GenState
  { _idGen         :: !TagId
  , _namedKeysets  :: !(Map String (Pact.KeySet, KeySet))
  , _namedDecimals :: !(Map String Decimal)
  } deriving Show

makeLenses ''GenEnv
makeLenses ''GenState

emptyGenState :: GenState
emptyGenState = GenState 0 Map.empty Map.empty

-- Explicitly shrink the size parameter to generate smaller terms.
scale :: MonadGen m => Size -> m a -> m a
scale n = Gen.scale (`div` n)

genDecimal :: MonadGen m => NumBound -> m Decimal
genDecimal size = do
  places   <- Gen.word8 Range.constantBounded
  mantissa <- genInteger (size * 10 ^ places)
  pure $ fromPact decimalIso $ Decimal.Decimal places mantissa

genInteger :: MonadGen m => NumBound -> m Integer
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
type NumBound = Interval Double

data BoundedType where
  BoundedInt     :: NumBound -> BoundedType
  BoundedDecimal :: NumBound -> BoundedType
  BoundedString  :: Int      -> BoundedType
  BoundedTime    ::             BoundedType
  BoundedBool    ::             BoundedType
  BoundedKeySet  ::             BoundedType
  -- TODO: cover objects

arithSize :: ArithOp -> NumBound -> (NumBound, NumBound)
arithSize op size = case op of
  Add -> (size, size)
  Sub -> (size, size)
  Mul -> (sqrt size, sqrt size)
  Div -> (size, size)
  Pow -> error "not yet implemented: we don't symbolically interpret this operator"
  Log -> error "not yet implemented: we don't symbolically interpret this operator"

unaryArithSize :: UnaryArithOp -> NumBound -> NumBound
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

mkBool :: MonadGen m => Core Analyze.Term Bool -> m ETerm
mkBool = pure . ESimple TBool . Inj

-- | When we know what type we'll be receiving from an existential we can
-- unsafely extract it.
class Extract a where
  extract :: HasCallStack => ETerm -> Analyze.Term a

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

-- TODO: cover Var, objects
-- TODO: we might want to reweight these by using `Gen.frequency`.
genCore :: MonadGen m => BoundedType -> m ETerm
genCore (BoundedInt size) = Gen.recursive Gen.choice [
    ESimple TInt . CoreTerm . Lit <$> genInteger size
  ] $ scale 4 <$> [
    Gen.subtermM2 (genCore (BoundedInt size)) (genCore (BoundedInt (1 ... 1e3))) $
      \x y -> mkInt $ Numerical $ ModOp (extract x) (extract y)
  , do op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (BoundedInt size1)) (genCore (BoundedInt size2)) $
         \x y -> mkInt $ Numerical $ IntArithOp op (extract x) (extract y)
  , do op <- genUnaryArithOp
       let size' = unaryArithSize op size
       Gen.subtermM (genCore (BoundedInt size')) $
         mkInt . Numerical . IntUnaryArithOp op . extract
  , Gen.subtermM (genCore (BoundedDecimal size)) $ \x -> do
    op <- genRoundingLikeOp
    mkInt $ Numerical $ RoundingLikeOp1 op (extract x)
  , Gen.subtermM (genCore strSize) $ mkInt . StrLength . extract
  ]
genCore bounded@(BoundedDecimal size) = Gen.recursive Gen.choice [
    ESimple TDecimal . CoreTerm . Lit <$> genDecimal size
  ] $ scale 4 <$> [
    do op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (BoundedDecimal size1)) (genCore (BoundedDecimal size2)) $
         \x y -> mkDec $ DecArithOp op (extract x) (extract y)
  , do
       op <- genUnaryArithOp
       let size' = unaryArithSize op size
       Gen.subtermM (genCore (BoundedDecimal size')) $
         mkDec . DecUnaryArithOp op . extract
  , do op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (BoundedDecimal size1)) (genCore (BoundedInt size2)) $
         \x y -> mkDec $ DecIntArithOp op (extract x) (extract y)
  , do
       op <- genArithOp
       let (size1, size2) = arithSize op size
       Gen.subtermM2 (genCore (BoundedInt size1)) (genCore (BoundedDecimal size2)) $
         \x y -> mkDec $ IntDecArithOp op (extract x) (extract y)
  , Gen.subtermM2 (genCore bounded) (genCore (BoundedInt (0 +/- 255))) $ \x y -> do
      op <- genRoundingLikeOp
      mkDec $ RoundingLikeOp2 op (extract x) (extract y)
  ]
genCore (BoundedString len) = Gen.recursive Gen.choice [
    ESimple TStr . CoreTerm . Lit <$> Gen.string (Range.exponential 1 len) Gen.unicode
  ] [
    scale 4 $ Gen.subtermM2
      (genCore (BoundedString (len `div` 2)))
      (genCore (BoundedString (len `div` 2))) $ \x y ->
        pure $ ESimple TStr $ Inj $ StrConcat (extract x) (extract y)
  ]
genCore BoundedBool = Gen.recursive Gen.choice [
    ESimple TBool . CoreTerm . Lit <$> Gen.bool
  ] $ scale 4 <$> [
    do op <- genComparisonOp
       Gen.subtermM2 (genCore intSize) (genCore intSize) $ \x y -> do
         mkBool $ IntegerComparison op (extract x) (extract y)
  , do op <- genComparisonOp
       Gen.subtermM2 (genCore decSize) (genCore decSize) $ \x y ->
         mkBool $ DecimalComparison op (extract x) (extract y)
  , do op <- genComparisonOp
       Gen.subtermM2 (genCore BoundedTime) (genCore BoundedTime) $ \x y ->
         mkBool $ TimeComparison op (extract x) (extract y)
  , do op <- genComparisonOp
       Gen.subtermM2 (genCore strSize) (genCore strSize) $ \x y ->
         mkBool $ StringComparison op (extract x) (extract y)
  , do op <- Gen.element [Eq, Neq]
       Gen.subtermM2 (genCore BoundedBool) (genCore BoundedBool) $ \x y ->
         mkBool $ BoolComparison op (extract x) (extract y)
  , do op <- Gen.element [AndOp, OrOp]
       Gen.subtermM2 (genCore BoundedBool) (genCore BoundedBool) $ \x y ->
         mkBool $ Logical op [extract x, extract y]
  , Gen.subtermM (genCore BoundedBool) $ \x ->
      mkBool $ Logical NotOp [extract x]
  ]
genCore BoundedTime = Gen.recursive Gen.choice [
    ESimple TTime . CoreTerm . Lit <$> Gen.enumBounded -- Gen.int64
  ] $ scale 4 <$> [
    Gen.subtermM2 (genCore BoundedTime) (genCore (BoundedInt 1e9)) $ \x y ->
      pure $ ESimple TTime $ Inj $ IntAddTime (extract x) (extract y)
  , Gen.subtermM2 (genCore BoundedTime) (genCore (BoundedDecimal 1e9)) $ \x y ->
      pure $ ESimple TTime $ Inj $ DecAddTime (extract x) (extract y)
  ]
genCore BoundedKeySet = ESimple TKeySet . CoreTerm . Lit . KeySet
  <$> genInteger (0 ... 2)

intSize, decSize, strSize :: BoundedType
intSize = BoundedInt     (0 +/- 1e25)
decSize = BoundedDecimal (0 +/- 1e25)
strSize = BoundedString  1000

-- TODO: add tests for these constructs
-- generic:
-- # Let
-- object:
-- # Read
-- string:
-- # Write
genAnyTerm
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m, HasCallStack)
  => m ETerm
genAnyTerm = Gen.choice
  [ genTerm intSize
  , genTerm decSize
  , genTerm strSize
  , genTerm BoundedBool
  , genTerm BoundedTime
  -- , genTerm BoundedKeySet
  ]

genTerm
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m, HasCallStack)
  => BoundedType -> m ETerm
genTerm size = scale 2 $ Gen.choice [genCore size, genTermSpecific size]

genTermSpecific
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m, HasCallStack)
  => BoundedType -> m ETerm
genTermSpecific size@BoundedInt{} = genTermSpecific' size
genTermSpecific BoundedBool       = Gen.choice
  [
  -- TODO: temporarily disabled pending
  -- https://github.com/kadena-io/pact/issues/207
  -- [ ESimple TBool . Enforce (Just 0) . extract <$> genTerm BoundedBool
  -- , do xs <- Gen.list (Range.linear 0 4) (genTerm BoundedBool)
  --      pure $ ESimple TBool $ EnforceOne $ case xs of
  --        [] -> Left 0
  --        _  -> Right $ fmap (((Path 0, Path 0),) . extract) xs

  -- TODO(joel): cover these
  -- , do tagId <- genTagId
  --      ESimple TBool . KsAuthorized tagId . extract <$> genTerm BoundedKeySet
  -- , do tagId <- genTagId
  --      ESimple TBool . NameAuthorized tagId . extract <$> genTerm strSize

  -- HACK(joel): Right now we "dilute" this choice with literal bools.
  -- Otherwise this tends to hang forever. Fix this properly (why does scale
  -- not work?).
    ESimple TBool . CoreTerm . Lit <$> Gen.bool
  , ESimple TBool . CoreTerm . Lit <$> Gen.bool
  , ESimple TBool . CoreTerm . Lit <$> Gen.bool
  , genTermSpecific' BoundedBool
  ]
genTermSpecific size@(BoundedString _len) = scale 2 $ Gen.choice
  -- TODO: cover Write
  -- [ do
  --      tables <- view envTables
  --      (table, schema) <- Gen.element tables
  --      writeType <- genWriteType
  --      tagId <- genTagId
  --      Write writeType tagId table schema
  -- Write
  [ pure $ ESimple TStr PactVersion
  , do
       let genFormattableTerm = Gen.choice
             [ genTerm intSize
             , do
                  x <- genTerm strSize
                  pure x
             , genTerm BoundedBool
             ]
       (str, tms) <- Gen.choice
         [ scale 2 $ do
              tm <- genFormattableTerm
              pure (lit "{}", [tm])
         , scale 4 $ do
              tm1 <- genFormattableTerm
              tm2 <- genFormattableTerm
              str <- Gen.element ["{} {}", "{} / {}", "{} - {}"]
              pure (lit str, [tm1, tm2])
         , scale 8 $ do
              tm1 <- genFormattableTerm
              tm2 <- genFormattableTerm
              tm3 <- genFormattableTerm
              str <- Gen.element ["{} {} {}", "{} / {} / {}", "{} - {} - {}"]
              pure (lit str, [tm1, tm2, tm3])
         ]
       pure $ ESimple TStr $ Format str tms
  , scale 4 $ do
       -- just generate literal format strings here so this tests something
       -- interesting
       format           <- genFormat
       ESimple TTime t2 <- genTerm BoundedTime
       pure $ ESimple TStr $ FormatTime (lit (showTimeFormat format)) t2
  , let genHashableTerm = Gen.choice
          [ genTerm intSize
          , genTerm strSize
          , genTerm BoundedBool
          ]
    in ESimple TStr . Hash <$> genHashableTerm
  , genTermSpecific' size
  ]
genTermSpecific BoundedKeySet = scale 2 $
  ESimple TKeySet . ReadKeySet . lit <$> genKeySetName
genTermSpecific (BoundedDecimal len) = scale 2 $
  ESimple TDecimal . ReadDecimal . lit <$> genDecimalName len
genTermSpecific BoundedTime = scale 8 $ Gen.choice
  [ do
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
  keyset    <- Gen.element keysets
  let k = show nat
  namedKeysets . at k ?= keyset
  pure k

genDecimalName
  :: (MonadGen m, MonadState GenState m)
  => NumBound -> m String
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
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m, HasCallStack)
  => BoundedType -> m ETerm
genTermSpecific' boundedTy = scale 8 $ Gen.choice
  -- TODO: Let
  -- [ do
  --      eTm <- genAnyTerm
  --      ESimple ty tm <- genTerm boundedTy
  --      pure $ ESimple ty $ Sequence eTm tm
  [ do
       ESimple TBool b <- genTerm BoundedBool
       ESimple tyt1 t1 <- genTerm boundedTy
       ESimple tyt2 t2 <- genTerm boundedTy
       case typeEq tyt1 tyt2 of
         Just Refl -> pure $ ESimple tyt1 $ IfThenElse b (Path 0, t1) (Path 0, t2)
         Nothing   -> error "t1 and t2 must have the same type"
  ]

genType :: MonadGen m => m EType
genType = Gen.element
  [ EType TInt, EType TDecimal, EType TBool, EType TStr, EType TTime
  , EType TKeySet
  ]

describeAnalyzeFailure :: AnalyzeFailure -> String
describeAnalyzeFailure (AnalyzeFailure info err) = unlines
  [ show info
  , T.unpack (describeAnalyzeFailureNoLoc err)
  ]

genAnyTerm' :: Gen (ETerm, GenState)
genAnyTerm' = runReaderT (runStateT genAnyTerm emptyGenState) genEnv

safeGenAnyTerm
  :: (MonadCatch m, HasCallStack) => PropertyT m (ETerm, GenState)
safeGenAnyTerm = (do
  (etm, gState) <- forAll genAnyTerm'
  pure $ show etm `deepseq` (etm, gState)
  ) `catch` (\(_e :: EmptyInterval)  -> discard) -- see note [EmptyInterval]

genFormatTime :: Gen (ETerm, GenState)
genFormatTime = do
  format <- genFormat
  (ESimple TTime t2, gState) <- runReaderT
    (runStateT (genTerm BoundedTime) emptyGenState)
    genEnv
  let etm = ESimple TStr $ FormatTime (lit (showTimeFormat format)) t2
  pure (etm, gState)

genParseTime :: Gen (ETerm, GenState)
genParseTime = do
  format  <- genFormat
  timeStr <- genTimeOfFormat format
  let etm = ESimple TTime $ ParseTime (Just (lit (showTimeFormat format))) $
        lit timeStr
  pure (etm, emptyGenState)

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
