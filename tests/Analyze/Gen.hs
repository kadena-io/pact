{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Analyze.Gen where

import           Control.DeepSeq
import           Control.Lens               hiding (op, (...), Empty)
import           Control.Monad.Catch        (MonadCatch (catch), SomeException)
import           Control.Monad.Reader       (MonadReader, ReaderT (runReaderT))
import           Control.Monad.State.Strict (MonadState, runStateT)
import qualified Data.Decimal               as Decimal
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Data.Type.Equality         ((:~:) (Refl))
import           GHC.Natural                (Natural)
import           GHC.Stack                  (HasCallStack)
import           Hedgehog                   hiding (Update, Var)
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range
import           Numeric.Interval           (Interval, inf, midpoint, sup,
                                             (+/-), (...))
import           Numeric.Interval.Exception (EmptyInterval)
import           Pact.Analyze.Errors
import           Pact.Analyze.Types         hiding (Term, Var)
import qualified Pact.Analyze.Types         as Analyze
import           Pact.Analyze.Util          (dummyInfo)

import           Pact.Types.Persistence     (WriteType)
import           Pact.Types.Pretty          (renderCompactString', pretty, vsep)
import           Pact.Types.Term            (Name (Name))
import qualified Pact.Types.Term            as Pact

import           Analyze.TimeGen


-- Note [EmptyInterval]: For both genDecimal and genInteger, it's possible that
-- the range is too small to generate even one number. When this is true,
-- these'll throw EmptyInterval. This is caught and discarded by the
-- properties.

data GenEnv = GenEnv
  { _envTables  :: ![(TableName, ESchema)]
  , _envKeysets :: ![(Pact.KeySet, Guard)]
  }

data GenState = GenState
  { _idGen            :: !TagId
  , _registryKeySets  :: !(Map.Map String (Pact.KeySet, Guard))
  , _txKeySets        :: !(Map.Map String (Pact.KeySet, Guard))
  , _txDecimals       :: !(Map.Map String Decimal)
  , _txIntegers       :: !(Map.Map String Integer)
  } deriving Show

makeLenses ''GenEnv
makeLenses ''GenState

emptyGenState :: GenState
emptyGenState = GenState 0 Map.empty Map.empty Map.empty Map.empty

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

  BoundedList :: BoundedType -> BoundedType

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

mkInt :: MonadGen m => Core Analyze.Term 'TyInteger -> m ETerm
mkInt = pure . Some SInteger . Inj

mkDec :: MonadGen m => Numerical Analyze.Term 'TyDecimal -> m ETerm
mkDec = pure . Some SDecimal . Inj . Numerical

mkBool :: MonadGen m => Core Analyze.Term 'TyBool -> m ETerm
mkBool = pure . Some SBool . Inj

-- | When we know what type we'll be receiving from an existential we can
-- unsafely extract it.
class Extract a where
  extract :: HasCallStack => ETerm -> Analyze.Term a

instance Extract 'TyInteger where
  extract = \case
    Some SInteger x -> x
    other -> error (show other)

instance Extract 'TyDecimal where
  extract = \case
    Some SDecimal x -> x
    other -> error (show other)

instance Extract 'TyStr where
  extract = \case
    Some SStr x -> x
    other -> error (show other)

instance Extract 'TyBool where
  extract = \case
    Some SBool x -> x
    other -> error (show other)

instance Extract 'TyTime where
  extract = \case
    Some STime x -> x
    other -> error (show other)

instance Extract 'TyGuard where
  extract = \case
    Some SGuard x -> x
    other -> error (show other)

-- TODO: cover Var, objects
-- TODO: we might want to reweight these by using `Gen.frequency`.
genCore :: (HasCallStack, MonadGen m) => BoundedType -> m ETerm
genCore (BoundedInt size) = Gen.recursive Gen.choice [
    Some SInteger . Lit' <$> genInteger size
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
    Some SDecimal . Lit' <$> genDecimal size
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
    Some SStr . StrLit
      -- TODO: unicode (SBV has trouble with some unicode characters)
      <$> Gen.string (Range.exponential 1 len) Gen.latin1
  ] [
    scale 4 $ Gen.subtermM2
      (genCore (BoundedString (len `div` 2)))
      (genCore (BoundedString (len `div` 2))) $ \x y ->
        pure $ Some SStr $ Inj $ StrConcat (extract x) (extract y)
  ]
genCore BoundedBool = Gen.recursive Gen.choice [
    Some SBool . Lit' <$> Gen.bool
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
         mkBool $ StrComparison op (extract x) (extract y)
  , do op <- Gen.element [Eq, Neq]
       Gen.subtermM2 (genCore BoundedBool) (genCore BoundedBool) $ \x y ->
         mkBool $ BoolComparison op (extract x) (extract y)
  , do op <- Gen.element [AndOp, OrOp]
       Gen.subtermM2 (genCore BoundedBool) (genCore BoundedBool) $ \x y ->
         mkBool $ Logical op [extract x, extract y]
  , do op <- Gen.element [Eq', Neq']
       EType ty <- Gen.element
         -- TODO?: keyset
         [EType SInteger, EType SDecimal, EType SBool, EType SStr, EType STime]
       let aSize = case ty of
             SInteger -> intSize
             SDecimal -> decSize
             SStr     -> strSize
             SBool    -> BoundedBool
             STime    -> BoundedTime
             _        -> error "impossible"
       Gen.subtermM2
         (genCore (BoundedList aSize)) (genCore (BoundedList aSize)) $
           \elst1 elst2 -> case (elst1, elst2) of
             (Some (SList lty1) l1, Some (SList lty2) l2) ->
               case singEq lty1 ty of
                 Nothing   -> error "impossible"
                 Just Refl -> case singEq lty2 ty of
                   Nothing   -> error "impossible"
                   Just Refl -> mkBool $ ListEqNeq ty op l1 l2
             _ -> error (show (elst1, elst2))
  , Gen.subtermM (genCore BoundedBool) $ \x ->
      mkBool $ Logical NotOp [extract x]
  ]
genCore BoundedTime = Gen.recursive Gen.choice [
    Some STime . Lit' <$> Gen.enumBounded -- Gen.int64
  ] $ scale 4 <$> [
    Gen.subtermM2 (genCore BoundedTime) (genCore (BoundedInt 1e9)) $ \x y ->
      pure $ Some STime $ Inj $ IntAddTime (extract x) (extract y)
  , Gen.subtermM2 (genCore BoundedTime) (genCore (BoundedDecimal 1e9)) $ \x y ->
      pure $ Some STime $ Inj $ DecAddTime (extract x) (extract y)
  ]
genCore BoundedKeySet = Some SGuard . Lit' . Guard
  <$> genInteger (0 ... 2)
genCore bound@(BoundedList elemBound) = Gen.recursive Gen.choice
  [ Gen.subtermM2 (genCore (BoundedInt (0 ... 5))) (genCore elemBound) $
      \elst1 elst2 -> case (elst1, elst2) of
      (Some SInteger i, Some ty a)
        -> pure $ Some (SList ty) $ Inj $ MakeList ty i a
      _ -> listError elst1 elst2
  ]
  -- EqNeq, At, Contains
  [ Gen.subtermM (genCore bound) $ \case
      Some lty@(SList ty) lst -> pure $ Some lty $ Inj $ ListReverse ty lst
      other -> error (show other)
  , Gen.subtermM (genCore bound) $ \case
      Some lty@(SList ty) lst -> pure $ Some lty $ Inj $ ListSort ty lst
      other -> error (show other)
  , Gen.subtermM2 (genCore bound) (genCore bound) $ \elst1 elst2 ->
    case (elst1, elst2) of
      (Some lty@(SList ty) l1, Some lty2 l2) -> case singEq lty lty2 of
        Nothing   -> error "impossible"
        Just Refl -> pure $ Some lty $ Inj $ ListConcat ty l1 l2
      _ -> listError elst1 elst2
  , Gen.subtermM2 (genCore bound) (genCore (BoundedInt (0 +/- 10))) $
      \elst1 elst2 -> case (elst1, elst2) of
      (Some lty@(SList ty) l, Some SInteger i)
        -> pure $ Some lty $ Inj $ ListDrop ty i l
      _ -> listError elst1 elst2
  , Gen.subtermM2 (genCore bound) (genCore (BoundedInt (0 +/- 10))) $
      \elst1 elst2 -> case (elst1, elst2) of
      (Some lty@(SList ty) l, Some SInteger i)
        -> pure $ Some lty $ Inj $ ListTake ty i l
      _ -> listError elst1 elst2
  -- Note: we currently use bounded list checking so anything beyond 10 is
  -- pointless
  -- LiteralList
  -- , Gen.subtermM
  ]

listError :: HasCallStack => ETerm -> ETerm -> a
listError a@(Some aTy _) b@(Some bTy _) = error $ renderCompactString' $ vsep
  [ "expected two lists, got"
  , pretty a <> ": " <> pretty aTy
  , "/"
  , pretty a
  , "and"
  , pretty b <> ": " <> pretty bTy
  , "/"
  , pretty b
  ]

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
  , genTerm (BoundedList intSize)
  , genTerm (BoundedList decSize)
  , genTerm (BoundedList strSize)
  , genTerm (BoundedList BoundedBool)
  , genTerm (BoundedList BoundedTime)
  ]

genTerm
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m, HasCallStack)
  => BoundedType -> m ETerm
genTerm size@(BoundedList _) = scale 2 $ genCore size
genTerm size = scale 2 $ Gen.choice [genCore size, genTermSpecific size]

genTermSpecific
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m, HasCallStack)
  => BoundedType -> m ETerm
genTermSpecific size@(BoundedInt _len) = Gen.choice
  [ do
      base      <- Gen.int    (Range.linear 2 16)
      formatted <- Gen.string (Range.exponential 1 128) (genBaseChar base)
      pure $ Some SInteger $ CoreTerm $ StrToIntBase
        (Lit' (fromIntegral base :: Integer))
        (Lit' (Str formatted))
  , do
      formatted <- Gen.string (Range.exponential 1 128) (genBaseChar 10)
      pure $ Some SInteger $ CoreTerm $ StrToInt $ Lit' $ Str formatted
  -- TODO:
  -- , Some SInteger . ReadInteger . StrLit <$> genIntegerName len
  , genTermSpecific' size
  ]
genTermSpecific BoundedBool       = Gen.choice
  [
  -- TODO: temporarily disabled pending
  -- https://github.com/kadena-io/pact/issues/207
  -- [ Some SBool . Enforce (Just 0) . extract <$> genTerm BoundedBool
  -- , do xs <- Gen.list (Range.linear 0 4) (genTerm BoundedBool)
  --      pure $ Some SBool $ EnforceOne $ case xs of
  --        [] -> Left 0
  --        _  -> Right $ fmap (((Path 0, Path 0),) . extract) xs

  -- TODO(joel): cover these
  -- , do tagId <- genTagId
  --      Some SBool . KsAuthorized tagId . extract <$> genTerm BoundedKeySet
  -- , do tagId <- genTagId
  --      Some SBool . NameAuthorized tagId . extract <$> genTerm strSize

  -- HACK(joel): Right now we "dilute" this choice with literal bools.
  -- Otherwise this tends to hang forever. Fix this properly (why does scale
  -- not work?).
    Some SBool . Lit' <$> Gen.bool
  , Some SBool . Lit' <$> Gen.bool
  , Some SBool . Lit' <$> Gen.bool
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
  [ pure $ Some SStr PactVersion
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
              pure (Lit' "{}", [tm])
         , scale 4 $ do
              tm1 <- genFormattableTerm
              tm2 <- genFormattableTerm
              str <- Gen.element ["{} {}", "{} / {}", "{} - {}"]
              pure (Lit' str, [tm1, tm2])
         , scale 8 $ do
              tm1 <- genFormattableTerm
              tm2 <- genFormattableTerm
              tm3 <- genFormattableTerm
              str <- Gen.element ["{} {} {}", "{} / {} / {}", "{} - {} - {}"]
              pure (Lit' str, [tm1, tm2, tm3])
         ]
       pure $ Some SStr $ Format str tms
  , scale 4 $ do
       -- just generate literal format strings here so this tests something
       -- interesting
       format        <- genFormat
       Some STime t2 <- genTerm BoundedTime
       pure $ Some SStr $ FormatTime (StrLit (showTimeFormat format)) t2
  , let genHashableTerm = Gen.choice
          [ genTerm intSize
          , genTerm strSize
          , genTerm BoundedBool
          ]
    in Some SStr . Hash <$> genHashableTerm
  , genTermSpecific' size
  ]
genTermSpecific BoundedKeySet = scale 2 $
  Some SGuard . ReadKeySet . StrLit <$> genKeySetName
genTermSpecific (BoundedDecimal len) = scale 2 $
  Some SDecimal . ReadDecimal . StrLit <$> genDecimalName len
genTermSpecific BoundedTime = scale 8 $ Gen.choice
  [ do
       format  <- genFormat
       timeStr <- genTimeOfFormat format
       pure $ Some STime $ ParseTime (Just (StrLit (showTimeFormat format))) $
         StrLit timeStr
  , do
       timeStr <- genTimeOfFormat standardTimeFormat
       pure $ Some STime $ ParseTime Nothing $ StrLit timeStr
  ]
genTermSpecific (BoundedList _)
  = error "There are no term-specific list constructors"

genBaseChar :: MonadGen m => Int -> m Char
genBaseChar base = Gen.element $
  take (2 * base) "00112233445566778899aAbBcCdDeEfF"

genWriteType :: MonadGen m => m WriteType
genWriteType = Gen.enumBounded

genTagId :: MonadState GenState m => m TagId
genTagId = do
  idGen %= succ
  use idGen

--
-- TODO: in these gen* functions, consider instituing a chance to fail to
-- update the appropriate map; this would generate test cases where the user
-- performs e.g. (read-integer "foo") when the tx metadata does not contain the
-- requested key. it would be nice if we knew that concrete and symbolic
-- evaluators coincided here. one challenge is that there is no way to
-- currently tell the symbolic evaluator that we *know* the tx environment does
-- not have a certain key -- it assumes the key exists, and, if it wasn't
-- supplied a priori, the value for that key will simply be /free/.
--

genKeySetName
  :: (MonadGen m, MonadReader GenEnv m, MonadState GenState m)
  => m String
genKeySetName = do
  idGen %= succ
  TagId nat <- use idGen
  keysets   <- view envKeysets
  keyset    <- Gen.element keysets
  let k = show nat
  txKeySets . at k ?= keyset
  pure k

genDecimalName
  :: (MonadGen m, MonadState GenState m)
  => NumBound -> m String
genDecimalName size = do
  idGen %= succ
  TagId nat <- use idGen
  d         <- genDecimal size
  let k = show nat
  txDecimals . at k ?= d
  pure k

genIntegerName
  :: (MonadGen m, MonadState GenState m)
  => NumBound -> m String
genIntegerName size = do
  idGen %= succ
  TagId nat <- use idGen
  i         <- genInteger size
  let k = show nat
  txIntegers . at k ?= i
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
  --      Some ty tm <- genTerm boundedTy
  --      pure $ Some ty $ Sequence eTm tm
  [ do
       Some SBool b <- genTerm BoundedBool
       Some tyt1 t1 <- genTerm boundedTy
       Some tyt2 t2 <- genTerm boundedTy
       case singEq tyt1 tyt2 of
         Just Refl -> pure $ Some tyt1 $ IfThenElse tyt1 b (Path 0, t1) (Path 0, t2)
         Nothing   -> error "t1 and t2 must have the same type"
  ]

genType :: MonadGen m => m EType
genType = Gen.element
  [ EType SInteger, EType (SList SInteger)
  , EType SDecimal, EType (SList SDecimal)
  , EType SBool   , EType (SList SBool)
  , EType SStr    , EType (SList SStr)
  , EType STime   , EType (SList STime)
  , EType SGuard  , EType (SList SGuard)
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
  footnote $ renderCompactString' $ "term: " <> pretty etm
  pure $ show etm `deepseq` (etm, gState)
  ) `catch` (\(_e :: EmptyInterval)  -> discard) -- see note [EmptyInterval]
    -- also, sometimes term generation fails for mysterious reasons
    `catch` (\(_e :: SomeException) -> discard)

genFormatTime :: Gen (ETerm, GenState)
genFormatTime = do
  format <- genFormat
  (Some STime t2, gState) <- runReaderT
    (runStateT (genTerm BoundedTime) emptyGenState)
    genEnv
  let etm = Some SStr $ FormatTime (StrLit (showTimeFormat format)) t2
  pure (etm, gState)

genParseTime :: Gen (ETerm, GenState)
genParseTime = do
  format  <- genFormat
  timeStr <- genTimeOfFormat format
  let etm = Some STime $ ParseTime (Just (StrLit (showTimeFormat format))) $
        StrLit timeStr
  pure (etm, emptyGenState)

alice, bob :: Pact.PublicKey
alice = "7d0c9ba189927df85c8c54f8b5c8acd76c1d27e923abbf25a957afdf25550804"
bob   = "ac69d9856821f11b8e6ca5cdd84a98ec3086493fd6407e74ea9038407ec9eba9"

genEnv :: GenEnv
genEnv = GenEnv
  [("accounts", ESchema $ normalizeSchema $
      SCons' (SSymbol @"balance") SInteger $
        SCons' (SSymbol @"name") SStr
          SNil'
    )]
  [ (Pact.KeySet [alice, bob] (Name "keys-all" dummyInfo), Guard 0)
  , (Pact.KeySet [alice, bob] (Name "keys-any" dummyInfo), Guard 1)
  , (Pact.KeySet [alice, bob] (Name "keys-2" dummyInfo), Guard 2)
  ]
