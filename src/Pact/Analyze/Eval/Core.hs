{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Pact.Analyze.Eval.Core where

import           Control.Lens                (over)
import           Data.Foldable               (foldrM)
import qualified Data.Map.Strict             as Map
import           Data.Monoid                 ((<>))
import           Data.SBV                    (Boolean (bnot, false, true),
                                              EqSymbolic ((./=), (.==)),
                                              OrdSymbolic ((.<), (.<=), (.>), (.>=)),
                                              SymWord, ite, true, false, (|||))
import qualified Data.SBV.String             as SBVS
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.SBV.List as SBVL
import Data.SBV.List.Bounded (bfoldr, band, bzipWith, breverse, bsort)

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Numerical
import           Pact.Analyze.Types
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Util
import qualified Pact.Native                 as Pact


-- | Bound on the size of lists we check. This may be user-configurable in the
-- future.
listBound :: Int
listBound = 10

-- Note [Time Representation]
--
-- Pact uses the Thyme library (UTCTime) to represent times. Thyme internally
-- uses a 64-bit count of microseconds since the MJD epoch. So, our symbolic
-- representation is naturally a 64-bit integer.
--
-- The effect from a Pact-user's point of view is that we store 6 digits to
-- the right of the decimal point in times (even though we don't print
-- sub-second precision by default...).
--
-- pact> (add-time (time "2016-07-23T13:30:45Z") 0.001002)
-- "2016-07-23T13:30:45Z"
-- pact> (= (add-time (time "2016-07-23T13:30:45Z") 0.001002)
--          (add-time (time "2016-07-23T13:30:45Z") 0.0010021))
-- true
-- pact> (= (add-time (time "2016-07-23T13:30:45Z") 0.001002)
--          (add-time (time "2016-07-23T13:30:45Z") 0.001003))
-- false

evalIntAddTime
  :: Analyzer m
  => TermOf m 'TyTime
  -> TermOf m 'TyInteger
  -> m (S Time)
evalIntAddTime timeT secsT = do
  time <- eval timeT
  secs <- eval secsT
  -- Convert seconds to milliseconds /before/ conversion to Integer (see note
  -- [Time Representation]).
  pure $ time + fromIntegralS (secs * 1000000)

evalDecAddTime
  :: Analyzer m
  => TermOf m 'TyTime
  -> TermOf m 'TyDecimal
  -> m (S Time)
evalDecAddTime timeT secsT = do
  time <- eval timeT
  secs <- eval secsT
  if isConcreteS secs
  -- Convert seconds to milliseconds /before/ conversion to Integer (see note
  -- [Time Representation]).
  then pure $ time + fromIntegralS (banker'sMethodS (secs * fromInteger' 1000000))
  else throwErrorNoLoc $ PossibleRoundoff
    "A time being added is not concrete, so we can't guarantee that roundoff won't happen when it's converted to an integer."

evalComparisonOp
  :: ( Analyzer m
     , a' ~ Concrete a
     , SymWord a', Show a')
  => ComparisonOp
  -> TermOf m a
  -> TermOf m a
  -> m (S Bool)
evalComparisonOp op xT yT = do
  x <- eval xT
  y <- eval yT
  pure $ sansProv $ case op of
    Gt  -> x .> y
    Lt  -> x .< y
    Gte -> x .>= y
    Lte -> x .<= y
    Eq  -> x .== y
    Neq -> x ./= y

evalEqNeq
  :: ( Analyzer m
     , a' ~ Concrete a
     , SymWord a', Show a')
  => EqNeq
  -> TermOf m a
  -> TermOf m a
  -> m (S Bool)
evalEqNeq op xT yT = do
  x <- eval xT
  y <- eval yT
  pure $ sansProv $ case op of
    Eq'  -> x .== y
    Neq' -> x ./= y

evalObjectEqNeq
  :: Analyzer m
  => EqNeq
  -> TermOf m 'TyObject
  -> TermOf m 'TyObject
  -> m (S Bool)
evalObjectEqNeq op xT yT = do
  x <- evalO xT
  y <- evalO yT
  pure $ sansProv $ case op of
    Eq'  -> x .== y
    Neq' -> x ./= y

evalLogicalOp
  :: Analyzer m
  => LogicalOp
  -> [TermOf m 'TyBool]
  -> m (S Bool)
evalLogicalOp AndOp [a, b] = do
  a' <- eval a
  ite (_sSbv a') (eval b) (pure false)
evalLogicalOp OrOp [a, b] = do
  a' <- eval a
  ite (_sSbv a') (pure true) (eval b)
evalLogicalOp NotOp [a] = bnot <$> eval a
evalLogicalOp op terms = throwErrorNoLoc $ MalformedLogicalOpExec op $ length terms

evalCore
  :: ( Analyzer m
     , a' ~ Concrete a
     , SymWord a', Show (Core (TermOf m) a))
  => Core (TermOf m) a -> m (S a')
evalCore (Lit a)                           = pure (literalS a)
evalCore (Sym s)                           = pure s
evalCore (StrConcat p1 p2)                 = (.++) <$> eval p1 <*> eval p2
evalCore (StrLength p)
  = over s2Sbv SBVS.length . coerceS @Str @String <$> eval p
evalCore (StrToInt s)                      = evalStrToInt s
evalCore (StrToIntBase b s)                = evalStrToIntBase b s
evalCore (Numerical a)                     = evalNumerical a
evalCore (IntAddTime time secs)            = evalIntAddTime time secs
evalCore (DecAddTime time secs)            = evalDecAddTime time secs
evalCore (IntegerComparison op x y)        = evalComparisonOp op x y
evalCore (DecimalComparison op x y)        = evalComparisonOp op x y
evalCore (TimeComparison op x y)           = evalComparisonOp op x y
evalCore (StringComparison op x y)         = evalComparisonOp op x y
evalCore (BoolComparison op x y)           = evalComparisonOp op x y
evalCore (ObjectEqNeq op x y)              = evalObjectEqNeq  op x y
evalCore (KeySetEqNeq      op x y)         = evalEqNeq        op x y
evalCore (Logical op props)                = evalLogicalOp op props
evalCore (ObjAt schema colNameT objT retType)
  = evalObjAt schema colNameT objT retType
evalCore (ObjectMerge _ _)                 =
  error "object merge can not produce a simple value"
evalCore LiteralObject {}                  =
  error "literal object can't be an argument to evalCore"
evalCore (StringContains needle haystack) = do
  needle'   <- eval needle
  haystack' <- eval haystack
  pure $ sansProv $
    _sSbv (coerceS @Str @String needle')
    `SBVS.isInfixOf`
    _sSbv (coerceS @Str @String haystack')
evalCore (ListContains ty needle haystack) = withShow ty $ withSymWord ty $ do
  S _ needle'   <- eval needle
  S _ haystack' <- eval haystack
  pure $ sansProv $
    bfoldr listBound (\cell rest -> cell .== needle' ||| rest) false haystack'
evalCore (ListEqNeq ty op a b) = withEq ty $ withSymWord ty $ withShow ty $ do
  S _ a' <- eval a
  S _ b' <- eval b

  let wrongLength = case op of
        Eq'  -> false
        Neq' -> true
      zipF = case op of
        Eq'  -> (.==)
        Neq' -> (./=)

  pure $ ite (SBVL.length a' .== SBVL.length b')
    (sansProv $ band listBound $ bzipWith listBound zipF a' b')
    wrongLength
evalCore (ListAt tyA i l) = do
  S _ i' <- eval i
  S _ l' <- withShow tyA $ eval l

  -- valid range [0..length l - 1]
  markFailure $ i' .< 0 ||| i' .>= SBVL.length l'

  -- statically build a list of index comparisons
  pure $ sansProv $ SBVL.elemAt l' i'
evalCore (ListLength ty l) = withShow ty $ withSymWord ty $ do
  S prov l' <- withShow ty $ eval l
  pure $ S prov $ SBVL.length l'

evalCore (LiteralList (SList ty) xs) = withShow ty $ withSymWord ty $ do
  vals <- traverse (fmap _sSbv . eval) xs
  pure $ sansProv $ SBVL.implode vals

evalCore (ListDrop ty n list) = withShow ty $ withSymWord ty $ do
  S _ n'    <- eval n
  S _ list' <- eval list

  -- if the index is positive, count from the start of the list, otherwise
  -- count from the end.
  pure $ sansProv $ ite (n' .>= 0)
    (SBVL.drop n' list')
    (SBVL.take (SBVL.length list' + n') list')

evalCore (ListTake ty n list) = withShow ty $ withSymWord ty $ do
  S _ n'    <- eval n
  S _ list' <- eval list

  -- if the index is positive, count from the start of the list, otherwise
  -- count from the end.
  pure $ sansProv $ ite (n' .>= 0)
    (SBVL.take n' list')
    (SBVL.drop (SBVL.length list' + n') list')

evalCore (ListConcat ty p1 p2) = withShow ty $ withSymWord ty $ do
  S _ p1' <- eval p1
  S _ p2' <- eval p2
  pure $ sansProv $ SBVL.concat p1' p2'
evalCore (ListReverse ty l) = withShow ty $ withSymWord ty $ do
  S prov l' <- eval l
  pure $ S prov $ breverse listBound l'
evalCore (ListSort ty l) = withShow ty $ withSymWord ty $ do
  S prov l' <- eval l
  pure $ S prov $ bsort listBound l'

evalCore (Var vid name) = do
  mVal <- getVar vid
  case mVal of
    Nothing                -> throwErrorNoLoc $ VarNotInScope name vid
    Just (AVal mProv sval) -> pure $ mkS mProv sval
    Just (AnObj obj)       -> throwErrorNoLoc $ AValUnexpectedlyObj obj
    Just OpaqueVal         -> throwErrorNoLoc OpaqueValEncountered
evalCore x = error $ "no case for: " ++ show x

evalStrToInt :: Analyzer m => TermOf m 'TyStr -> m (S Integer)
evalStrToInt sT = do
  s' <- _sSbv <$> eval sT
  let s = coerceSBV @Str @String s'
  markFailure $ SBVS.null s
  markFailure $ SBVS.length s .> 128
  let nat = SBVS.strToNat s
  markFailure $ nat .< 0 -- will happen if empty or contains a non-digit
  pure $ sansProv nat

evalStrToIntBase
  :: (Analyzer m) => TermOf m 'TyInteger -> TermOf m 'TyStr -> m (S Integer)
evalStrToIntBase bT sT = do
  b <- eval bT
  s' <- eval sT
  let s = coerceS @Str @String s'

  markFailure $ SBVS.null $ _sSbv s
  markFailure $ SBVS.length (_sSbv s) .> 128

  case (unliteralS b, unliteralS s) of
    -- Symbolic base and string: give up; too many possible solutions.
    (Nothing, Nothing) ->
      throwErrorNoLoc "Unable to convert string to integer for symbolic base and string"

    -- Symbolic base and concrete string: pre-compute all 17 possible outcomes
    (Nothing, Just conStr) ->
      let conText = T.pack conStr
      in foldr
           (\conBase rest ->
             iteS (sansProv $ b .== literalS conBase)
               (precompute conBase conText)
               rest)
           symbolicFailure
           [2..16]

    -- Concrete base and symbolic string: only support base 10
    (Just conBase, Nothing)
      | conBase == 10 -> evalStrToInt $ inject' $ coerceS @String @Str s
      | otherwise     -> throwErrorNoLoc $ FailureMessage $ T.pack $ "Unable to statically determine the string for conversion to integer from base " ++ show conBase

    -- Concrete base and string: use pact native impl
    (Just conBase, Just conStr) ->
      case Pact.baseStrToInt conBase (T.pack conStr) of
        Left err -> throwErrorNoLoc $
          FailureMessage $ "Failed to convert string to integer: " <> err
        Right res -> pure $ literalS res

  where
    symbolicFailure = do
      markFailure true
      pure (literalS 0)

    precompute base conText =
      case Pact.baseStrToInt base conText of
        Left _err -> symbolicFailure
        Right res -> pure (literalS res)

evalObjAt
  :: (Analyzer m, SymWord a)
  => Schema
  -> TermOf m 'TyStr
  -> TermOf m 'TyObject
  -> EType
  -> m (S a)
evalObjAt schema@(Schema schemaFields) colNameT objT retType = do
  obj@(Object fields) <- evalO objT

  -- Filter down to only fields which contain the type we're looking for
  let relevantFields
        = map fst
        $ filter (\(_name, ty) -> ty == retType)
        $ Map.toList schemaFields

  colName :: S Str <- eval colNameT

  firstName:relevantFields' <- case relevantFields of
    [] -> throwErrorNoLoc $ AtHasNoRelevantFields retType schema
    _  -> pure relevantFields

  let getObjVal fieldName = case Map.lookup fieldName fields of
        Nothing -> throwErrorNoLoc $ KeyNotPresent fieldName obj

        Just (_fieldType, AVal mProv sval) -> pure $ mkS mProv sval

        Just (fieldType, AnObj _subObj) -> throwErrorNoLoc $
          ObjFieldOfWrongType fieldName fieldType

        Just (_fieldType, OpaqueVal) -> throwErrorNoLoc OpaqueValEncountered

  firstVal <- getObjVal firstName

  -- Fold over each relevant field, building a sequence of `ite`s. We require
  -- at least one matching field, ie firstVal. At first glance, this should
  -- just be a `foldr1M`, but we want the type of accumulator and element to
  -- differ, because elements are `String` `fieldName`s, while the accumulator
  -- is an `SBV a`.
  foldrM
    (\fieldName rest -> do
      val <- getObjVal fieldName
      pure $ ite (colName .== literalS (Str (T.unpack fieldName))) val rest
    )
    firstVal
    relevantFields'

evalObjAtO
  :: forall m
   . Analyzer m
  => TermOf m 'TyStr
  -> TermOf m 'TyObject
  -> m Object
evalObjAtO colNameT objT = do
    obj@(Object fields) <- evalO objT
    sCn <- eval colNameT

    let getObjVal :: Text -> m Object
        getObjVal fieldName = case Map.lookup fieldName fields of
          Nothing -> throwErrorNoLoc $ KeyNotPresent fieldName obj
          Just (fieldType, AVal _ _) -> throwErrorNoLoc $
            ObjFieldOfWrongType fieldName fieldType
          Just (_fieldType, AnObj subObj) -> pure subObj
          Just (_fieldType, OpaqueVal) -> throwErrorNoLoc OpaqueValEncountered

    case unliteralS (coerceS @Str @String sCn) of
      Nothing -> throwErrorNoLoc "Unable to determine statically the key used in an object access evaluating to an object (this is an object in an object)"
      Just concreteColName -> getObjVal (T.pack concreteColName)

evalCoreO
  :: Analyzer m
  => Core (TermOf m) 'TyObject -> m Object
evalCoreO (LiteralObject obj) = Object <$> traverse evalExistential obj
evalCoreO (ObjAt _schema colNameT objT _retType) = evalObjAtO colNameT objT
evalCoreO (ObjectMerge objT1 objT2) = mappend <$> evalO objT1 <*> evalO objT2
evalCoreO (Var vid name) = do
  mVal <- getVar vid
  case mVal of
    Nothing            -> throwErrorNoLoc $ VarNotInScope name vid
    Just (AVal _ val') -> throwErrorNoLoc $ AValUnexpectedlySVal val'
    Just (AnObj obj)   -> pure obj
    Just OpaqueVal     -> throwErrorNoLoc OpaqueValEncountered

-- TODO(joel): I don't think an object can appear hear. Get more clarity on
-- this.
evalCoreO (Lit obj)     = pure obj
evalCoreO (Sym _)       = vacuousMatch "an object cannot be a symbolic value"
evalCoreO (Numerical _) = vacuousMatch "an object cannot be a numerical value"
evalCoreO ListAt{} = throwErrorNoLoc "not yet implemented"
evalCoreO ObjTake{} = throwErrorNoLoc "not yet implemented"
evalCoreO ObjDrop{} = throwErrorNoLoc "not yet implemented"

-- evalCoreO (ObjDrop schema@(Schema schemaFields) keys _obj) = do
--   keys' <- eval keys
--   case unliteralS keys' of
--     Nothing -> throwErrorNoLoc "Unable to statically determine keys to drop"
--     Just literalKeys -> do
--       fields <- for literalKeys $ \(Str key) -> do
--         let retType = schemaFields ^?! ix (T.pack key)
--         val <- withSymWord singTy $
--           evalObjAt schema (Lit' (Str key)) obj retType
--         pure (T.pack key, (retType, mkAVal val))
--       pure $ Object $ Map.fromList fields


evalExistential :: Analyzer m => Existential (TermOf m) -> m (EType, AVal)
evalExistential = \case
  ESimple ty prop -> withShow ty $ withSymWord ty $ do
    prop' <- eval prop
    pure (EType ty, mkAVal prop')
  EList (SList ty) prop -> withShow ty $ withSymWord ty $ do
    vals  <- eval prop
    pure (EType ty, mkAVal vals)
  EObject ty prop -> do
    prop' <- evalO prop
    pure (EObjectTy ty, AnObj prop')
