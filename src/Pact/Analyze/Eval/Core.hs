{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pact.Analyze.Eval.Core where

import           Control.Lens                (over)
import           Data.Foldable               (foldrM)
import qualified Data.Map.Strict             as Map
import           Data.Monoid                 ((<>))
import           Data.SBV                    (Boolean (bnot, false, true),
                                              EqSymbolic ((./=), (.==)),
                                              OrdSymbolic ((.<), (.<=), (.>), (.>=)),
                                              SymWord, ite)
import qualified Data.SBV.String             as SBV
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Numerical
import           Pact.Analyze.Types
import           Pact.Analyze.Types.Eval
import           Pact.Analyze.Util
import qualified Pact.Native                 as Pact


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
  => TermOf m Time
  -> TermOf m Integer
  -> m (S Time)
evalIntAddTime timeT secsT = do
  time <- eval timeT
  secs <- eval secsT
  -- Convert seconds to milliseconds /before/ conversion to Integer (see note
  -- [Time Representation]).
  pure $ time + fromIntegralS (secs * 1000000)

evalDecAddTime
  :: Analyzer m
  => TermOf m Time
  -> TermOf m Decimal
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
  :: (Analyzer m, SymWord a, Show a)
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
  :: (Analyzer m, SymWord a, Show a)
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
  -> TermOf m Object
  -> TermOf m Object
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
  -> [TermOf m Bool]
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
  :: (Analyzer m, SymWord a)
  => Core (TermOf m) a -> m (S a)
evalCore (Lit a)                           = pure (literalS a)
evalCore (Sym s)                           = pure s
evalCore (StrConcat p1 p2)                 = (.++) <$> eval p1 <*> eval p2
evalCore (StrLength p)                     = over s2Sbv SBV.length <$> eval p
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
evalCore (At schema colNameT objT retType) = evalAt schema colNameT objT retType
evalCore (ObjectMerge _ _)                 =
  error "object merge can not produce a simple value"
evalCore LiteralObject {}                  =
  error "literal object can't be an argument to evalCore"
evalCore (Var vid name) = do
  mVal <- getVar vid
  case mVal of
    Nothing                -> throwErrorNoLoc $ VarNotInScope name vid
    Just (AVal mProv sval) -> pure $ mkS mProv sval
    Just (AnObj obj)       -> throwErrorNoLoc $ AValUnexpectedlyObj obj
    Just OpaqueVal         -> throwErrorNoLoc OpaqueValEncountered

evalStrToInt :: Analyzer m => TermOf m String -> m (S Integer)
evalStrToInt sT = do
  s <- _sSbv <$> eval sT
  markFailure $ SBV.null s
  markFailure $ SBV.length s .> 128
  let nat = SBV.strToNat s
  markFailure $ nat .< 0 -- will happen if empty or contains a non-digit
  pure $ sansProv nat

evalStrToIntBase :: (Analyzer m) => TermOf m Integer -> TermOf m String -> m (S Integer)
evalStrToIntBase bT sT = do
  b <- eval bT
  s <- eval sT

  markFailure $ SBV.null $ _sSbv s
  markFailure $ SBV.length (_sSbv s) .> 128

  case (unliteralS b, unliteralS s) of
    -- Symbolic base and string: give up; too many possible solutions.
    (Nothing, Nothing) ->
      throwErrorNoLoc "Unable to convert string to integer for symbolic base and string"

    -- Symbolic base and concrete string: pre-compute all 17 possible outcomes
    (Nothing, Just conStr) ->
      let conText = T.pack conStr
      in iteS (sansProv $ b .== 1) (precompute 1 conText) $
           iteS (sansProv $ b .== 2) (precompute 2 conText) $
           iteS (sansProv $ b .== 3) (precompute 3 conText) $
           iteS (sansProv $ b .== 4) (precompute 4 conText) $
           iteS (sansProv $ b .== 5) (precompute 5 conText) $
           iteS (sansProv $ b .== 6) (precompute 6 conText) $
           iteS (sansProv $ b .== 7) (precompute 7 conText) $
           iteS (sansProv $ b .== 8) (precompute 8 conText) $
           iteS (sansProv $ b .== 9) (precompute 9 conText) $
           iteS (sansProv $ b .== 10) (precompute 10 conText) $
           iteS (sansProv $ b .== 11) (precompute 11 conText) $
           iteS (sansProv $ b .== 12) (precompute 12 conText) $
           iteS (sansProv $ b .== 13) (precompute 13 conText) $
           iteS (sansProv $ b .== 14) (precompute 14 conText) $
           iteS (sansProv $ b .== 15) (precompute 15 conText) $
           iteS (sansProv $ b .== 16) (precompute 16 conText) $
           symbolicFailure

    -- Concrete base and symbolic string: only support base 10
    (Just conBase, Nothing)
      | conBase == 10 -> evalStrToInt $ inject s
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

evalAt
  :: (Analyzer m, SymWord a)
  => Schema
  -> TermOf m String
  -> TermOf m Object
  -> EType
  -> m (S a)
evalAt schema@(Schema schemaFields) colNameT objT retType = do
  obj@(Object fields) <- evalO objT

  -- Filter down to only fields which contain the type we're looking for
  let relevantFields
        = map fst
        $ filter (\(_name, ty) -> ty == retType)
        $ Map.toList schemaFields

  colName :: S String <- eval colNameT

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
      pure $ ite (colName .== literalS (T.unpack fieldName)) val rest
    )
    firstVal
    relevantFields'

evalAtO
  :: forall m
   . Analyzer m
  => TermOf m String
  -> TermOf m Object
  -> m Object
evalAtO colNameT objT = do
    obj@(Object fields) <- evalO objT
    sCn <- eval colNameT

    let getObjVal :: Text -> m Object
        getObjVal fieldName = case Map.lookup fieldName fields of
          Nothing -> throwErrorNoLoc $ KeyNotPresent fieldName obj
          Just (fieldType, AVal _ _) -> throwErrorNoLoc $
            ObjFieldOfWrongType fieldName fieldType
          Just (_fieldType, AnObj subObj) -> pure subObj
          Just (_fieldType, OpaqueVal) -> throwErrorNoLoc OpaqueValEncountered

    case unliteralS sCn of
      Nothing -> throwErrorNoLoc "Unable to determine statically the key used in an object access evaluating to an object (this is an object in an object)"
      Just concreteColName -> getObjVal (T.pack concreteColName)

evalCoreO
  :: Analyzer m
  => Core (TermOf m) Object -> m Object
evalCoreO (LiteralObject obj) = Object <$> traverse evalExistential obj
evalCoreO (At _schema colNameT objT _retType) = evalAtO colNameT objT
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


evalExistential :: Analyzer m => Existential (TermOf m) -> m (EType, AVal)
evalExistential = \case
  ESimple ty prop -> do
    prop' <- eval prop
    pure (EType ty, mkAVal prop')
  EObject ty prop -> do
    prop' <- evalO prop
    pure (EObjectTy ty, AnObj prop')
