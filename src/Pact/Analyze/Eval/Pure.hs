{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pact.Analyze.Eval.Pure where

import           Control.Lens                (over)
import           Data.Foldable               (foldrM)
import qualified Data.Map.Strict             as Map
import           Data.SBV                    (Boolean (bnot, (&&&), (|||)),
                                              EqSymbolic ((./=), (.==)),
                                              OrdSymbolic ((.<), (.<=), (.>), (.>=)),
                                              SymWord, ite)
import qualified Data.SBV.String             as SBV
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Pact.Analyze.Errors
import           Pact.Analyze.Eval.Numerical
import           Pact.Analyze.Eval.Types
import           Pact.Analyze.Types          hiding (tableName)
import           Pact.Analyze.Util


-- Note [Time Representation]
--
-- Pact uses the Thyme library (UTCTime) to represent times. Thyme internally
-- uses a 64-bit count of microseconds since the MJD epoch. So, our symbolic
-- representation is naturally a 64-bit integer.
--
-- The effect from a Pact-user's point of view is that we stores 6 digits to
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
  then pure $ time + fromIntegralS (banker'sMethod (secs * 1000000))
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

evalLogicalOp
  :: (Analyzer m, Boolean (S a), Show a, SymWord a)
  => LogicalOp
  -> [TermOf m a]
  -> m (S a)
evalLogicalOp op terms = do
  symBools <- traverse eval terms
  case (op, symBools) of
    (AndOp, [a, b]) -> pure $ a &&& b
    (OrOp,  [a, b]) -> pure $ a ||| b
    (NotOp, [a])    -> pure $ bnot a
    _               -> throwErrorNoLoc $ MalformedLogicalOpExec op $ length terms

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

evalPure
  :: (Analyzer m, SymWord a)
  => PureTerm (TermOf m) a -> m (S a)
evalPure (Lit a)                    = pure (literalS a)
evalPure (Sym s)                    = pure s
evalPure (StrConcat p1 p2)          = (.++) <$> eval p1 <*> eval p2
evalPure (StrLength p)              = over s2Sbv SBV.length <$> eval p
evalPure (Numerical a)              = evalNumerical a
evalPure (IntAddTime time secs)     = evalIntAddTime time secs
evalPure (DecAddTime time secs)     = evalDecAddTime time secs
evalPure (IntegerComparison op x y) = evalComparisonOp op x y
evalPure (DecimalComparison op x y) = evalComparisonOp op x y
evalPure (TimeComparison op x y)    = evalComparisonOp op x y
evalPure (StringComparison op x y)  = evalComparisonOp op x y
evalPure (BoolComparison op x y)    = evalComparisonOp op x y
evalPure (ObjectEqNeq op x y)       = evalObjectEqNeq  op x y
evalPure (KeySetEqNeq      op x y)  = evalEqNeq        op x y
evalPure (Logical op props)         = evalLogicalOp op props
evalPure (At schema colNameT objT retType)
  = evalAt schema colNameT objT retType
evalPure LiteralObject {}
  = error "literal object can't be an argument to evalPure"
evalPure (Var vid name) = do
  mVal <- getVar vid
  case mVal of
    Nothing                -> throwErrorNoLoc $ VarNotInScope name vid
    Just (AVal mProv sval) -> pure $ mkS mProv sval
    Just (AnObj obj)       -> throwErrorNoLoc $ AValUnexpectedlyObj obj
    Just (OpaqueVal)       -> throwErrorNoLoc OpaqueValEncountered

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

evalPureO
  :: Analyzer m
  => PureTerm (TermOf m) Object -> m Object
evalPureO (LiteralObject obj) = Object <$> traverse evalExistential obj
evalPureO (At _schema colNameT objT _retType) = evalAtO colNameT objT
evalPureO (Var vid name) = do
  mVal <- getVar vid
  case mVal of
    Nothing            -> throwErrorNoLoc $ VarNotInScope name vid
    Just (AVal _ val') -> throwErrorNoLoc $ AValUnexpectedlySVal val'
    Just (AnObj obj)   -> pure obj
    Just (OpaqueVal)   -> throwErrorNoLoc OpaqueValEncountered

-- TODO(joel): I don't think an object can appear hear. Get more clarity on
-- this.
evalPureO (Lit obj)     = pure obj
evalPureO (Sym _)       = vacuousMatch "an object cannot be a symbolic value"
evalPureO (Numerical _) = vacuousMatch "an object cannot be a numerical value"


evalExistential :: Analyzer m => Existential (TermOf m) -> m (EType, AVal)
evalExistential = \case
  ETerm ty prop   -> do
    prop' <- eval prop
    pure (EType ty, mkAVal prop')
  EObject ty prop -> do
    prop' <- evalO prop
    pure (EObjectTy ty, AnObj prop')

