{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Pact.Analyze.Parse
  ( expToCheck
  , expToProp
  , expToInvariant
  , TableEnv
  ) where

import           Control.Applicative          (Alternative, (<|>))
import           Control.Lens                 (at, ix, makeLenses, view,
                                               (^.), (^..), (&), (?~), (%~))
import           Control.Monad                (when)
import           Control.Monad.Except         (MonadError(throwError))
import           Control.Monad.Reader         (ReaderT, ask, local, runReaderT, asks)
import           Control.Monad.State.Strict   (StateT, evalStateT)
import           Data.Foldable                (asum, find)
import           Data.Maybe                   (isJust)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Semigroup               ((<>))
import qualified Data.Set                     as Set
import           Data.Set                     (Set)
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Type.Equality           ((:~:)(Refl))
import           Prelude                      hiding (exp)

import           Pact.Types.Lang              hiding (KeySet, KeySetName,
                                               SchemaVar, TKeySet, TableName,
                                               Type)
import qualified Pact.Types.Lang              as Pact
import           Pact.Types.Typecheck         (UserType)
import           Pact.Types.Util              (tShow)

import           Pact.Analyze.PrenexNormalize
import           Pact.Analyze.Translate
import           Pact.Analyze.Types

throwErrorT :: MonadError String m => Text -> m a
throwErrorT = throwError . T.unpack

-- TODO(joel): add location info
throwErrorIn :: (MonadError String m, UserShow a) => a -> Text -> m b
throwErrorIn exp text = throwError $ T.unpack $
  "in " <> userShow exp <> ", " <> text

-- | Just 'asum' with a fallback
asum' :: (Foldable t, Alternative f) => t (f a) -> f a -> f a
asum' foldable fallback = asum foldable <|> fallback

textToArithOp :: Text -> Maybe ArithOp
textToArithOp = \case
  "+"   -> Just Add
  "-"   -> Just Sub
  "*"   -> Just Mul
  "/"   -> Just Div
  "^"   -> Just Pow
  "log" -> Just Log
  _     -> Nothing

textToUnaryArithOp :: Text -> Maybe UnaryArithOp
textToUnaryArithOp = \case
  "-"    -> Just Negate
  "sqrt" -> Just Sqrt
  "ln"   -> Just Ln
  "exp"  -> Just Exp
  "abs"  -> Just Abs
  -- explicitly no signum
  _      -> Nothing

textToComparisonOp :: Text -> Maybe ComparisonOp
textToComparisonOp = \case
  ">"  -> Just Gt
  "<"  -> Just Lt
  ">=" -> Just Gte
  "<=" -> Just Lte
  "="  -> Just Eq
  "!=" -> Just Neq
  _    -> Nothing

textToEqNeq :: Text -> Maybe EqNeq
textToEqNeq = \case
  "="  -> Just Eq'
  "!=" -> Just Neq'
  _    -> Nothing

textToRoundingLikeOp :: Text -> Maybe RoundingLikeOp
textToRoundingLikeOp = \case
  "round"   -> Just Round
  "ceiling" -> Just Ceiling
  "floor"   -> Just Floor
  _         -> Nothing

textToLogicalOp :: Text -> Maybe LogicalOp
textToLogicalOp = \case
  "and" -> Just AndOp
  "or"  -> Just OrOp
  "not" -> Just NotOp
  _     -> Nothing

textToQuantifier
  :: Text -> Maybe (VarId -> Text -> QType -> PreProp -> PreProp)
textToQuantifier = \case
  "forall" -> Just PreForall
  "exists" -> Just PreExists
  _        -> Nothing

stringLike :: Exp -> Maybe Text
stringLike = \case
  ESymbol str _  -> Just str
  ELitString str -> Just str
  _              -> Nothing

toQ :: PreProp -> Maybe
  ( VarId -> Text -> QType -> Prop Bool -> Prop Bool
  , VarId
  , Text
  , QType
  , PreProp
  )
toQ = \case
  PreForall vid name ty' p -> Just (Forall, vid, name, ty', p)
  PreExists vid name ty' p -> Just (Exists, vid, name, ty', p)
  _ -> Nothing

type TableEnv = TableMap (ColumnMap EType)

data PropCheckEnv = PropCheckEnv
  { _varTys            :: Map VarId QType
  , _tableEnv          :: TableEnv
  , _quantifiedTables  :: Set TableName
  -- , _quantifiedColumns :: Set ColumnName
  }

type PropParse = ReaderT (Map Text VarId) (StateT VarId (Either String))
type PropCheck = ReaderT PropCheckEnv (Either String)

type InvariantParse = ReaderT [Pact.Arg UserType] (Either String)

makeLenses ''PropCheckEnv

parseTableName :: PreProp -> PropCheck (Prop TableName)
parseTableName (PreStringLit str) = pure (fromString (T.unpack str))
parseTableName (PreVar vid name) = do
  varTy <- view (varTys . at vid)
  case varTy of
    Just QTable -> pure (fromString (T.unpack name))
    _           -> throwError $ T.unpack $
      "invalid table name: " <> name
parseTableName bad = throwError $ T.unpack $
  "invalid table name: " <> userShow bad

parseColumnName :: PreProp -> PropCheck (Prop ColumnName)
parseColumnName (PreStringLit str) = pure (fromString (T.unpack str))
parseColumnName bad = throwError $ T.unpack $
  "invalid table name: " <> userShow bad

-- The conversion from @Exp@ to @PreProp@
--
--
-- The biggest thing it handles is generating unique ids for variables and
-- binding them.
--
-- We also handle literals and disambiguating identifiers.
--
-- One thing which is not done yet is the conversion from @Text@ to @ArithOp@,
-- @ComparisonOp@, etc. We handle this in @checkPreProp@ as it doesn't cause
-- any difficulty there and is less burdensome than creating a new data type
-- for these operators.
expToPreProp :: Exp -> PropParse PreProp
expToPreProp = \case
  ELiteral (LDecimal d) _ -> pure (PreDecimalLit (mkDecimal d))
  ELiteral (LInteger i) _ -> pure (PreIntegerLit i)
  (stringLike -> Just s)  -> pure (PreStringLit s)
  ELiteral (LTime t) _    -> pure (PreTimeLit (mkTime t))
  ELiteral (LBool b) _    -> pure (PreBoolLit b)

  EList' [EAtom' (textToQuantifier -> Just q), EList' bindings, body] -> do
    bindings' <- propBindings bindings
    let theseBindingsMap = Map.fromList $
          fmap (\(vid, name, _ty) -> (name, vid)) bindings'
    body'     <- local (Map.union theseBindingsMap) (expToPreProp body)
    pure $ foldr
      (\(vid, name, ty) accum -> q vid name ty accum)
      body'
      bindings'

  EList' (EAtom' funName:args) -> PreApp funName <$> traverse expToPreProp args

  EAtom' "abort"   -> pure PreAbort
  EAtom' "success" -> pure PreSuccess
  EAtom' "result"  -> pure PreResult
  EAtom' var       -> mkVar var

  exp -> throwErrorIn exp $ "expected property"

  where propBindings :: [Exp] -> PropParse [(VarId, Text, QType)]
        propBindings [] = pure []
        -- we require a type annotation
        propBindings (exp@(EAtom _name _qual Nothing _parsed):_exps)
          = throwErrorIn exp $
            "type annotation required for all property bindings."
        propBindings (exp@(EAtom name _qual (Just ty) _parsed):exps) = do
          -- XXX handle table / columns
          nameTy <- case maybeTranslateType' (const Nothing) ty of
            Just ty' -> do
              vid <- genVarId
              pure (vid, name, ty')
            -- This is challenging because `ty : Pact.Type TypeName`, but
            -- `maybeTranslateType` handles `Pact.Type UserType`.
            Nothing -> throwErrorIn exp
              "currently objects can't be quantified in properties (issue 139)"
          (nameTy:) <$> propBindings exps
        propBindings exp = throwErrorT $
          "in " <> userShowList exp <> ", unexpected binding form"

        mkVar :: Text -> PropParse PreProp
        mkVar var = do
          mVid <- view (at var)
          case mVid of
            Nothing  -> throwErrorT $
              "couldn't find property variable " <> var
            Just vid -> pure (PreVar vid var)

checkPreProp :: Type a -> PreProp -> PropCheck (Prop a)
checkPreProp ty preProp = case (ty, preProp) of
  -- literals
  (TDecimal, PreDecimalLit a) -> pure (PLit a)
  (TInt, PreIntegerLit a)     -> pure (PLit a)
  (TStr, PreStringLit a)      -> pure (PLit (T.unpack a))
  (TTime, PreTimeLit a)       -> pure (PLit a)
  (TBool, PreBoolLit a)       -> pure (PLit a)

  -- identifiers
  (TBool, PreAbort)    -> pure Abort
  (TBool, PreSuccess)  -> pure Success
  (_, PreResult)       -> pure Result
  (_, PreVar vid name) -> do
    varTy <- view (varTys . at vid)
    case varTy of
      Nothing -> throwErrorT $
        "couldn't find property variable " <> name
      Just (EType varTy') -> case typeEq ty varTy' of
        Nothing   -> throwErrorT $ "property type mismatch: " <> name <>
          " has type " <> userShow varTy' <> ", but " <> userShow ty <>
          " was expected"
        Just Refl -> pure (PVar vid name)
      Just (EObjectTy _) -> throwErrorIn preProp
        "ERROR: object types not currently allowed in properties (issue 139)"
      Just QTable        -> error "TODO"
      Just (QColumnOf _) -> error "TODO"

  -- quantifiers
  (TBool, toQ -> Just (q, vid, name, ty', p)) -> do
    let quantifyTable = case ty' of
          QTable -> Set.insert (TableName (T.unpack name))
          _      -> id
    let modEnv env = env & varTys . at vid  ?~ ty'
                         & quantifiedTables %~ quantifyTable

    q vid name ty' <$> local modEnv (checkPreProp TBool p)

  -- TODO: PreAt / PAt

  -- applications
  (TInt, PreApp "str-length" [str]) -> PStrLength <$> checkPreProp TStr str
  (TStr, PreApp "+" [a, b])
    -> PStrConcat <$> checkPreProp TStr a <*> checkPreProp TStr b

  (TDecimal, PreApp (textToArithOp -> Just op) [a, b]) -> asum'
    [ PDecArithOp    op <$> checkPreProp TDecimal a <*> checkPreProp TDecimal b
    , PDecIntArithOp op <$> checkPreProp TDecimal a <*> checkPreProp TInt b
    , PIntDecArithOp op <$> checkPreProp TInt a     <*> checkPreProp TDecimal b
    ] (throwErrorIn preProp $ "expected decimal, found " <> userShow preProp)
  (TInt, PreApp (textToArithOp -> Just op) [a, b])
    -> PIntArithOp op <$> checkPreProp TInt a <*> checkPreProp TInt b
  (TDecimal, PreApp (textToUnaryArithOp -> Just op) [a])
    -> PDecUnaryArithOp op <$> checkPreProp TDecimal a
  (TInt, PreApp (textToUnaryArithOp -> Just op) [a])
    -> PIntUnaryArithOp op <$> checkPreProp TInt a

  (TInt, PreApp "mod" [a, b])
    -> PModOp <$> checkPreProp TInt a <*> checkPreProp TInt b
  (TInt, PreApp (textToRoundingLikeOp -> Just op) [a])
    -> PRoundingLikeOp1 op <$> checkPreProp TDecimal a
  (TDecimal, PreApp (textToRoundingLikeOp -> Just op) [a, b])
    -> PRoundingLikeOp2 op <$> checkPreProp TDecimal a <*> checkPreProp TInt b
  (TTime, PreApp "add-time" [a, b]) -> do
    a' <- checkPreProp TTime a
    asum'
      [ PIntAddTime a' <$> checkPreProp TInt b
      , PDecAddTime a' <$> checkPreProp TDecimal b
      ] (throwErrorIn preProp "invalid argument types")

  (TBool, PreApp op'@(textToComparisonOp -> Just op) [a, b]) -> asum'
    [ PIntegerComparison op <$> checkPreProp TInt a     <*> checkPreProp TInt b
    , PDecimalComparison op <$> checkPreProp TDecimal a <*> checkPreProp TDecimal b
    , PTimeComparison    op <$> checkPreProp TTime a    <*> checkPreProp TTime b
    , PBoolComparison    op <$> checkPreProp TBool a    <*> checkPreProp TBool b
    , PStringComparison  op <$> checkPreProp TStr a     <*> checkPreProp TStr b
    , case textToEqNeq op' of
      Just eqNeq -> PKeySetEqNeq eqNeq
        <$> checkPreProp TKeySet a
        <*> checkPreProp TKeySet b
      Nothing -> throwErrorIn preProp $
        "found " <> op' <> " where = or != was expected"
    ] (throwErrorIn preProp $ "expected bool, but found " <> userShow preProp)

  (TBool, PreApp op'@(textToLogicalOp -> Just op) args) -> case (op, args) of
    (NotOp, [a])    -> PNot <$> checkPreProp TBool a
    (AndOp, [a, b]) -> PAnd <$> checkPreProp TBool a <*> checkPreProp TBool b
    (OrOp, [a, b])  -> POr  <$> checkPreProp TBool a <*> checkPreProp TBool b
    _               -> throwErrorIn preProp $
      op' <> " applied to wrong number of arguments"

  (TBool, PreApp "when" [a, b]) -> do
    propNotA <- PNot <$> checkPreProp TBool a
    POr propNotA <$> checkPreProp TBool b

  --
  -- TODO: should be "table-written"
  --
  (TBool, PreApp "table-write" [tn]) -> do
    tn' <- parseTableName tn
    expectTableExists tn'
    pure (TableWrite tn')
  (TBool, PreApp "table-read" [tn]) -> do
    tn' <- parseTableName tn
    expectTableExists tn'
    pure (TableRead tn')
  --
  -- NOTE: disabled until implemented on the backend:
  --
  -- (TBool, PreApp "column-written" [PLit tn, PLit cn])
  --   -> pure (ColumnWrite tn cn)
  -- (TBool, PreApp "column-read" [PLit tn, PLit cn])
  --   -> pure (ColumnRead tn cn)
  (TInt, PreApp "cell-delta" [tn, cn, rk]) -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    _   <- expectColumnType tn' cn' TInt
    IntCellDelta tn' cn' <$> checkPreProp TStr rk
  (TDecimal, PreApp "cell-delta" [tn, cn, rk]) -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    _   <- expectColumnType tn' cn' TDecimal
    DecCellDelta tn' cn' <$> checkPreProp TStr rk
  (TInt, PreApp "column-delta" [tn, cn]) -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    _   <- expectColumnType tn' cn' TInt
    -- TODO: if these weren't *Int*ColumnDelta / *Dec*ColumnDelta these clauses
    -- could be collapsed
    pure (IntColumnDelta tn' cn')
  (TDecimal, PreApp "column-delta" [tn, cn]) -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    _   <- expectColumnType tn' cn' TDecimal
    pure (DecColumnDelta tn' cn')
  (TBool, PreApp "row-read" [tn, rk]) -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    RowRead tn' <$> checkPreProp TStr rk
  (TInt, PreApp "row-read-count" [tn, rk]) -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    RowReadCount tn' <$> checkPreProp TStr rk
  --
  -- TODO: should be "row-written"
  --
  (TBool, PreApp "row-write" [tn, rk]) -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    RowWrite tn' <$> checkPreProp TStr rk
  (TInt, PreApp "row-write-count" [tn, rk]) -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    RowWriteCount tn' <$> checkPreProp TStr rk
  (TBool, PreApp "authorized-by" [PreStringLit ks])
    -> pure (KsNameAuthorized (KeySetName ks))
  (TBool, PreApp "row-enforced" [tn, cn, rk]) -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    _   <- expectColumnType tn' cn' TKeySet
    RowEnforced tn' cn' <$> checkPreProp TStr rk

  _ -> throwErrorIn preProp $ "type error: expected type " <> userShow ty

expectColumnType
  :: Prop TableName -> Prop ColumnName -> Type a -> PropCheck ()
expectColumnType (PLit tn) (PLit cn) expectedTy = do
  tys <- asks (^.. tableEnv . ix tn . ix cn)
  case tys of
    [EType foundTy] -> case typeEq foundTy expectedTy of
      Nothing   -> throwErrorT $
        "expected column " <> userShow cn <> " in table " <> userShow tn <>
        " to have type " <> userShow expectedTy <> ", instead found " <>
        userShow foundTy
      Just Refl -> pure ()
    _ -> throwErrorT $
      "didn't find expected column " <> userShow cn <> " in table " <> userShow tn
expectColumnType _ _ _
  = error "table and column names must be concrete at this point"

expectTableExists :: Prop TableName -> PropCheck ()
expectTableExists (PLit tn) = do
  quantified <- view $ quantifiedTables . at tn
  defined    <- view $ tableEnv . at tn
  when (not (isJust quantified || isJust defined)) $
    throwErrorT $ "expected table " <> userShow tn <> " but it isn't in scope"
expectTableExists _ = error "table name must be concrete at this point"

-- Convert an @Exp@ to a @Check@ in an environment where the variables have
-- types.
--
-- TODO: the one property this can't parse yet is PAt because it includes an
-- EType.
--
expToCheck
  :: TableEnv
  -- ^ Tables and schemas in scope
  -> VarId
  -- ^ ID to start issuing from
  -> Map Text VarId
  -- ^ Environment mapping names to var IDs
  -> Map VarId EType
  -- ^ Environment mapping var IDs to their types
  -> Exp
  -- ^ Exp to convert
  -> Either String Check
expToCheck tableEnv' genStart nameEnv idEnv body = do
  preTypedBody <- evalStateT (runReaderT (expToPreProp body) nameEnv) genStart
  let env = PropCheckEnv (coerceQType <$> idEnv) tableEnv' Set.empty
  typedBody <- runReaderT (checkPreProp TBool preTypedBody) env
  pure $ PropertyHolds $ prenexConvert typedBody

expToProp
  :: TableEnv
  -- ^ Tables and schemas in scope
  -> VarId
  -- ^ ID to start issuing from
  -> Map Text VarId
  -- ^ Environment mapping names to var IDs
  -> Map VarId EType
  -- ^ Environment mapping var IDs to their types
  -> Type a
  -- ^ Expected prop type
  -> Exp
  -- ^ Exp to convert
  -> Either String (Prop a)
expToProp tableEnv' genStart nameEnv idEnv ty body = do
  preTypedBody <- evalStateT (runReaderT (expToPreProp body) nameEnv) genStart
  let env = PropCheckEnv (coerceQType <$> idEnv) tableEnv' Set.empty
  runReaderT (checkPreProp ty preTypedBody) env

expToInvariant :: Type a -> Exp -> InvariantParse (Invariant a)
expToInvariant ty exp = case (ty, exp) of
  (_, EAtom' var) -> do
    schemaTys <- ask
    case find (\arg -> arg ^. aName == var) schemaTys of
      Just (Pact.Arg _name (TyPrim primTy) _info) -> case (ty, primTy) of
        (TInt,     TyInteger) -> pure (IVar var)
        (TDecimal, TyDecimal) -> pure (IVar var)
        (TTime,    TyTime)    -> pure (IVar var)
        (TStr,     TyString)  -> pure (IVar var)
        (TBool,    TyBool)    -> pure (IVar var)
        (TKeySet,  TyKeySet)  -> pure (IVar var)
        (_,        TyValue)   -> throwErrorIn exp
          "Invariants can't constrain opaque values"
        (_,        _)         -> throwErrorIn exp $
          "found variable " <> var <> " of type " <> tShow primTy <>
          " where " <> userShow ty <> " was expected"
      _ -> throwErrorT $ "couldn't find column named " <> var

  (TDecimal, ELiteral (LDecimal d) _) -> pure (IDecimalLiteral (mkDecimal d))
  (TInt, ELiteral (LInteger i) _)     -> pure (IIntLiteral i)
  (TStr, stringLike -> Just s)        -> pure (IStringLiteral s)
  (TStr, ELiteral (LString _) _)      -> error "impossible (handled by stringLike)"
  (TTime, ELiteral (LTime t) _)       -> pure (ITimeLiteral (mkTime t))
  (TBool, ELiteral (LBool b) _)       -> pure (IBoolLiteral b)
  (_, ELiteral _ _)                   ->
    throwErrorIn exp "literal of unexpected type"

  (TInt, EList' [EAtom' "str-length", str]) -> IStrLength <$> expToInvariant TStr str
  (TStr, EList' [EAtom' "+", a, b])
    -> IStrConcat <$> expToInvariant TStr a <*> expToInvariant TStr b

  (TDecimal, EList' [EAtom' (textToArithOp -> Just op), a, b]) -> asum'
    [ IDecArithOp    op <$> expToInvariant TDecimal a <*> expToInvariant TDecimal b
    , IDecIntArithOp op <$> expToInvariant TDecimal a <*> expToInvariant TInt b
    , IIntDecArithOp op <$> expToInvariant TInt a     <*> expToInvariant TDecimal b
    ] (throwErrorIn exp "unexpected argument types")
  (TInt, EList' [EAtom' (textToArithOp -> Just op), a, b])
    -> IIntArithOp op <$> expToInvariant TInt a <*> expToInvariant TInt b
  (TDecimal, EList' [EAtom' (textToUnaryArithOp -> Just op), a])
    -> IDecUnaryArithOp op <$> expToInvariant TDecimal a
  (TInt, EList' [EAtom' (textToUnaryArithOp -> Just op), a])
    -> IIntUnaryArithOp op <$> expToInvariant TInt a

  (TBool, EList' [EAtom' op'@(textToComparisonOp -> Just op), a, b]) -> asum'
    [ IIntComparison op     <$> expToInvariant TInt a     <*> expToInvariant TInt b
    , IDecimalComparison op <$> expToInvariant TDecimal a <*> expToInvariant TDecimal b
    , ITimeComparison op    <$> expToInvariant TTime a    <*> expToInvariant TTime b
    , IBoolComparison op    <$> expToInvariant TBool a    <*> expToInvariant TBool b
    , IStringComparison op  <$> expToInvariant TStr a     <*> expToInvariant TStr b
    , case textToEqNeq op' of
      Just eqNeq -> IKeySetEqNeq eqNeq
        <$> expToInvariant TKeySet a
        <*> expToInvariant TKeySet b
      Nothing -> throwErrorIn exp $
        op' <> " is an invalid operation for keysets (only = or /= allowed)"
    ] (throwErrorIn exp "unexpected argument types")

  (TBool, EList' (EAtom' op:args))
    | Just op' <- textToLogicalOp op -> do
    operands' <- traverse (expToInvariant TBool) args
    case (op', operands') of
      (AndOp, [a, b]) -> pure (ILogicalOp AndOp [a, b])
      (OrOp, [a, b])  -> pure (ILogicalOp OrOp [a, b])
      (NotOp, [a])    -> pure (ILogicalOp NotOp [a])
      _ -> throwErrorIn exp $ "logical op with wrong number of args: " <> op

  (_, ESymbol {})  -> throwErrorIn exp $ "illegal invariant form"
  (_, EAtom {})    -> throwErrorIn exp $ "illegal invariant form"
  (_, EList {})    -> throwErrorIn exp $ "illegal invariant form"
  (_, EObject {})  -> throwErrorIn exp $ "illegal invariant form"
  (_, EBinding {}) -> throwErrorIn exp $ "illegal invariant form"
