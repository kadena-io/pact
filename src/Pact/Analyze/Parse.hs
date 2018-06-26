{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Pact.Analyze.Parse
  ( expToCheck
  , expToProp
  , expToInvariant
  , TableEnv
  ) where

import           Control.Lens                 (_1, _2, at, ix, view, (^.), (^..), (&), (.~), (?~))
import           Control.Monad.Except         (throwError)
import           Control.Monad.Reader         (ReaderT, ask, local, runReaderT, asks)
import           Control.Monad.State.Strict   (StateT, evalStateT)
import           Data.Foldable                (asum, find)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Type.Equality           ((:~:)(Refl))
import           Prelude                      hiding (exp)

import           Pact.Types.Lang              hiding (KeySet, KeySetName,
                                               SchemaVar, TKeySet, TableName,
                                               Type)
import qualified Pact.Types.Lang              as Pact
import           Pact.Types.Typecheck         (UserType)

import           Pact.Analyze.PrenexNormalize
import           Pact.Analyze.Types

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
  :: Text -> Maybe (VarId -> Text -> Ty -> PreProp -> PreProp)
textToQuantifier = \case
  "forall" -> Just PreForall
  "exists" -> Just PreExists
  _        -> Nothing

stringLike :: Exp -> Maybe Text
stringLike = \case
  ESymbol str _  -> Just str
  ELitString str -> Just str
  _              -> Nothing

pattern TableLit :: TableName -> PreProp
pattern TableLit tn <- PreStringLit (TableName . T.unpack -> tn)

pattern ColumnLit :: ColumnName -> PreProp
pattern ColumnLit tn <- PreStringLit (ColumnName . T.unpack -> tn)

type TableEnv = TableMap (ColumnMap EType)

type PropParse = ReaderT (Map Text VarId) (StateT VarId (Either String))
type PropCheck = ReaderT (Map VarId EType, TableEnv) (Either String)

type InvariantParse = ReaderT [Pact.Arg UserType] (Either String)

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

  _ -> throwError "expected property"

  where propBindings :: [Exp] -> PropParse [(VarId, Text, Ty)]
        propBindings [] = pure []
        -- we require a type annotation
        propBindings (EAtom _name _qual Nothing _parsed:_exps) = throwError "type annotation required for all property bindings"
        propBindings (EAtom name _qual (Just ty) _parsed:exps) = do
          nameTy <- case ty of
            TyPrim TyString -> do
              vid <- genVarId
              pure (vid, name, Ty (Rep @String))
            _               -> throwError "currently only strings can be quantified in properties"
          (nameTy:) <$> propBindings exps
        propBindings _ = throwError "unexpected binding form"

        mkVar :: Text -> PropParse PreProp
        mkVar var = do
          mVid <- view (at var)
          case mVid of
            Nothing  -> throwError $ "variable not found: " ++ T.unpack var
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
    varTy <- view (_1 . at vid)
    case varTy of
      Nothing -> throwError $ "couldn't find property variable " ++ T.unpack name
      Just (EType varTy') -> case typeEq ty varTy' of
        Nothing   -> throwError "wrong type"
        Just Refl -> pure (PVar vid name)

  -- quantifiers
  (a, PreForall vid name ty' p) -> Forall vid name ty' <$>
    (local (& _1 . at vid ?~ EType ty') $ checkPreProp a p)
  (a, PreExists vid name ty' p) -> Exists vid name ty' <$>
    checkPreProp a p

  -- TODO: PreAt / PAt

  -- applications
  (TInt, PreApp "str-length" [str]) -> PStrLength <$> checkPreProp TStr str
  (TStr, PreApp "+" [a, b])
    -> PStrConcat <$> checkPreProp TStr a <*> checkPreProp TStr b

  (TDecimal, PreApp (textToArithOp -> Just op) [a, b]) -> asum
    [ PDecArithOp    op <$> checkPreProp TDecimal a <*> checkPreProp TDecimal b
    , PDecIntArithOp op <$> checkPreProp TDecimal a <*> checkPreProp TInt b
    , PIntDecArithOp op <$> checkPreProp TInt a     <*> checkPreProp TDecimal b
    ]
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
    asum
      [ PIntAddTime a' <$> checkPreProp TInt b
      , PDecAddTime a' <$> checkPreProp TDecimal b
      ]

  (TBool, PreApp op'@(textToComparisonOp -> Just op) [a, b]) -> asum
    [ PIntegerComparison op <$> checkPreProp TInt a     <*> checkPreProp TInt b
    , PDecimalComparison op <$> checkPreProp TDecimal a <*> checkPreProp TDecimal b
    , PTimeComparison    op <$> checkPreProp TTime a    <*> checkPreProp TTime b
    , PBoolComparison    op <$> checkPreProp TBool a    <*> checkPreProp TBool b
    , PStringComparison  op <$> checkPreProp TStr a     <*> checkPreProp TStr b
    , case textToEqNeq op' of
      Just eqNeq -> PKeySetEqNeq eqNeq
        <$> checkPreProp TKeySet a
        <*> checkPreProp TKeySet b
      Nothing -> throwError $ "found " ++ show op' ++ " where = or != expected"
    ]

  (TBool, PreApp op'@(textToLogicalOp -> Just op) args) -> case (op, args) of
    (NotOp, [a])    -> PNot <$> checkPreProp TBool a
    (AndOp, [a, b]) -> PAnd <$> checkPreProp TBool a <*> checkPreProp TBool b
    (OrOp, [a, b])  -> POr  <$> checkPreProp TBool a <*> checkPreProp TBool b
    _               -> throwError $ show op' ++ " applied to wrong number of arguments"

  (TBool, PreApp "when" [a, b]) -> do
    propNotA <- PNot <$> checkPreProp TBool a
    POr propNotA <$> checkPreProp TBool b

  --
  -- TODO: should be "table-written"
  --
  (TBool, PreApp "table-write" [TableLit tn]) -> do
    expectTableExists tn
    pure (TableWrite tn)
  (TBool, PreApp "table-read" [TableLit tn]) -> do
    expectTableExists tn
    pure (TableRead tn)
  --
  -- NOTE: disabled until implemented on the backend:
  --
  -- (TBool, PreApp "column-written" [TableLit tn, ColumnLit cn])
  --   -> pure (ColumnWrite tn cn)
  -- (TBool, PreApp "column-read" [TableLit tn, ColumnLit cn])
  --   -> pure (ColumnRead tn cn)
  (TInt, PreApp "cell-delta" [TableLit tn, ColumnLit cn, rk]) -> do
    _ <- expectTableExists tn
    _ <- expectColumnType tn cn TInt
    IntCellDelta tn cn <$> checkPreProp TStr rk
  (TDecimal, PreApp "cell-delta" [TableLit tn, ColumnLit cn, rk]) -> do
    _ <- expectTableExists tn
    _ <- expectColumnType tn cn TDecimal
    DecCellDelta tn cn <$> checkPreProp TStr rk
  (TInt, PreApp "column-delta" [TableLit tn, ColumnLit cn]) -> do
    _ <- expectTableExists tn
    _ <- expectColumnType tn cn TInt
    -- TODO: if these weren't *Int*ColumnDelta / *Dec*ColumnDelta these clauses
    -- could be collapsed
    pure (IntColumnDelta tn cn)
  (TDecimal, PreApp "column-delta" [TableLit tn, ColumnLit cn]) -> do
    _ <- expectTableExists tn
    _ <- expectColumnType tn cn TDecimal
    pure (DecColumnDelta tn cn)
  (TBool, PreApp "row-read" [TableLit tn, rk]) -> do
    _ <- expectTableExists tn
    RowRead tn <$> checkPreProp TStr rk
  --
  -- TODO: should be "row-written"
  --
  (TBool, PreApp "row-write" [TableLit tn, rk]) -> do
    _ <- expectTableExists tn
    RowWrite tn <$> checkPreProp TStr rk
  (TBool, PreApp "authorized-by" [PreStringLit ks])
    -> pure (KsNameAuthorized (KeySetName ks))
  (TBool, PreApp "row-enforced" [TableLit tn, ColumnLit cn, rk]) -> do
    _ <- expectTableExists tn
    _ <- expectColumnType tn cn TStr
    RowEnforced tn cn <$> checkPreProp TStr rk

  _ -> throwError $ "type error: " ++ show preProp ++ " does not have type " ++ show ty

expectColumnType :: TableName -> ColumnName -> Type a -> PropCheck ()
expectColumnType tn@(TableName tnStr) cn@(ColumnName cnStr) expectedTy = do
  tys <- asks (^.. _2 . ix tn . ix cn)
  case tys of
    [EType foundTy] -> case typeEq foundTy expectedTy of
      Nothing   -> throwError $
        "expected column " ++ cnStr ++ " in table " ++ tnStr ++
        " to have type " ++ userShow expectedTy ++ ", instead found " ++
        userShow foundTy
      Just Refl -> pure ()
    _ -> throwError $
      "didn't find expected column " ++ cnStr ++ " in table " ++ tnStr

expectTableExists :: TableName -> PropCheck ()
expectTableExists tn@(TableName tnStr) = do
  tn' <- view $ _2 . at tn
  case tn' of
    Nothing -> throwError $
      "expected table " ++ tnStr ++ "but it isn't in scope"
    Just _  -> pure ()

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
expToCheck tableEnv genStart nameEnv idEnv body = do
  preTypedBody <- evalStateT (runReaderT (expToPreProp body) nameEnv) genStart
  typedBody    <- runReaderT (checkPreProp TBool preTypedBody) (idEnv, tableEnv)
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
expToProp tableEnv genStart nameEnv idEnv ty body = do
  preTypedBody <- evalStateT (runReaderT (expToPreProp body) nameEnv) genStart
  runReaderT (checkPreProp ty preTypedBody) (idEnv, tableEnv)

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
        (_,        TyValue)   -> throwError
          "Invariants can't constrain opaque values"
        (_,        _)         -> throwError $
          "found variable " ++ show var ++ " of type " ++ show primTy ++
          " where " ++ show ty ++ " was expected"
      _ -> throwError $ "couldn't find column named " ++ show var

  (TDecimal, ELiteral (LDecimal d) _) -> pure (IDecimalLiteral (mkDecimal d))
  (TInt, ELiteral (LInteger i) _)     -> pure (IIntLiteral i)
  (TStr, stringLike -> Just s)        -> pure (IStringLiteral s)
  (TStr, ELiteral (LString _) _)      -> error "impossible (handled by stringLike)"
  (TTime, ELiteral (LTime t) _)       -> pure (ITimeLiteral (mkTime t))
  (TBool, ELiteral (LBool b) _)       -> pure (IBoolLiteral b)
  (_, ELiteral _ _)                   -> throwError "literal of unexpected type"

  (TInt, EList' [EAtom' "str-length", str]) -> IStrLength <$> expToInvariant TStr str
  (TStr, EList' [EAtom' "+", a, b])
    -> IStrConcat <$> expToInvariant TStr a <*> expToInvariant TStr b

  (TDecimal, EList' [EAtom' (textToArithOp -> Just op), a, b]) -> asum
    [ IDecArithOp    op <$> expToInvariant TDecimal a <*> expToInvariant TDecimal b
    , IDecIntArithOp op <$> expToInvariant TDecimal a <*> expToInvariant TInt b
    , IIntDecArithOp op <$> expToInvariant TInt a     <*> expToInvariant TDecimal b
    ]
  (TInt, EList' [EAtom' (textToArithOp -> Just op), a, b])
    -> IIntArithOp op <$> expToInvariant TInt a <*> expToInvariant TInt b
  (TDecimal, EList' [EAtom' (textToUnaryArithOp -> Just op), a])
    -> IDecUnaryArithOp op <$> expToInvariant TDecimal a
  (TInt, EList' [EAtom' (textToUnaryArithOp -> Just op), a])
    -> IIntUnaryArithOp op <$> expToInvariant TInt a

  (TBool, EList' [EAtom' op'@(textToComparisonOp -> Just op), a, b]) -> asum
    [ IIntComparison op     <$> expToInvariant TInt a     <*> expToInvariant TInt b
    , IDecimalComparison op <$> expToInvariant TDecimal a <*> expToInvariant TDecimal b
    , ITimeComparison op    <$> expToInvariant TTime a    <*> expToInvariant TTime b
    , IBoolComparison op    <$> expToInvariant TBool a    <*> expToInvariant TBool b
    , IStringComparison op  <$> expToInvariant TStr a     <*> expToInvariant TStr b
    , case textToEqNeq op' of
      Just eqNeq -> IKeySetEqNeq eqNeq
        <$> expToInvariant TKeySet a
        <*> expToInvariant TKeySet b
      Nothing -> throwError $ show op' ++ " is an invalid operation for keysets (only = or /= allowed)"
    ]

  (TBool, EList' (EAtom' op:args))
    | Just op' <- textToLogicalOp op -> do
    operands' <- traverse (expToInvariant TBool) args
    case (op', operands') of
      (AndOp, [a, b]) -> pure (ILogicalOp AndOp [a, b])
      (OrOp, [a, b])  -> pure (ILogicalOp OrOp [a, b])
      (NotOp, [a])    -> pure (ILogicalOp NotOp [a])
      _ -> throwError $ "logical op with wrong number of args: " ++ T.unpack op

  (_, ESymbol {})  -> throwError $ "illegal invariant form: " ++ show exp
  (_, EAtom {})    -> throwError $ "illegal invariant form: " ++ show exp
  (_, EList {})    -> throwError $ "illegal invariant form: " ++ show exp
  (_, EObject {})  -> throwError $ "illegal invariant form: " ++ show exp
  (_, EBinding {}) -> throwError $ "illegal invariant form: " ++ show exp
