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
  ) where

import           Control.Lens         ((^.), at, view)
import           Control.Monad.Gen    (GenT, gen, runGenT)
import           Control.Monad.Reader (ReaderT, local, runReaderT)
import           Control.Monad.Trans  (lift)
import           Data.Foldable        (asum, find)
import           Data.Map             (Map)
import qualified Data.Map             as Map
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Traversable     (for)
import           Data.Type.Equality   ((:~:) (Refl))

import           Pact.Types.Lang      hiding (KeySet, KeySetName, SchemaVar,
                                       TKeySet, TableName, Type)
import qualified Pact.Types.Lang      as Pact
import           Pact.Types.Typecheck (UserType)

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
  :: Text -> Maybe (UniqueId -> Text -> Ty -> PreProp -> PreProp)
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

-- TODO: Maybe -> Either
type PropParse = ReaderT (Map Text UniqueId) (GenT UniqueId Maybe)

-- TODO: Maybe -> Either
type PropCheck = ReaderT (Map UniqueId EType) Maybe

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
          fmap (\(uid, name, _ty) -> (name, uid)) bindings'
    body'     <- local (Map.union theseBindingsMap) (expToPreProp body)
    pure $ foldr
      (\(uid, name, ty) accum -> q uid name ty accum)
      body'
      bindings'

  EList' (EAtom' funName:args) -> PreApp funName <$> traverse expToPreProp args

  EAtom' "abort"   -> pure PreAbort
  EAtom' "success" -> pure PreSuccess
  EAtom' "result"  -> pure PreResult
  EAtom' var       -> mkVar var

  _ -> noParse

  where propBindings :: [Exp] -> PropParse [(UniqueId, Text, Ty)]
        propBindings [] = pure []
        -- we require a type annotation
        propBindings (EAtom _name _qual Nothing _parsed:_exps) = noParse
        propBindings (EAtom name _qual (Just ty) _parsed:exps) = do
          nameTy <- case ty of
            TyPrim TyString -> do
              uid <- gen
              pure (uid, name, Ty (Rep @String))
            _               -> noParse
          (nameTy:) <$> propBindings exps
        propBindings _ = noParse

        noParse :: PropParse a
        noParse = lift (lift Nothing)

        mkVar :: Text -> PropParse PreProp
        mkVar var = do
          mUid <- view (at var)
          case mUid of
            Nothing  -> noParse
            Just uid -> pure (PreVar uid var)

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
  (_, PreVar uid name) -> pure (PVar uid name)

  -- quantifiers
  (a, PreForall uid name ty' p) -> Forall uid name ty' <$> checkPreProp a p
  (a, PreExists uid name ty' p) -> Exists uid name ty' <$> checkPreProp a p

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

  (TBool, PreApp (textToComparisonOp -> Just op) [a, b]) -> asum
    [ PIntegerComparison op <$> checkPreProp TInt a     <*> checkPreProp TInt b
    , PDecimalComparison op <$> checkPreProp TDecimal a <*> checkPreProp TDecimal b
    , PTimeComparison op    <$> checkPreProp TTime a    <*> checkPreProp TTime b
    , PBoolComparison op    <$> checkPreProp TBool a    <*> checkPreProp TBool b
    , PStringComparison op  <$> checkPreProp TStr a     <*> checkPreProp TStr b
    , PKeySetComparison op  <$> checkPreProp TKeySet a  <*> checkPreProp TKeySet b
    ]

  (TBool, PreApp (textToLogicalOp -> Just op) args) -> case (op, args) of
    (NotOp, [a])    -> PNot <$> checkPreProp TBool a
    (AndOp, [a, b]) -> PAnd <$> checkPreProp TBool a <*> checkPreProp TBool b
    (OrOp, [a, b])  -> POr  <$> checkPreProp TBool a <*> checkPreProp TBool b
    _               -> lift Nothing

  (TBool, PreApp "table-write" [TableLit tn]) -> pure (TableWrite tn)
  (TBool, PreApp "table-read" [TableLit tn])  -> pure (TableRead tn)
  (TBool, PreApp "column-write" [TableLit tn, ColumnLit cn])
    -> pure (ColumnWrite tn cn)
  (TBool, PreApp "cell-increase" [TableLit tn, ColumnLit cn])
    -> pure (CellIncrease tn cn)
  (TInt, PreApp "cell-delta" [TableLit tn, ColumnLit cn, rk])
    -> IntCellDelta tn cn <$> checkPreProp TStr rk
  (TDecimal, PreApp "cell-delta" [TableLit tn, ColumnLit cn, rk])
    -> DecCellDelta tn cn <$> checkPreProp TStr rk
  (TInt, PreApp "column-delta" [TableLit tn, ColumnLit cn])
    -> pure (IntColumnDelta tn cn)
  (TDecimal, PreApp "column-delta" [TableLit tn, ColumnLit cn])
    -> pure (DecColumnDelta tn cn)
  (TBool, PreApp "row-read" [TableLit tn, rk])
    -> RowRead tn <$> checkPreProp TStr rk
  (TBool, PreApp "row-write" [TableLit tn, rk])
    -> RowWrite tn <$> checkPreProp TStr rk
  (TBool, PreApp "authorized-by" [PreStringLit ks])
    -> pure (KsNameAuthorized (KeySetName ks))
  (TBool, PreApp "row-enforced" [TableLit tn, ColumnLit cn, rk])
    -> RowEnforced tn cn <$> checkPreProp TStr rk

  _ -> lift Nothing

--
-- TODO: the one property this can't parse yet is PAt because it includes an
-- EType.
--
expToCheck :: Exp -> Maybe Check
expToCheck body = do
  preTypedBody <- runGenT (runReaderT (expToPreProp body) Map.empty)
  typedBody    <- runReaderT (checkPreProp TBool preTypedBody) Map.empty
  pure $ PropertyHolds $ prenexConvert typedBody

expToProp :: Type a -> Exp -> Maybe (Prop a)
expToProp ty body = do
  preTypedBody <- runGenT (runReaderT (expToPreProp body) Map.empty)
  runReaderT (checkPreProp ty preTypedBody) Map.empty

-- We pass in the type of the variable so we can use it to construct
-- `SomeSchemaInvariant` when we encounter a var.
-- TODO(joel): finish these!
expToInvariant :: [Pact.Arg UserType] -> Exp -> Maybe SomeSchemaInvariant
expToInvariant schemaTys = \case
  EAtom' var -> case find (\arg -> arg ^. aName == var) schemaTys of
    Just (Pact.Arg _name (TyPrim primTy) _info) -> case primTy of
      TyInteger -> Just (SomeSchemaInvariant (SchemaVar var) TInt)
      TyDecimal -> Just (SomeSchemaInvariant (SchemaVar var) TDecimal)
      TyTime    -> Just (SomeSchemaInvariant (SchemaVar var) TTime)
      TyString  -> Just (SomeSchemaInvariant (SchemaVar var) TStr)
      TyBool    -> Just (SomeSchemaInvariant (SchemaVar var) TBool)
      TyKeySet  -> Just (SomeSchemaInvariant (SchemaVar var) TKeySet)
      TyValue   -> Nothing
    _ -> Nothing

  ELiteral (LDecimal d) _ -> Just
    (SomeSchemaInvariant (SchemaDecimalLiteral (mkDecimal d)) TDecimal)
  ELiteral (LInteger i) _ -> Just
    (SomeSchemaInvariant (SchemaIntLiteral i) TInt)
  (stringLike -> Just s) -> Just
    (SomeSchemaInvariant (SchemaStringLiteral s) TStr)
  ELiteral (LString _) _ -> error "impossible (handled by stringLike)"
  ELiteral (LTime t) _ -> Just
    (SomeSchemaInvariant (SchemaTimeLiteral (mkTime t)) TTime)
  ELiteral (LBool b) _ -> Just
    (SomeSchemaInvariant (SchemaBoolLiteral b) TBool)

  EList' [EAtom' "+", a, b] -> do
    SomeSchemaInvariant a' TInt <- expToInvariant schemaTys a
    SomeSchemaInvariant b' TInt <- expToInvariant schemaTys b
    pure (SomeSchemaInvariant (SchemaIntArithOp Add a' b') TInt)

  EList' [EAtom' op, a, b]
    | Just op' <- textToComparisonOp op -> do
    SomeSchemaInvariant a' aTy <- expToInvariant schemaTys a
    SomeSchemaInvariant b' bTy <- expToInvariant schemaTys b
    let opEqNeq = case op of
          "="  -> Just Eq'
          "!=" -> Just Neq'
          _    -> Nothing

    case typeEq aTy bTy of
      Just Refl -> case aTy of
        TDecimal ->
          Just (SomeSchemaInvariant (SchemaDecimalComparison op' a' b') TBool)
        TInt     ->
          Just (SomeSchemaInvariant (SchemaIntComparison op' a' b') TBool)
        TStr     ->
          Just (SomeSchemaInvariant (SchemaStringComparison op' a' b') TBool)
        TTime    ->
          Just (SomeSchemaInvariant (SchemaTimeComparison op' a' b') TBool)

        TBool    -> do
          opEqNeq' <- opEqNeq
          pure (SomeSchemaInvariant (SchemaBoolEqNeq opEqNeq' a' b') TBool)
        TKeySet  -> do
          opEqNeq' <- opEqNeq
          pure (SomeSchemaInvariant (SchemaKeySetEqNeq opEqNeq' a' b') TBool)

        TAny -> Nothing
      Nothing   -> Nothing

  EList' (EAtom' op:args)
    | op `Set.member` Set.fromList ["and", "or", "not"] -> do
    operands' <- for args $ \arg -> do
      SomeSchemaInvariant arg' TBool <- expToInvariant schemaTys arg
      Just arg'
    case (op, operands') of
      ("and", [a, b]) ->
        Just $ SomeSchemaInvariant (SchemaLogicalOp AndOp [a, b]) TBool
      ("or", [a, b]) ->
        Just $ SomeSchemaInvariant (SchemaLogicalOp OrOp [a, b]) TBool
      ("not", [a]) ->
        Just $ SomeSchemaInvariant (SchemaLogicalOp NotOp [a]) TBool
      _ -> Nothing

  ESymbol {}  -> Nothing
  EAtom {}    -> Nothing
  EList {}    -> Nothing
  EObject {}  -> Nothing
  EBinding {} -> Nothing
