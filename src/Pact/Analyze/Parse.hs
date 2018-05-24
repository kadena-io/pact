{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Pact.Analyze.Parse
  ( expToCheck
  , expToInvariant
  , prenexConvert
  ) where

import           Control.Lens         ((^.))
import           Control.Monad.Gen    -- (Gen)
import           Data.Foldable        (asum, find)
import           Data.Semigroup       ((<>))
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Traversable     (for)
import           Data.Type.Equality   ((:~:) (Refl))

import           Pact.Types.Lang      hiding (KeySet, KeySetName, SchemaVar,
                                       TKeySet, TableName)
import           Pact.Types.Typecheck (UserType)
import           Pact.Types.Util      (tShow)

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

mkT :: Text -> TableName
mkT = TableName . T.unpack

mkC :: Text -> ColumnName
mkC = ColumnName . T.unpack

mkK :: Text -> KeySetName
mkK = KeySetName

expToPropRowKey :: Exp -> Maybe (Prop RowKey)
expToPropRowKey = \case
  EAtom' "result" -> Just (PVar Nothing "result")
  EAtom' var      -> Just (PVar Nothing var)
  _               -> Nothing

expToPropInteger :: Exp -> Maybe (Prop Integer)
expToPropInteger = \case
  EAtom' "result"                    -> Just (PVar Nothing "result")
  EAtom' var                         -> Just (PVar Nothing var)
  ELiteral (LInteger i) _            -> Just (PLit i)
  EList' [EAtom' "string-length", a] -> PStrLength <$> expToPropString a

  EList' [EAtom' "mod", a, b]
    -> PModOp <$> expToPropInteger a <*> expToPropInteger b

  EList' [EAtom' op, a]
    | op `Set.member` Set.fromList [ "round", "ceiling", "floor" ] -> do
    let op' = case op of
          "round"   -> Round
          "ceiling" -> Ceiling
          "floor"   -> Floor
          _         -> error "impossible"
    PRoundingLikeOp1 op' <$> expToPropDecimal a

  EList' [EAtom' op, a]
    -> PIntUnaryArithOp <$> textToUnaryArithOp op <*> expToPropInteger a

  EList' [EAtom' op, a, b] -> PIntArithOp
    <$> textToArithOp op
    <*> expToPropInteger a
    <*> expToPropInteger b

  EList' [EAtom' "column-delta", ELitString tab, ELitString col]
    -> Just (IntColumnDelta (mkT tab) (mkC col))

  _ -> Nothing

expToPropString :: Exp -> Maybe (Prop String)
expToPropString = \case
  EAtom' "result"        -> Just (PVar Nothing "result")
  EAtom' var             -> Just (PVar Nothing var)
  ELiteral (LString s) _ -> Just (PLit (T.unpack s))
  EList' [EAtom' "+", a, b]
    -> PStrConcat <$> expToPropString a <*> expToPropString b
  _ -> Nothing

expToPropDecimal :: Exp -> Maybe (Prop Decimal)
expToPropDecimal = \case
  EAtom' "result"         -> Just (PVar Nothing "result")
  EAtom' var              -> Just (PVar Nothing var)
  ELiteral (LDecimal d) _ -> Just (PLit (mkDecimal d))
  EList' [EAtom' op, a, b]
    | op `Set.member` Set.fromList [ "round", "ceiling", "floor" ] -> do
    let op' = case op of
          "round"   -> Round
          "ceiling" -> Ceiling
          "floor"   -> Floor
          _         -> error "impossible"
    PRoundingLikeOp2 op' <$> expToPropDecimal a <*> expToPropInteger b

  EList' [EAtom' op, a, b] -> asum
    [ PDecArithOp
      <$> textToArithOp op <*> expToPropDecimal a <*> expToPropDecimal b
    , PDecIntArithOp
      <$> textToArithOp op <*> expToPropDecimal a <*> expToPropInteger b
    , PIntDecArithOp
      <$> textToArithOp op <*> expToPropInteger a <*> expToPropDecimal b
    ]

  EList' [EAtom' op, a]
    -> PDecUnaryArithOp <$> textToUnaryArithOp op <*> expToPropDecimal a

  EList' [EAtom' "column-delta", ELitString tab, ELitString col]
    -> Just (DecColumnDelta (mkT tab) (mkC col))

  _ -> Nothing

expToPropTime :: Exp -> Maybe (Prop Time)
expToPropTime = \case
  EAtom' "result"      -> Just (PVar Nothing "result")
  EAtom' var           -> Just (PVar Nothing var)
  ELiteral (LTime t) _ -> Just (PLit (mkTime t))
  EList' [EAtom' "add-time", a, b] -> do
    a' <- expToPropTime a
    asum
      [ PIntAddTime a' <$> expToPropInteger b
      , PDecAddTime a' <$> expToPropDecimal b
      ]
  _ -> Nothing

expToPropKeySet :: Exp -> Maybe (Prop KeySet)
expToPropKeySet = \case
  EAtom' "result" -> Just (PVar Nothing "result")
  EAtom' var      -> Just (PVar Nothing var)
  _               -> Nothing

expToPropBool :: Exp -> Maybe (Prop Bool)
expToPropBool = \case
  EAtom' "result"      -> Just (PVar Nothing "result")
  ELiteral (LBool b) _ -> Just (PLit b)

  EList' [EAtom' "when", a, b] -> do
    propNotA <- PLogical NotOp <$> traverse expToPropBool [a]
    PLogical OrOp . (propNotA:) <$> traverse expToPropBool [b]

  EList' [EAtom' "row-read", ELitString tab, rowKey] ->
    RowRead (mkT tab) <$> expToPropRowKey rowKey
  EList' [EAtom' "row-write", ELitString tab, rowKey] ->
    RowWrite (mkT tab) <$> expToPropRowKey rowKey

  EAtom' "abort"   -> Just Abort
  EAtom' "success" -> Just Success

  EList' [EAtom' "not", a]     -> PLogical NotOp <$> traverse expToPropBool [a]
  EList' [EAtom' "and", a, b]  -> PLogical AndOp <$> traverse expToPropBool [a, b]
  EList' [EAtom' "or", a, b]   -> PLogical OrOp  <$> traverse expToPropBool [a, b]

  EList' [EAtom' "table-write", ELitString tab] -> Just (TableWrite (mkT tab))
  EList' [EAtom' "table-read", ELitString tab] -> Just (TableRead (mkT tab))
  EList' [EAtom' "column-write", ELitString tab, ELitString col]
    -> Just (ColumnWrite (mkT tab) (mkC col))
  EList' [EAtom' "cell-increase", ELitString tab, ELitString col]
    -> Just (CellIncrease (mkT tab) (mkC col))

  -- TODO: in the future, these should be moved into a stdlib:
  EList' [EAtom' "column-conserve", ELitString tab, ELitString col]
    -> Just (PIntegerComparison Eq 0 $ IntColumnDelta (mkT tab) (mkC col))
  EList' [EAtom' "column-increase", ELitString tab, ELitString col]
    -> Just (PIntegerComparison Lt 0 $ IntColumnDelta (mkT tab) (mkC col))

  --
  -- TODO: add support for DecColumnDelta. but we need type info...
  --

  EList' [EAtom' "row-enforced", ELitString tab, ELitString col, body] -> do
    body' <- expToPropRowKey body
    Just (RowEnforced (mkT tab) (mkC col) body')

  EList' [EAtom' "authorized-by", ELitString name]
    -> Just (KsNameAuthorized (mkK name))

  EList' [EAtom' "authorized-by", ESymbol name _]
    -> Just (KsNameAuthorized (mkK name))

  EList' [EAtom' "forall", EList' bindings, body] -> do
    bindings' <- propBindings bindings
    body'     <- expToPropBool body
    pure $ foldr
      (\(name, ty) accum -> Forall name ty accum)
      body'
      bindings'
  EList' [EAtom' "exists", EList' bindings, body] -> do
    bindings' <- propBindings bindings
    body'     <- expToPropBool body
    pure $ foldr
      (\(name, ty) accum -> Exists name ty accum)
      body'
      bindings'

  EList' [EAtom' op, a, b] -> do
    op' <- textToComparisonOp op
    asum
      [ PIntegerComparison op' <$> expToPropInteger a <*> expToPropInteger b
      , PDecimalComparison op' <$> expToPropDecimal a <*> expToPropDecimal b
      , PTimeComparison op'    <$> expToPropTime a    <*> expToPropTime b
      , PBoolComparison op'    <$> expToPropBool a    <*> expToPropBool b
      , PStringComparison op'  <$> expToPropString a  <*> expToPropString b
      , PKeySetComparison op'  <$> expToPropKeySet a  <*> expToPropKeySet b
      ]

  EAtom' var           -> Just (PVar Nothing var)

  _ -> Nothing

  where propBindings :: [Exp] -> Maybe [(Text, Ty)]
        propBindings [] = Just []
        -- we require a type annotation
        propBindings (EAtom _name _qual Nothing _parsed:_exps) = Nothing
        propBindings (EAtom name _qual (Just ty) _parsed:exps) = do
          nameTy <- case ty of
            TyPrim TyString -> Just (name, Ty (Rep @RowKey))
            _               -> Nothing
          (nameTy:) <$> propBindings exps
        propBindings _ = Nothing

type UniqueId = Int

data ForallExists
  = Forall' Text UniqueId Ty
  | Exists' Text UniqueId Ty

flipQuantifier :: ForallExists -> ForallExists
flipQuantifier = \case
  Forall' name uid ty -> Exists' name uid ty
  Exists' name uid ty -> Forall' name uid ty

rename :: Maybe UniqueId -> Text -> Prop Bool -> Prop Bool

floatQuantifiers :: Prop Bool -> Gen UniqueId ([ForallExists], Prop Bool)
floatQuantifiers p = case p of
  Forall name ty prop -> do
    (qs, prop') <- floatQuantifiers prop
    uid <- gen
    pure (Forall' name uid ty:qs, prop')
  Exists name ty prop -> do
    (qs, prop') <- floatQuantifiers prop
    uid <- gen
    pure (Exists' name uid ty:qs, prop')
  PAnd a b -> do
    (aQs, a') <- floatQuantifiers a
    (bQs, b') <- floatQuantifiers b
    pure (aQs ++ bQs, PAnd a' b')
  POr a b -> do
    (aQs, a') <- floatQuantifiers a
    (bQs, b') <- floatQuantifiers b
    pure (aQs ++ bQs, POr a' b')
  PNot a -> do
    (qs, a') <- floatQuantifiers a
    pure $ (flipQuantifier <$> qs, PNot a')
  PVar _ v -> pure ([], p)

reassembleFloated :: [ForallExists] -> Prop Bool -> Prop Bool
reassembleFloated qs prop =
  let getName = \case
        Forall' name _ _ -> name
        Exists' name _ _ -> name
      getUid = \case
        Forall' _ uid _ -> uid
        Exists' _ uid _ -> uid
      quantifiedNames = getName <$> qs
      nameIsUnique name
        = length (filter (== name) quantifiedNames) == 1
      getUniqueName quantifier =
        let name = getName quantifier
        in if nameIsUnique name
           then name
           else name <> tShow (getUid quantifier)
      mkQuantifiedProp q acc = case q of
        Forall' name uid ty -> Forall (getUniqueName q) ty acc
        Exists' name uid ty -> Exists (getUniqueName q) ty acc
  in foldr mkQuantifiedProp prop qs

prenexConvert :: Prop Bool -> Prop Bool
prenexConvert prop =
  let (quantifiers, prop') = runGen $ floatQuantifiers prop
  in reassembleFloated quantifiers prop'

-- Note: the one property this can't parse yet is PAt because it includes an
-- EType.
expToCheck :: Exp -> Maybe Check
expToCheck body = Valid . prenexConvert <$> expToPropBool body

-- We pass in the type of the variable so we can use it to construct
-- `SomeSchemaInvariant` when we encounter a var.
-- TODO(joel): finish these!
expToInvariant :: [Arg UserType] -> Exp -> Maybe SomeSchemaInvariant
expToInvariant schemaTys = \case
  EAtom' var -> case find (\arg -> arg ^. aName == var) schemaTys of
    Just (Arg _name (TyPrim primTy) _info) -> case primTy of
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
  ELiteral (LString s) _ -> Just
    (SomeSchemaInvariant (SchemaStringLiteral s) TStr)
  ELiteral (LTime t) _ -> Just
    (SomeSchemaInvariant (SchemaTimeLiteral (mkTime t)) TTime)
  ELiteral (LBool b) _ -> Just
    (SomeSchemaInvariant (SchemaBoolLiteral b) TBool)

  EList' [EAtom' op, a, b]
    | op `Set.member` Set.fromList [">", "<", ">=", "<=", "=", "!="] -> do
    SomeSchemaInvariant a' aTy <- expToInvariant schemaTys a
    SomeSchemaInvariant b' bTy <- expToInvariant schemaTys b
    let op' = case op of
          ">"  -> Gt
          "<"  -> Lt
          ">=" -> Gte
          "<=" -> Lte
          "="  -> Eq
          "!=" -> Neq
          _    -> error "impossible"
        opEqNeq = case op of
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
