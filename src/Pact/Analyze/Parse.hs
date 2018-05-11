{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Pact.Analyze.Parse
  ( expToCheck
  , expToInvariant
  ) where

import           Control.Lens         ((^.))
import           Data.Foldable        (asum, find)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Traversable     (for)
import           Data.Type.Equality   ((:~:) (Refl))

import           Pact.Types.Lang      hiding (KeySet, KeySetName, SchemaVar,
                                       TKeySet, TableName)
import           Pact.Types.Typecheck (UserType)

import           Pact.Analyze.Types

textToArithOp :: Text -> Either String ArithOp
textToArithOp = \case
  "+"   -> Right Add
  "-"   -> Right Sub
  "*"   -> Right Mul
  "/"   -> Right Div
  "^"   -> Right Pow
  "log" -> Right Log
  str   -> Left $ "unrecognized arith op: " ++ T.unpack str

textToUnaryArithOp :: Text -> Either String UnaryArithOp
textToUnaryArithOp = \case
  "-"    -> Right Negate
  "sqrt" -> Right Sqrt
  "ln"   -> Right Ln
  "exp"  -> Right Exp
  "abs"  -> Right Abs
  -- explicitly no signum
  _      -> Nothing

textToComparisonOp :: Text -> Either String ComparisonOp
textToComparisonOp = \case
  ">"  -> Right Gt
  "<"  -> Right Lt
  ">=" -> Right Gte
  "<=" -> Right Lte
  "="  -> Right Eq
  "!=" -> Right Neq
  _    -> Nothing

mkT :: Text -> TableName
mkT = TableName . T.unpack

mkC :: Text -> ColumnName
mkC = ColumnName . T.unpack

mkK :: Text -> KeySetName
mkK = KeySetName

expToPropRowKey :: Exp -> Either String (Prop RowKey)
expToPropRowKey = \case
  EAtom' "result" -> Right (PVar "result")
  EAtom' var      -> Right (PVar var)
  _               -> Nothing

expToPropInteger :: Exp -> Either String (Prop Integer)
expToPropInteger = \case
  EAtom' "result"                    -> Right (PVar "result")
  EAtom' var                         -> Right (PVar var)
  ELiteral (LInteger i) _            -> Right (PLit i)
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
    -> Right (IntColumnDelta (mkT tab) (mkC col))

  _ -> Nothing

expToPropString :: Exp -> Either String (Prop String)
expToPropString = \case
  EAtom' "result"        -> Right (PVar "result")
  EAtom' var             -> Right (PVar var)
  ELiteral (LString s) _ -> Right (PLit (T.unpack s))
  EList' [EAtom' "+", a, b]
    -> PStrConcat <$> expToPropString a <*> expToPropString b
  _ -> Nothing

expToPropDecimal :: Exp -> Either String (Prop Decimal)
expToPropDecimal = \case
  EAtom' "result"         -> Right (PVar "result")
  EAtom' var              -> Right (PVar var)
  ELiteral (LDecimal d) _ -> Right (PLit (mkDecimal d))
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
    -> Right (DecColumnDelta (mkT tab) (mkC col))

  _ -> Nothing

expToPropTime :: Exp -> Either String (Prop Time)
expToPropTime = \case
  EAtom' "result"      -> Right (PVar "result")
  EAtom' var           -> Right (PVar var)
  ELiteral (LTime t) _ -> Right (PLit (mkTime t))
  EList' [EAtom' "add-time", a, b] -> do
    a' <- expToPropTime a
    asum
      [ PIntAddTime a' <$> expToPropInteger b
      , PDecAddTime a' <$> expToPropDecimal b
      ]
  _ -> Nothing

expToPropKeySet :: Exp -> Either String (Prop KeySet)
expToPropKeySet = \case
  EAtom' "result" -> Right (PVar "result")
  EAtom' var      -> Right (PVar var)
  _               -> Nothing

expToPropBool :: Exp -> Either String (Prop Bool)
expToPropBool = \case
  EAtom' "result"      -> Right (PVar "result")
  ELiteral (LBool b) _ -> Right (PLit b)

  EList' [EAtom' "when", a, b] -> do
    propNotA <- PLogical NotOp <$> traverse expToPropBool [a]
    PLogical OrOp . (propNotA:) <$> traverse expToPropBool [b]

  EList' [EAtom' "row-read", ELitString tab, rowKey] ->
    RowRead (mkT tab) <$> expToPropRowKey rowKey
  EList' [EAtom' "row-write", ELitString tab, rowKey] ->
    RowWrite (mkT tab) <$> expToPropRowKey rowKey

  EAtom' "abort"   -> Right Abort
  EAtom' "success" -> Right Success

  EList' [EAtom' "not", a]     -> PLogical NotOp <$> traverse expToPropBool [a]
  EList' [EAtom' "and", a, b]  -> PLogical AndOp <$> traverse expToPropBool [a, b]
  EList' [EAtom' "or", a, b]   -> PLogical OrOp  <$> traverse expToPropBool [a, b]

  EList' [EAtom' "table-write", ELitString tab] -> Right (TableWrite (mkT tab))
  EList' [EAtom' "table-read", ELitString tab] -> Right (TableRead (mkT tab))
  EList' [EAtom' "column-write", ELitString tab, ELitString col]
    -> Right (ColumnWrite (mkT tab) (mkC col))
  EList' [EAtom' "cell-increase", ELitString tab, ELitString col]
    -> Right (CellIncrease (mkT tab) (mkC col))

  -- TODO: in the future, these should be moved into a stdlib:
  EList' [EAtom' "column-conserve", ELitString tab, ELitString col]
    -> Right (PComparison Eq 0 $ IntColumnDelta (mkT tab) (mkC col))
  EList' [EAtom' "column-increase", ELitString tab, ELitString col]
    -> Right (PComparison Lt 0 $ IntColumnDelta (mkT tab) (mkC col))

  --
  -- TODO: add support for DecColumnDelta. but we need type info...
  --

  EList' [EAtom' "row-enforced", ELitString tab, ELitString col, body] -> do
    body' <- expToPropRowKey body
    Right (RowEnforced (mkT tab) (mkC col) body')

  EList' [EAtom' "authorized-by", ELitString name]
    -> Right (KsNameAuthorized (mkK name))

  EList' [EAtom' "authorized-by", ESymbol name _]
    -> Right (KsNameAuthorized (mkK name))

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
      [ PComparison op' <$> expToPropInteger a <*> expToPropInteger b
      , PComparison op' <$> expToPropDecimal a <*> expToPropDecimal b
      , PComparison op' <$> expToPropTime a    <*> expToPropTime b
      , PComparison op' <$> expToPropBool a    <*> expToPropBool b
      , PComparison op' <$> expToPropString a  <*> expToPropString b
      , PComparison op' <$> expToPropKeySet a  <*> expToPropKeySet b
      ]

  EAtom' var           -> Right (PVar var)

  _ -> Nothing

  where propBindings :: [Exp] -> Either String [(Text, Ty)]
        propBindings [] = Right []
        -- we require a type annotation
        propBindings (EAtom _name _qual Nothing _parsed:_exps) = Nothing
        propBindings (EAtom name _qual (Right ty) _parsed:exps) = do
          nameTy <- case ty of
            TyPrim TyString -> Right (name, Ty (Rep @RowKey))
            _               -> Nothing
          (nameTy:) <$> propBindings exps
        propBindings _ = Nothing

-- Note: the one property this can't parse yet is PAt because it includes an
-- EType.
expToCheck :: Exp -> Either String Check
expToCheck body = Valid <$> expToPropBool body

-- We pass in the type of the variable so we can use it to construct
-- `SomeSchemaInvariant` when we encounter a var.
-- TODO(joel): finish these!
expToInvariant :: [Arg UserType] -> Exp -> Either String SomeSchemaInvariant
expToInvariant schemaTys = \case
  EAtom' var -> case find (\arg -> arg ^. aName == var) schemaTys of
    Right (Arg _name (TyPrim primTy) _info) -> case primTy of
      TyInteger -> Right (SomeSchemaInvariant (SchemaVar var) TInt)
      TyDecimal -> Right (SomeSchemaInvariant (SchemaVar var) TDecimal)
      TyTime    -> Right (SomeSchemaInvariant (SchemaVar var) TTime)
      TyString  -> Right (SomeSchemaInvariant (SchemaVar var) TStr)
      TyBool    -> Right (SomeSchemaInvariant (SchemaVar var) TBool)
      TyKeySet  -> Right (SomeSchemaInvariant (SchemaVar var) TKeySet)
      TyValue   -> Nothing
    _ -> Nothing

  ELiteral (LDecimal d) _ -> Right
    (SomeSchemaInvariant (SchemaDecimalLiteral (mkDecimal d)) TDecimal)
  ELiteral (LInteger i) _ -> Right
    (SomeSchemaInvariant (SchemaIntLiteral i) TInt)
  ELiteral (LString s) _ -> Right
    (SomeSchemaInvariant (SchemaStringLiteral s) TStr)
  ELiteral (LTime t) _ -> Right
    (SomeSchemaInvariant (SchemaTimeLiteral (mkTime t)) TTime)
  ELiteral (LBool b) _ -> Right
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
          "="  -> Right Eq'
          "!=" -> Right Neq'
          _    -> Nothing

    case typeEq aTy bTy of
      Right Refl -> case aTy of
        TDecimal ->
          Right (SomeSchemaInvariant (SchemaDecimalComparison op' a' b') TBool)
        TInt     ->
          Right (SomeSchemaInvariant (SchemaIntComparison op' a' b') TBool)
        TStr     ->
          Right (SomeSchemaInvariant (SchemaStringComparison op' a' b') TBool)
        TTime    ->
          Right (SomeSchemaInvariant (SchemaTimeComparison op' a' b') TBool)

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
      Right arg'
    case (op, operands') of
      ("and", [a, b]) ->
        Right $ SomeSchemaInvariant (SchemaLogicalOp AndOp [a, b]) TBool
      ("or", [a, b]) ->
        Right $ SomeSchemaInvariant (SchemaLogicalOp OrOp [a, b]) TBool
      ("not", [a]) ->
        Right $ SomeSchemaInvariant (SchemaLogicalOp NotOp [a]) TBool
      _ -> Nothing

  ESymbol {}  -> Nothing
  EAtom {}    -> Nothing
  EList {}    -> Nothing
  EObject {}  -> Nothing
  EBinding {} -> Nothing
