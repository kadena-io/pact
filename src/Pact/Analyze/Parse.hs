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
  ) where

import           Control.Applicative  ((<|>))
import           Control.Lens         (at, makeLenses, view,
                                       (^.), (&), (%~), (.~))
import           Control.Monad.Except (mzero, throwError)
import           Control.Monad.Gen    (GenT, gen, runGenTFrom)
import           Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import           Control.Monad.Trans  (lift)
import           Data.Foldable        (asum, find)
import qualified Data.HashMap.Strict  as HM
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Prelude              hiding (exp)

import           Pact.Types.Lang      hiding (KeySet, KeySetName, SchemaVar,
                                       TKeySet, TableName, Type)
import qualified Pact.Types.Lang      as Pact
import           Pact.Types.Typecheck (UserType)

import           Pact.Analyze.PrenexNormalize as Prenex
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

data ParseEnv = ParseEnv
  { _quantifiedVars :: Map Text UniqueId
  , _definedVars    :: HM.HashMap Text Exp
  }

makeLenses ''ParseEnv

-- TODO: Maybe -> Either
type PropParse = ReaderT ParseEnv (GenT UniqueId Maybe)

-- TODO: Maybe -> Either
type PropCheck = ReaderT (Map UniqueId EType) Maybe

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
          fmap (\(uid, name, _ty) -> (name, uid)) bindings'
    body' <- local (& quantifiedVars %~ Map.union theseBindingsMap) (expToPreProp body)
    pure $ foldr
      (\(uid, name, ty) accum -> q uid name ty accum)
      body'
      bindings'

  EList' (EAtom' funName:args) -> PreApp funName <$> traverse expToPreProp args

  EAtom' "abort"   -> pure PreAbort
  EAtom' "success" -> pure PreSuccess
  EAtom' "result"  -> pure PreResult
  EAtom' var       -> lookupVar var

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

        lookupVar :: Text -> PropParse PreProp
        lookupVar var =
          let fromDefn = do
                defn <- view (definedVars . at var)
                case defn of
                -- Note: we remove this definition from the environment as we
                -- continue. This has the effect of inlining the definition and
                -- removing it from the environment. This is a slight layering
                -- violation but will do for now.
                  Just exp -> local (& definedVars . at var .~ Nothing)
                    (expToPreProp exp)
                  Nothing -> noParse
              fromQuantified = do
                mUid <- view (quantifiedVars . at var)
                case mUid of
                  Just uid -> pure (PreVar uid var)
                  Nothing  -> noParse
          in fromDefn <|> fromQuantified

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
      Nothing -> mzero
    ]

  (TBool, PreApp (textToLogicalOp -> Just op) args) -> case (op, args) of
    (NotOp, [a])    -> PNot <$> checkPreProp TBool a
    (AndOp, [a, b]) -> PAnd <$> checkPreProp TBool a <*> checkPreProp TBool b
    (OrOp, [a, b])  -> POr  <$> checkPreProp TBool a <*> checkPreProp TBool b
    _               -> mzero

  (TBool, PreApp "when" [a, b]) -> do
    propNotA <- PNot <$> checkPreProp TBool a
    POr propNotA <$> checkPreProp TBool b

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

  _ -> mzero

-- Convert an @Exp@ to a @Check@ in an environment where the variables have
-- types.
--
-- TODO: the one property this can't parse yet is PAt because it includes an
-- EType.
--
expToCheck
  :: UniqueId
  -- ^ ID to start issuing from
  -> Map Text UniqueId
  -- ^ Environment mapping names to unique IDs
  -> Map UniqueId EType
  -- ^ Environment mapping unique IDs to their types
  -> HM.HashMap Text Exp
  -- ^ Defined props in the environment
  -> Exp
  -- ^ Exp to convert
  -> Maybe Check
expToCheck genStart nameEnv idEnv defprops body
  = PropertyHolds <$> expToProp genStart nameEnv idEnv defprops TBool body

expToProp
  :: Prenex.Float a
  => UniqueId
  -- ^ ID to start issuing from
  -> Map Text UniqueId
  -- ^ Environment mapping names to unique IDs
  -> Map UniqueId EType
  -- ^ Environment mapping unique IDs to their types
  -> HM.HashMap Text Exp
  -- ^ Defined props in the environment
  -> Type a
  -- ^ Expected prop type
  -> Exp
  -- ^ Exp to convert
  -> Maybe (Prop a)
expToProp genStart nameEnv idEnv defprops ty body = do
  preTypedBody <- runGenTFrom genStart
    (runReaderT (expToPreProp body) (ParseEnv nameEnv defprops))
  typedBody    <- runReaderT (checkPreProp ty preTypedBody) idEnv
  pure $ prenexConvert typedBody

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
      Nothing -> mzero
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
