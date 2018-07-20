{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Pact.Analyze.Parse
  ( PreProp(..)
  , TableEnv
  , expToCheck
  , expToProp
  , expToInvariant
  , inferProp
  ) where

import           Control.Applicative          (Alternative, (<|>))
import           Control.Lens                 (at, ix, makeLenses, view, (%~),
                                               (&), (?~), (^.), (^..), (^?))
import           Control.Monad                (unless)
import           Control.Monad.Except         (MonadError (throwError))
import           Control.Monad.Reader         (ReaderT, ask, asks, local,
                                               runReaderT)
import           Control.Monad.State.Strict   (StateT, evalStateT)
import           Data.Foldable                (asum, find)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (isJust)
import           Data.Semigroup               ((<>))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Traversable             (for)
import           Data.Type.Equality           ((:~:) (Refl))
import           Prelude                      hiding (exp)

import           Pact.Types.Lang              hiding (EObject, KeySet,
                                               KeySetName, SchemaVar, TKeySet,
                                               TableName, Type)
import qualified Pact.Types.Lang              as Pact
import           Pact.Types.Typecheck         (UserType)
import           Pact.Types.Util              (tShow)

import           Pact.Analyze.Feature         hiding (Type, Var, str, obj, ks)
import           Pact.Analyze.PrenexNormalize
import           Pact.Analyze.Translate
import           Pact.Analyze.Types
import           Pact.Analyze.Util


-- @PreProp@ stands between @Exp@ and @Prop@.
--
-- The conversion from @Exp@ is light, handled in @expToPreProp@.
data PreProp
  -- literals
  = PreIntegerLit Integer
  | PreStringLit  Text
  | PreDecimalLit Decimal
  | PreTimeLit    Time
  | PreBoolLit    Bool

  -- identifiers
  | PreAbort
  | PreSuccess
  | PreResult
  | PreVar     VarId Text

  -- quantifiers
  | PreForall VarId Text QType PreProp
  | PreExists VarId Text QType PreProp

  -- applications
  | PreApp Text [PreProp]

  | PreAt Text PreProp
  | PreLiteralObject (Map Text PreProp)
  deriving Eq

instance UserShow PreProp where
  userShowsPrec prec = \case
    PreIntegerLit i -> tShow i
    PreStringLit t  -> tShow t
    PreDecimalLit d -> tShow d
    PreTimeLit t    -> tShow (Pact.LTime (unMkTime t))
    PreBoolLit b    -> tShow (Pact.LBool b)

    PreAbort        -> "abort"
    PreSuccess      -> "success"
    PreResult       -> "result"
    PreVar _id name -> name

    PreForall _vid name qty prop ->
      "(forall (" <> name <> ":" <> userShow qty <> ") " <> userShow prop <> ")"
    PreExists _vid name qty prop ->
      "(exists (" <> name <> ":" <> userShow qty <> ") " <> userShow prop <> ")"
    PreApp name applicands -> "(" <> name <> " " <> T.unwords
      ((map userShow) applicands) <> ")"

    PreAt objIx obj      -> "(at '" <> objIx <> " " <> userShow obj <> ")"
    PreLiteralObject obj -> userShowsPrec prec obj


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
  SAddition       -> Just Add
  SSubtraction    -> Just Sub
  SMultiplication -> Just Mul
  SDivision       -> Just Div
  SExponentiation -> Just Pow
  SLogarithm      -> Just Log
  _               -> Nothing

textToUnaryArithOp :: Text -> Maybe UnaryArithOp
textToUnaryArithOp = \case
  SNegation         -> Just Negate
  SSquareRoot       -> Just Sqrt
  SNaturalLogarithm -> Just Ln
  SExponential      -> Just Exp
  SAbsoluteValue    -> Just Abs
  -- explicitly no signum
  _                 -> Nothing

textToComparisonOp :: Text -> Maybe ComparisonOp
textToComparisonOp = \case
  SGreaterThan        -> Just Gt
  SLessThan           -> Just Lt
  SGreaterThanOrEqual -> Just Gte
  SLessThanOrEqual    -> Just Lte
  SEquality           -> Just Eq
  SInequality         -> Just Neq
  _                   -> Nothing

textToEqNeq :: Text -> Maybe EqNeq
textToEqNeq = \case
  SEquality   -> Just Eq'
  SInequality -> Just Neq'
  _           -> Nothing

textToRoundingLikeOp :: Text -> Maybe RoundingLikeOp
textToRoundingLikeOp = \case
  SRound    -> Just Round
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

type TableEnv = TableMap (ColumnMap EType)

data PropCheckEnv = PropCheckEnv
  { _varTys           :: Map VarId QType
  , _tableEnv         :: TableEnv
  , _quantifiedTables :: Set TableName
  -- , _quantifiedColumns :: Set ColumnName
  }

type PropParse = ReaderT (Map Text VarId) (StateT VarId (Either String))
type PropCheck = ReaderT PropCheckEnv (Either String)

type InvariantParse = ReaderT [(Pact.Arg UserType, VarId)] (Either String)

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

  EList' [EAtom' "at", stringLike -> Just objIx, obj]
    -> PreAt objIx <$> expToPreProp obj
  exp@(EList' [EAtom' "at", _, _]) -> throwErrorIn exp
    "Property object access must use a static string or symbol"
  Pact.EObject bindings _parsed -> do
    bindings' <- for bindings $ \(key, body) -> case stringLike key of
      Just key' -> (key',) <$> expToPreProp body
      Nothing   -> throwErrorIn key "static key required"
    pure $ PreLiteralObject $ Map.fromList bindings'

  EList' (EAtom' funName:args) -> PreApp funName <$> traverse expToPreProp args

  EAtom' "abort"   -> pure PreAbort
  EAtom' "success" -> pure PreSuccess
  EAtom' "result"  -> pure PreResult
  EAtom' var       -> mkVar var

  exp -> throwErrorIn exp "expected property"

  where propBindings :: [Exp] -> PropParse [(VarId, Text, QType)]
        propBindings [] = pure []
        -- we require a type annotation
        propBindings (exp@(EAtom _name _qual Nothing _parsed):_exps)
          = throwErrorIn exp
            "type annotation required for all property bindings."
        propBindings (exp@(EAtom name _qual (Just ty) _parsed):exps) = do
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

-- helper view pattern for checking quantifiers
viewQ :: PreProp -> Maybe
  ( VarId -> Text -> QType -> Prop Bool -> PropSpecific Bool
  , VarId
  , Text
  , QType
  , PreProp
  )
viewQ = \case
  PreForall vid name ty' p -> Just (Forall, vid, name, ty', p)
  PreExists vid name ty' p -> Just (Exists, vid, name, ty', p)
  _                        -> Nothing

inferrable :: PreProp -> Bool
inferrable = \case
  -- we can infer all functions (as is typical bidirectionally), except for
  -- some overloaded ones.
  PreApp f _
    | Just _ <- textToArithOp f      -> False
    | Just _ <- textToUnaryArithOp f -> False
    | otherwise                      -> True
  _                                  -> True

inferVar :: VarId -> Text -> (forall a. Prop a) -> PropCheck EProp
inferVar vid name prop = do
  varTy <- view (varTys . at vid)
  case varTy of
    Nothing -> throwErrorT $
      "couldn't find property variable " <> name
    Just (EType varTy')     -> pure (ESimple varTy' prop)
    Just (EObjectTy schema) -> pure (EObject schema prop)
    Just QTable             -> error "Table names cannot be vars"
    Just QColumnOf{}        -> error "Column names cannot be vars"

inferPreProp :: PreProp -> PropCheck EProp
inferPreProp preProp = case preProp of
  -- literals
  PreDecimalLit a -> pure (ESimple TDecimal (PLit a))
  PreIntegerLit a -> pure (ESimple TInt (PLit a))
  PreStringLit a  -> pure (ESimple TStr (PLit (T.unpack a)))
  PreTimeLit a    -> pure (ESimple TTime (PLit a))
  PreBoolLit a    -> pure (ESimple TBool (PLit a))
  PreAbort        -> pure (ESimple TBool (PropSpecific Abort))
  PreSuccess      -> pure (ESimple TBool (PropSpecific Success))

  -- identifiers
  PreResult       -> inferVar 0 "result" (PropSpecific Result)
  PreVar vid name -> inferVar vid name (PureProp (Var vid name))

  -- quantifiers
  (viewQ -> Just (q, vid, name, ty', p)) -> do
    let quantifyTable = case ty' of
          QTable -> Set.insert (TableName (T.unpack name))
          _      -> id
    let modEnv env = env & varTys . at vid  ?~ ty'
                         & quantifiedTables %~ quantifyTable

    ESimple TBool . PropSpecific . q vid name ty'
      <$> local modEnv (checkPreProp TBool p)

  PreAt objIx obj -> do
    obj' <- inferPreProp obj
    case obj' of
      ESimple ty _ -> throwErrorIn preProp $
        "expected object (with key " <> tShow objIx <> ") but found type " <>
        userShow ty
      EObject objSchema@(Schema tyMap) objProp -> case tyMap ^? ix objIx of
        Nothing -> throwErrorIn preProp $ "could not find expected key " <> objIx
        Just ety@(EType ty) -> pure $ ESimple
          ty
          (PAt objSchema (PLit (T.unpack objIx)) objProp ety)
        Just ety@(EObjectTy schemaTy) -> pure $ EObject
          schemaTy
          (PAt objSchema (PLit (T.unpack objIx)) objProp ety)

  PreLiteralObject obj -> do
    obj' <- traverse inferPreProp obj
    let schema = Schema $ fmap existentialType obj'
    pure $ EObject schema $ PureProp $ LiteralObject obj'

  -- applications:
  --
  -- Function types are inferred; arguments are checked.
  PreApp "str-length" [str] ->
    ESimple TInt . PStrLength <$> checkPreProp TStr str

  PreApp "mod" [a, b] -> do
    it <- PNumerical ... ModOp <$> checkPreProp TInt a <*> checkPreProp TInt b
    pure $ ESimple TInt it
  PreApp (textToRoundingLikeOp -> Just op) [a] ->
    ESimple TInt . PNumerical . RoundingLikeOp1 op <$> checkPreProp TDecimal a
  PreApp (textToRoundingLikeOp -> Just op) [a, b] -> do
    it <- RoundingLikeOp2 op <$> checkPreProp TDecimal a <*> checkPreProp TInt b
    pure $ ESimple TDecimal (PNumerical it)
  PreApp "add-time" [a, b] -> do
    a' <- checkPreProp TTime a
    b' <- inferPreProp b
    case b' of
      ESimple TInt     b'' -> pure $ ESimple TTime $ PIntAddTime a' b''
      ESimple TDecimal b'' -> pure $ ESimple TTime $ PDecAddTime a' b''
      _                    -> throwErrorIn b $
        "expected integer or decimal, found " <> userShow (existentialType b')

  PreApp op'@(textToComparisonOp -> Just op) [a, b] -> do
    a' <- inferPreProp a
    b' <- inferPreProp b
    let ret :: (ComparisonOp -> Prop a -> Prop a -> Prop Bool)
            -> Prop a -> Prop a -> PropCheck EProp
        ret c aProp bProp = pure $ ESimple TBool $ c op aProp bProp
        eqNeqMsg :: Text -> Text
        eqNeqMsg nouns = nouns
                      <> " only support equality (=) / inequality (!=) checks"
    case (a', b') of
      (ESimple aTy aProp, ESimple bTy bProp) -> case typeEq aTy bTy of
        Nothing -> typeError preProp aTy bTy
        Just Refl -> case aTy of
          TInt     -> ret (PureProp .... IntegerComparison) aProp bProp
          TDecimal -> ret (PureProp .... DecimalComparison) aProp bProp
          TTime    -> ret (PureProp .... TimeComparison) aProp bProp
          TBool    -> ret (PureProp .... BoolComparison) aProp bProp
          TStr     -> ret (PureProp .... StringComparison) aProp bProp
          TAny     -> throwErrorIn preProp $
            "cannot compare objects of type " <> userShow aTy
          TKeySet  -> case textToEqNeq op' of
            Just eqNeq -> pure $ ESimple TBool $ PKeySetEqNeq eqNeq aProp bProp
            Nothing    -> throwErrorIn preProp $ eqNeqMsg "keysets"
      (EObject _ aProp, EObject _ bProp) -> case textToEqNeq op' of
          Just eqNeq -> pure $ ESimple TBool $ PureProp $ ObjectEqNeq eqNeq aProp bProp
          Nothing    -> throwErrorIn preProp $ eqNeqMsg "objects"
      (_, _) -> throwErrorIn preProp $
        "can't compare primitive types with objects (found " <>
        userShow (existentialType a') <> " and " <>
        userShow (existentialType b') <> ")"

  PreApp op'@(textToLogicalOp -> Just op) args -> do
    ESimple TBool <$> case (op, args) of
      (NotOp, [a])    -> PNot <$> checkPreProp TBool a
      (AndOp, [a, b]) -> PAnd <$> checkPreProp TBool a <*> checkPreProp TBool b
      (OrOp, [a, b])  -> POr  <$> checkPreProp TBool a <*> checkPreProp TBool b
      _               -> throwErrorIn preProp $
        op' <> " applied to wrong number of arguments"

  PreApp "when" [a, b] -> do
    propNotA <- PNot <$> checkPreProp TBool a
    ESimple TBool . POr propNotA <$> checkPreProp TBool b

  --
  -- TODO: should be "table-written"
  --
  PreApp "table-write" [tn] -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    pure $ ESimple TBool (PropSpecific (TableWrite tn'))
  PreApp "table-read" [tn] -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    pure $ ESimple TBool (PropSpecific (TableRead tn'))

  --
  -- NOTE: disabled until implemented on the backend:
  --
  -- (TBool, PreApp "column-written" [PLit tn, PLit cn])
  --   -> pure (ColumnWrite tn cn)
  -- (TBool, PreApp "column-read" [PLit tn, PLit cn])
  --   -> pure (ColumnRead tn cn)

  PreApp "cell-delta" [tn, cn, rk] -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    asum
      [ do
          _   <- expectColumnType tn' cn' TInt
          ESimple TInt . PropSpecific . IntCellDelta tn' cn' <$> checkPreProp TStr rk
      , do
          _   <- expectColumnType tn' cn' TDecimal
          ESimple TDecimal . PropSpecific . DecCellDelta tn' cn' <$> checkPreProp TStr rk
      ]
  PreApp "column-delta" [tn, cn] -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    asum
      [ do
          _   <- expectColumnType tn' cn' TInt
          pure $ ESimple TInt (PropSpecific (IntColumnDelta tn' cn'))
      , do
          _   <- expectColumnType tn' cn' TDecimal
          pure $ ESimple TDecimal (PropSpecific (DecColumnDelta tn' cn'))
      ]
  PreApp "row-read" [tn, rk] -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    ESimple TBool . PropSpecific . RowRead tn' <$> checkPreProp TStr rk
  PreApp "row-read-count" [tn, rk] -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    ESimple TInt . PropSpecific . RowReadCount tn' <$> checkPreProp TStr rk
  --
  -- TODO: should be "row-written"
  --
  PreApp "row-write" [tn, rk] -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    ESimple TBool . PropSpecific . RowWrite tn' <$> checkPreProp TStr rk
  PreApp "row-write-count" [tn, rk] -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    ESimple TInt . PropSpecific . RowWriteCount tn' <$> checkPreProp TStr rk
  PreApp "authorized-by" [PreStringLit ks]
    -> pure $ ESimple TBool (PropSpecific (KsNameAuthorized (KeySetName ks)))
  PreApp "row-enforced" [tn, cn, rk] -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    _   <- expectColumnType tn' cn' TKeySet
    ESimple TBool . PropSpecific . RowEnforced tn' cn' <$> checkPreProp TStr rk

  _ -> throwErrorIn preProp "could not infer type"

checkPreProp :: Type a -> PreProp -> PropCheck (Prop a)
checkPreProp ty preProp
  | inferrable preProp = do
    eprop <- inferPreProp preProp
    case eprop of
      ESimple ty' prop -> case typeEq ty ty' of
        Just Refl -> pure prop
        Nothing   -> typeError preProp ty ty'
      EObject ty' _prop -> typeError preProp ty ty'
  | otherwise = case (ty, preProp) of

  (TStr, PreApp "+" [a, b])
    -> PStrConcat <$> checkPreProp TStr a <*> checkPreProp TStr b
  (TDecimal, PreApp (textToArithOp -> Just op) [a, b]) -> do
    a' <- inferPreProp a
    b' <- inferPreProp b
    case (a', b') of
      (ESimple TDecimal aprop, ESimple TDecimal bprop) ->
        pure $ PNumerical $ DecArithOp op aprop bprop
      (ESimple TDecimal aprop, ESimple TInt bprop) ->
        pure $ PNumerical $ DecIntArithOp op aprop bprop
      (ESimple TInt aprop, ESimple TDecimal bprop) ->
        pure $ PNumerical $ IntDecArithOp op aprop bprop
      (_, _) -> throwErrorIn preProp $
        "unexpected argument types for (+): " <>
        userShow (existentialType a') <> " and " <>
        userShow (existentialType b')
  (TInt, PreApp (textToArithOp -> Just op) [a, b])
    -> PNumerical ... IntArithOp op <$> checkPreProp TInt a <*> checkPreProp TInt b
  (TDecimal, PreApp (textToUnaryArithOp -> Just op) [a])
    -> PNumerical . DecUnaryArithOp op <$> checkPreProp TDecimal a
  (TInt, PreApp (textToUnaryArithOp -> Just op) [a])
    -> PNumerical . IntUnaryArithOp op <$> checkPreProp TInt a

  _ -> throwErrorIn preProp $ "type error: expected type " <> userShow ty

typeError :: (UserShow a, UserShow b) => PreProp -> a -> b -> PropCheck c
typeError preProp a b = throwErrorIn preProp $
  "type error: " <> userShow a <> " vs " <> userShow b

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
  unless (isJust quantified || isJust defined) $
    throwErrorT $ "expected table " <> userShow tn <> " but it isn't in scope"
expectTableExists _ = error "table name must be concrete at this point"

-- Convert an @Exp@ to a @Check@ in an environment where the variables have
-- types.
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

inferProp
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
  -> Either String EProp
inferProp tableEnv' genStart nameEnv idEnv body = do
  preTypedBody <- evalStateT (runReaderT (expToPreProp body) nameEnv) genStart
  let env = PropCheckEnv (coerceQType <$> idEnv) tableEnv' Set.empty
  runReaderT (inferPreProp preTypedBody) env

expToInvariant :: Type a -> Exp -> InvariantParse (Invariant a)
expToInvariant ty exp = case (ty, exp) of
  (_, EAtom' varName) -> do
    schemaTys <- ask
    case find (\(arg, _vid) -> arg ^. aName == varName) schemaTys of
      Just (Pact.Arg _name (TyPrim primTy) _info, vid) -> case (ty, primTy) of
        (TInt,     TyInteger) -> pure (PureInvariant (Var vid varName))
        (TDecimal, TyDecimal) -> pure (PureInvariant (Var vid varName))
        (TTime,    TyTime)    -> pure (PureInvariant (Var vid varName))
        (TStr,     TyString)  -> pure (PureInvariant (Var vid varName))
        (TBool,    TyBool)    -> pure (PureInvariant (Var vid varName))
        (TKeySet,  TyKeySet)  -> pure (PureInvariant (Var vid varName))
        (_,        TyValue)   -> throwErrorIn exp
          "Invariants can't constrain opaque values"
        (_,        _)         -> throwErrorIn exp $
          "found variable " <> varName <> " of type " <> tShow primTy <>
          " where " <> userShow ty <> " was expected"
      _ -> throwErrorT $ "couldn't find column named " <> varName

  (TDecimal, ELiteral (LDecimal d) _) -> pure (ILiteral (mkDecimal d))
  (TInt, ELiteral (LInteger i) _)     -> pure (ILiteral i)
  (TStr, stringLike -> Just s)        -> pure (ILiteral (T.unpack s))
  (TStr, ELiteral (LString _) _)      -> error "impossible (handled by stringLike)"
  (TTime, ELiteral (LTime t) _)       -> pure (ILiteral (mkTime t))
  (TBool, ELiteral (LBool b) _)       -> pure (ILiteral b)
  (_, ELiteral _ _)                   ->
    throwErrorIn exp "literal of unexpected type"

  (TInt, EList' [EAtom' "str-length", str])
    -> PureInvariant . StrLength <$> expToInvariant TStr str
  (TStr, EList' [EAtom' "+", a, b]) -> PureInvariant ... StrConcat
    <$> expToInvariant TStr a <*> expToInvariant TStr b

  (TDecimal, EList' [EAtom' (textToArithOp -> Just op), a, b]) -> asum'
    [ Inj ... DecArithOp    op <$> expToInvariant TDecimal a <*> expToInvariant TDecimal b
    , Inj ... DecIntArithOp op <$> expToInvariant TDecimal a <*> expToInvariant TInt b
    , Inj ... IntDecArithOp op <$> expToInvariant TInt a     <*> expToInvariant TDecimal b
    ] (throwErrorIn exp "unexpected argument types")
  (TInt, EList' [EAtom' (textToArithOp -> Just op), a, b])
    -> Inj ... IntArithOp op <$> expToInvariant TInt a <*> expToInvariant TInt b
  (TDecimal, EList' [EAtom' (textToUnaryArithOp -> Just op), a])
    -> Inj . DecUnaryArithOp op <$> expToInvariant TDecimal a
  (TInt, EList' [EAtom' (textToUnaryArithOp -> Just op), a])
    -> Inj . IntUnaryArithOp op <$> expToInvariant TInt a

  (TBool, EList' [EAtom' op'@(textToComparisonOp -> Just op), a, b]) -> asum'
    [ PureInvariant ... IntegerComparison op
      <$> expToInvariant TInt a     <*> expToInvariant TInt b
    , PureInvariant ... DecimalComparison op
      <$> expToInvariant TDecimal a <*> expToInvariant TDecimal b
    , PureInvariant ... TimeComparison op
      <$> expToInvariant TTime a    <*> expToInvariant TTime b
    , PureInvariant ... BoolComparison op
      <$> expToInvariant TBool a    <*> expToInvariant TBool b
    , PureInvariant ... StringComparison op
      <$> expToInvariant TStr a     <*> expToInvariant TStr b
    , case textToEqNeq op' of
      Just eqNeq -> PureInvariant ... KeySetEqNeq eqNeq
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

  (_, ESymbol {})      -> throwErrorIn exp "illegal invariant form"
  (_, EAtom {})        -> throwErrorIn exp "illegal invariant form"
  (_, EList {})        -> throwErrorIn exp "illegal invariant form"
  (_, Pact.EObject {}) -> throwErrorIn exp "illegal invariant form"
  (_, EBinding {})     -> throwErrorIn exp "illegal invariant form"
