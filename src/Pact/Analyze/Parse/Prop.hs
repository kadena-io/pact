{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

module Pact.Analyze.Parse.Prop
  ( PreProp(..)
  , TableEnv
  , expToCheck
  , expToProp
  , inferProp
  , parseBindings
  ) where

import           Control.Lens                 (at, ix, view, (%~), (&), (.~),
                                               (?~), (^..), (^?))
import           Control.Monad                (unless, when)
import           Control.Monad.Except         (MonadError (throwError))
import           Control.Monad.Reader         (asks, local, runReaderT)
import           Control.Monad.State.Strict   (evalStateT)
import           Data.Foldable                (asum)
import qualified Data.HashMap.Strict          as HM
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (isJust)
import           Data.Semigroup               ((<>))
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
import           Pact.Types.Util              (tShow)

import           Pact.Analyze.Feature         hiding (Type, Var, ks, obj, str)
import           Pact.Analyze.Parse.Types
import           Pact.Analyze.PrenexNormalize
import           Pact.Analyze.Translate
import           Pact.Analyze.Types
import           Pact.Analyze.Util

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
    bindings' <- parseBindings (\name ty -> (, name, ty) <$> genVarId) bindings
    let theseBindingsMap = Map.fromList $
          fmap (\(vid, name, _ty) -> (name, vid)) bindings'
    body'     <- local (Map.union theseBindingsMap) (expToPreProp body)
    pure $ foldr
      (\(vid, name, ty) accum -> q vid name ty accum)
      body'
      bindings'

  EList' [EAtom' SObjectProjection, stringLike -> Just objIx, obj]
    -> PreAt objIx <$> expToPreProp obj
  exp@(EList' [EAtom' SObjectProjection, _, _]) -> throwErrorIn exp
    "Property object access must use a static string or symbol"
  Pact.EObject bindings _parsed -> do
    bindings' <- for bindings $ \(key, body) -> case stringLike key of
      Just key' -> (key',) <$> expToPreProp body
      Nothing   -> throwErrorIn key "static key required"
    pure $ PreLiteralObject $ Map.fromList bindings'

  EList' (EAtom' funName:args) -> PreApp funName <$> traverse expToPreProp args

  EAtom' STransactionAborts   -> pure PreAbort
  EAtom' STransactionSucceeds -> pure PreSuccess
  EAtom' SFunctionResult      -> pure PreResult
  EAtom' var                  -> do
    mVid <- view (at var)
    pure $ case mVid of
      Just vid -> PreVar vid var
      Nothing  -> PropDefVar var

  exp -> throwErrorIn exp "expected property"

-- | Parse a set of bindings like '(x:integer y:string)'
parseBindings
  :: MonadError String m
  => (Text -> QType -> m binding) -> [Exp] -> m [binding]
parseBindings mkBinding = \case
  [] -> pure []
  -- we require a type annotation
  exp@(EAtom _name _qual Nothing _parsed):_exps -> throwErrorIn exp
    "type annotation required for all property bindings."
  exp@(EAtom name _qual (Just ty) _parsed):exps -> do
    -- This is challenging because `ty : Pact.Type TypeName`, but
    -- `maybeTranslateType` handles `Pact.Type UserType`. We use `const
    -- Nothing` to punt on user types.
    nameTy <- case maybeTranslateType' (const Nothing) ty of
      Just ty' -> mkBinding name ty'
      Nothing  -> throwErrorIn exp
        "currently objects can't be quantified in properties (issue 139)"
    (nameTy:) <$> parseBindings mkBinding exps
  exp -> throwErrorT $
    "in " <> userShowList exp <> ", unexpected binding form"

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
    | Just _ <- toOp arithOpP f      -> False
    | Just _ <- toOp unaryArithOpP f -> False
    | otherwise                      -> True
  _                                  -> True

inferVar :: VarId -> Text -> (forall a. Prop a) -> PropCheck EProp
inferVar vid name prop = do
  varTy <- view (varTys . at vid)
  case varTy of
    Nothing -> throwErrorT $ "couldn't find property variable " <> name
    Just (EType varTy')     -> pure (ESimple varTy' prop)
    Just (EObjectTy schema) -> pure (EObject schema prop)
    Just QTable             -> error "Table names cannot be vars"
    Just QColumnOf{}        -> error "Column names cannot be vars"

--
-- NOTE: because we have a lot of cases here and we are using pattern synonyms
-- in conjunction with view patterns for feature symbols (see
-- Pact.Analyze.Feature), we use our symbols as values rather than as patterns
-- (by using a variable @s@ in conjunction with an equality check in a pattern
-- guard @s == SStringLength@) to avoid triggering GHC's max-pmcheck-iterations
-- limit for this function.
--
-- See GHC ticket 11822 for more details:
-- https://ghc.haskell.org/trac/ghc/ticket/11822
--
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
  PreResult       -> inferVar 0 SFunctionResult (PropSpecific Result)
  PreVar vid name -> inferVar vid name (CoreProp (Var vid name))
  PropDefVar name -> do
    defn        <- view $ localVars    . at name
    definedProp <- view $ definedProps . at name
    case defn of
      Nothing    -> case definedProp of
        -- clear this definition so it can't call itself
        Just (DefinedProperty [] definedProp') ->
          local (definedProps . at name .~ Nothing) $
            inferPreProp definedProp'
        Just _ -> throwErrorT $
          name <> " expects arguments but wasn't provided any"
        Nothing -> throwErrorT $ "couldn't find property variable " <> name
      Just defn' -> pure defn'

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
    pure $ EObject schema $ CoreProp $ LiteralObject obj'

  -- applications:
  --
  -- Function types are inferred; arguments are checked.
  PreApp s [str] | s == SStringLength ->
    ESimple TInt . PStrLength <$> checkPreProp TStr str

  PreApp s [a, b] | s == SModulus -> do
    it <- PNumerical ... ModOp <$> checkPreProp TInt a <*> checkPreProp TInt b
    pure $ ESimple TInt it
  PreApp (toOp roundingLikeOpP -> Just op) [a] ->
    ESimple TInt . PNumerical . RoundingLikeOp1 op <$> checkPreProp TDecimal a
  PreApp (toOp roundingLikeOpP -> Just op) [a, b] -> do
    it <- RoundingLikeOp2 op <$> checkPreProp TDecimal a <*> checkPreProp TInt b
    pure $ ESimple TDecimal (PNumerical it)
  PreApp s [a, b] | s == STemporalAddition -> do
    a' <- checkPreProp TTime a
    b' <- inferPreProp b
    case b' of
      ESimple TInt     b'' -> pure $ ESimple TTime $ PIntAddTime a' b''
      ESimple TDecimal b'' -> pure $ ESimple TTime $ PDecAddTime a' b''
      _                    -> throwErrorIn b $
        "expected integer or decimal, found " <> userShow (existentialType b')

  PreApp op'@(toOp comparisonOpP -> Just op) [a, b] -> do
    a' <- inferPreProp a
    b' <- inferPreProp b
    let ret :: (ComparisonOp -> Prop a -> Prop a -> Prop Bool)
            -> Prop a -> Prop a -> PropCheck EProp
        ret c aProp bProp = pure $ ESimple TBool $ c op aProp bProp
        eqNeqMsg :: Text -> Text
        eqNeqMsg nouns = nouns <> " only support equality (" <> SEquality <>
          ") / inequality (" <> SInequality <> ") checks"
    case (a', b') of
      (ESimple aTy aProp, ESimple bTy bProp) -> case typeEq aTy bTy of
        Nothing -> typeError preProp aTy bTy
        Just Refl -> case aTy of
          TInt     -> ret (CoreProp .... IntegerComparison) aProp bProp
          TDecimal -> ret (CoreProp .... DecimalComparison) aProp bProp
          TTime    -> ret (CoreProp .... TimeComparison) aProp bProp
          TBool    -> ret (CoreProp .... BoolComparison) aProp bProp
          TStr     -> ret (CoreProp .... StringComparison) aProp bProp
          TAny     -> throwErrorIn preProp $
            "cannot compare objects of type " <> userShow aTy
          TKeySet  -> case toOp eqNeqP op' of
            Just eqNeq -> pure $ ESimple TBool $ PKeySetEqNeq eqNeq aProp bProp
            Nothing    -> throwErrorIn preProp $ eqNeqMsg "keysets"
      (EObject _ aProp, EObject _ bProp) -> case toOp eqNeqP op' of
          Just eqNeq -> pure $ ESimple TBool $ CoreProp $ ObjectEqNeq eqNeq aProp bProp
          Nothing    -> throwErrorIn preProp $ eqNeqMsg "objects"
      (_, _) -> throwErrorIn preProp $
        "can't compare primitive types with objects (found " <>
        userShow (existentialType a') <> " and " <>
        userShow (existentialType b') <> ")"

  PreApp op'@(toOp logicalOpP -> Just op) args -> do
    ESimple TBool <$> case (op, args) of
      (NotOp, [a])    -> PNot <$> checkPreProp TBool a
      (AndOp, [a, b]) -> PAnd <$> checkPreProp TBool a <*> checkPreProp TBool b
      (OrOp, [a, b])  -> POr  <$> checkPreProp TBool a <*> checkPreProp TBool b
      _               -> throwErrorIn preProp $
        op' <> " applied to wrong number of arguments"

  PreApp s [a, b] | s == SLogicalImplication -> do
    propNotA <- PNot <$> checkPreProp TBool a
    ESimple TBool . POr propNotA <$> checkPreProp TBool b

  PreApp s [tn] | s == STableWritten -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    pure $ ESimple TBool (PropSpecific (TableWrite tn'))
  PreApp s [tn] | s == STableRead -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    pure $ ESimple TBool (PropSpecific (TableRead tn'))

  --
  -- NOTE: disabled until implemented on the backend:
  --
  -- (TBool, PreApp SColumWritten [PLit tn, PLit cn])
  --   -> pure (ColumnWrite tn cn)
  -- (TBool, PreApp SColumnRead [PLit tn, PLit cn])
  --   -> pure (ColumnRead tn cn)

  PreApp s [tn, cn, rk] | s == SCellDelta -> do
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
  PreApp s [tn, cn] | s == SColumnDelta -> do
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
  PreApp s [tn, rk] | s == SRowRead -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    ESimple TBool . PropSpecific . RowRead tn' <$> checkPreProp TStr rk
  PreApp s [tn, rk] | s == SRowReadCount -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    ESimple TInt . PropSpecific . RowReadCount tn' <$> checkPreProp TStr rk
  PreApp s [tn, rk] | s == SRowWritten -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    ESimple TBool . PropSpecific . RowWrite tn' <$> checkPreProp TStr rk
  PreApp s [tn, rk] | s == SRowWriteCount -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    ESimple TInt . PropSpecific . RowWriteCount tn' <$> checkPreProp TStr rk
  PreApp s [PreStringLit ks] | s == SAuthorizedBy ->
    pure $ ESimple TBool (PropSpecific (KsNameAuthorized (KeySetName ks)))
  PreApp s [tn, cn, rk] | s == SRowEnforced -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    _   <- expectColumnType tn' cn' TKeySet
    ESimple TBool . PropSpecific . RowEnforced tn' cn' <$> checkPreProp TStr rk

  -- inline property definitions
  PreApp fName args -> do
    defn <- view $ definedProps . at fName
    case defn of
      Nothing -> throwErrorIn preProp $ "couldn't find property named " <> fName
      Just (DefinedProperty argTys body) -> do
        when (length args /= length argTys) $
          throwErrorIn preProp "wrong number of arguments"
        propArgs <- for (zip args argTys) $ \(arg, (name, EType ty)) ->
          (name,) . ESimple ty <$> checkPreProp ty arg

        -- inline the function, removing it from `definedProps` so it can't
        -- recursively call itself.
        local (localVars %~ HM.union (HM.fromList propArgs)) $
          local (definedProps . at fName .~ Nothing) $
            inferPreProp body

  _ -> vacuousMatch
    "PreForall / PreExists are handled via the viewQ view pattern"

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

  (TStr, PreApp SStringConcatenation [a, b])
    -> PStrConcat <$> checkPreProp TStr a <*> checkPreProp TStr b
  (TDecimal, PreApp opSym@(toOp arithOpP -> Just op) [a, b]) -> do
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
        "unexpected argument types for (" <> opSym <> "): " <>
        userShow (existentialType a') <> " and " <>
        userShow (existentialType b')
  (TInt, PreApp (toOp arithOpP -> Just op) [a, b])
    -> PNumerical ... IntArithOp op <$> checkPreProp TInt a <*> checkPreProp TInt b
  (TDecimal, PreApp (toOp unaryArithOpP -> Just op) [a])
    -> PNumerical . DecUnaryArithOp op <$> checkPreProp TDecimal a
  (TInt, PreApp (toOp unaryArithOpP -> Just op) [a])
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
  -> HM.HashMap Text (DefinedProperty Exp)
  -- ^ Defined props in the environment
  -> Exp
  -- ^ Exp to convert
  -> Either String Check
expToCheck tableEnv' genStart nameEnv idEnv propDefs body =
  PropertyHolds . prenexConvert
    <$> expToProp tableEnv' genStart nameEnv idEnv propDefs TBool body

expToProp
  :: TableEnv
  -- ^ Tables and schemas in scope
  -> VarId
  -- ^ ID to start issuing from
  -> Map Text VarId
  -- ^ Environment mapping names to var IDs
  -> Map VarId EType
  -- ^ Environment mapping var IDs to their types
  -> HM.HashMap Text (DefinedProperty Exp)
  -- ^ Defined props in the environment
  -> Type a
  -> Exp
  -- ^ Exp to convert
  -> Either String (Prop a)
expToProp tableEnv' genStart nameEnv idEnv propDefs ty body = do
  (preTypedBody, preTypedPropDefs)
    <- parseToPreProp genStart nameEnv propDefs body
  let env = PropCheckEnv (coerceQType <$> idEnv) tableEnv' Set.empty
        preTypedPropDefs HM.empty
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
  -> HM.HashMap Text (DefinedProperty Exp)
  -- ^ Defined props in the environment
  -> Exp
  -- ^ Exp to convert
  -> Either String EProp
inferProp tableEnv' genStart nameEnv idEnv propDefs body = do
  (preTypedBody, preTypedPropDefs)
    <- parseToPreProp genStart nameEnv propDefs body
  let env = PropCheckEnv (coerceQType <$> idEnv) tableEnv' Set.empty
        preTypedPropDefs HM.empty
  runReaderT (inferPreProp preTypedBody) env

parseToPreProp
  :: Traversable t
  => VarId
  -> Map Text VarId
  -> t (DefinedProperty Exp)
  -> Exp
  -> Either String (PreProp, t (DefinedProperty PreProp))
parseToPreProp genStart nameEnv propDefs body =
  (`evalStateT` genStart) $ (`runReaderT` nameEnv) $ do
    body'     <- expToPreProp body
    propDefs' <- for propDefs $ \(DefinedProperty args argBody) ->
      DefinedProperty args <$> expToPreProp argBody
    pure (body', propDefs')
