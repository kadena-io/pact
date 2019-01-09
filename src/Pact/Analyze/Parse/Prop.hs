{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Pact.Analyze.Parse.Prop
  ( PreProp(..)
  , TableEnv
  , expToCheck
  , expToProp
  , inferProp
  , parseBindings
  ) where

-- Note [Inlining]:
--
-- The ASTs that the property analysis system receives from Pact are inlined,
-- so the analysis system has never had a need to represent functions. Though
-- we never had a need for functions in terms, we now have them, with
-- user-defined functions, in properties. The most obvious place to evaluate
-- functions would be in `eval`, however, this complicates the evaluation
-- system in a way that doesn't make sense for invariants and terms. That's why
-- we've chosen to inline functions at checking/inference -time.
--
-- When we hit the site of a defined property application (in 'inferPreProp'),
-- we check each argument before inlining them into the body. Note this is in a
-- bidirectional style, so the application is inferred while each argument is
-- checked.


import           Control.Applicative
import           Control.Lens                 (at, toListOf, view, (%~), (&),
                                               (.~), (?~))
import qualified Control.Lens                 as Lens
import           Control.Monad                (unless, when)
import           Control.Monad.Except         (MonadError (throwError))
import           Control.Monad.Reader         (asks, local, runReaderT)
import           Control.Monad.State.Strict   (evalStateT)
import           Data.Foldable                (asum)
import qualified Data.HashMap.Strict          as HM
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (isJust)
import           Data.Proxy                   (Proxy)
import qualified Data.Set                     as Set
import           Data.String                  (fromString)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Traversable             (for)
import           Data.Type.Equality           ((:~:) (Refl))
import           GHC.TypeLits
import           Prelude                      hiding (exp)

import           Pact.Types.Lang              hiding (KeySet, KeySetName,
                                               PrimType (..), SchemaVar, TList,
                                               TableName, Type)
import           Pact.Types.Util              (tShow)

import           Pact.Analyze.Feature         hiding (Type, Var, ks, obj, str)
import           Pact.Analyze.Parse.Types
import           Pact.Analyze.PrenexNormalize
import           Pact.Analyze.Types
import           Pact.Analyze.Util


parseTableName :: PreProp -> PropCheck (Prop TyTableName)
parseTableName (PreGlobalVar var) = pure (fromString (T.unpack var))
parseTableName (PreVar vid name) = do
  varTy <- view (varTys . at vid)
  case varTy of
    Just QTable -> pure $ CoreProp $ Var vid name
    _           -> throwError $ T.unpack $ "invalid table name: " <> name
parseTableName bad = throwError $ T.unpack $
  "invalid table name: " <> userShow bad

parseColumnName :: PreProp -> PropCheck (Prop TyColumnName)
parseColumnName (PreStringLit str) = pure (fromString (T.unpack str))
parseColumnName (PreVar vid name) = do
  varTy <- view (varTys . at vid)
  case varTy of
    Just QColumnOf{} -> pure $ CoreProp $ Var vid name
    _                -> throwError $ T.unpack $
      "invalid column name: " <> name
parseColumnName bad = throwError $ T.unpack $
  "invalid column name: " <> userShow bad

parseBeforeAfter :: PreProp -> PropCheck BeforeOrAfter
parseBeforeAfter (PreStringLit str)
  | str == "before" = pure Before
  | str == "after"  = pure After
parseBeforeAfter other = throwErrorIn other "expected 'before / 'after"

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
expToPreProp :: Exp Info -> PropParse PreProp
expToPreProp = \case
  ELiteral' (LDecimal d) -> pure (PreDecimalLit (fromPact decimalIso d))
  ELiteral' (LInteger i) -> pure (PreIntegerLit i)
  ELiteral' (LString s)  -> pure (PreStringLit s)
  ELiteral' (LTime t)    -> pure (PreTimeLit (fromPact timeIso t))
  ELiteral' (LBool b)    -> pure (PreBoolLit b)
  SquareList elems       -> PreListLit <$> traverse expToPreProp elems

  ParenList [EAtom' (textToQuantifier -> Just q), ParenList bindings, body] -> do
    bindings' <- parseBindings (\name ty -> (, name, ty) <$> genVarId) bindings
    let theseBindingsMap = Map.fromList $
          fmap (\(vid, name, _ty) -> (name, vid)) bindings'
    body'     <- local (Map.union theseBindingsMap) (expToPreProp body)
    pure $ foldr
      (\(vid, name, ty) accum -> q vid name ty accum)
      body'
      bindings'

  -- Note: this handles both object and list projection:
  ParenList [EAtom' SObjectProjection, ix, container]
    -> PreAt <$> expToPreProp ix <*> expToPreProp container

  ParenList [EAtom' SPropRead, tn, rk, ba] -> PrePropRead
    <$> expToPreProp tn
    <*> expToPreProp rk
    <*> expToPreProp ba
  exp@(ParenList [EAtom' SPropRead, _tn, _rk]) -> throwErrorIn exp $
    SPropRead <> " must specify a time ('before or 'after). example: " <>
    "(= result (read accounts user 'before))"

  exp@(ParenList [EAtom' SObjectProjection, _, _]) -> throwErrorIn exp
    "Property object access must use a static string or symbol"
  exp@(BraceList exps) ->
    let go (keyExp : Colon' : valExp : rest) = Map.insert
          <$> case keyExp of
            ELiteral' (LString key) -> pure key
            _                       -> throwErrorIn keyExp "static key required"
          <*> expToPreProp valExp
          <*> case rest of
            []               -> pure Map.empty
            CommaExp : rest' -> go rest'
            _                -> throwErrorIn keyExp "unexpected token"
        go _ = throwErrorIn exp "cannot parse as object"
    in PreLiteralObject <$> go exps

  ParenList (EAtom' funName:args) -> PreApp funName <$> traverse expToPreProp args

  EAtom' STransactionAborts   -> pure PreAbort
  EAtom' STransactionSucceeds -> pure PreSuccess
  EAtom' SFunctionResult      -> pure PreResult
  EAtom' var                  -> do
    mVid <- view (at var)
    pure $ case mVid of
      Just vid -> PreVar vid var
      Nothing  -> PreGlobalVar var

  exp -> throwErrorIn exp "expected property"

-- | Parse a set of bindings like '(x:integer y:string)'
parseBindings
  :: MonadError String m
  => (Text -> QType -> m binding) -> [Exp Info] -> m [binding]
parseBindings mkBinding = \case
  [] -> pure []
  -- we require a type annotation
  exp@(EAtom' name):Colon':ty:exps -> do
    -- This is challenging because `ty : Pact.Type TypeName`, but
    -- `maybeTranslateType` handles `Pact.Type UserType`. We use `const
    -- Nothing` to punt on user types.
    nameTy <- case parseType ty of
      Just ty' -> mkBinding name ty'
      Nothing  -> throwErrorIn exp
        "currently objects can't be quantified in properties (issue 139)"
    (nameTy:) <$> parseBindings mkBinding exps
  exp@(EAtom _):_exps -> throwErrorIn exp
    "type annotation required for all property bindings."
  exp -> throwErrorT $ "in " <> userShow exp <> ", unexpected binding form"

parseType :: Exp Info -> Maybe QType
parseType = \case
  EAtom' "bool"    -> pure $ EType SBool
  EAtom' "decimal" -> pure $ EType SDecimal
  EAtom' "integer" -> pure $ EType SInteger
  EAtom' "string"  -> pure $ EType SStr
  EAtom' "time"    -> pure $ EType STime
  EAtom' "keyset"  -> pure $ EType SKeySet
  EAtom' "*"       -> pure $ EType SAny

  EAtom' "table"   -> pure QTable

  -- TODO
  EAtom' "value"   -> Nothing

  -- TODO
  -- # user schema type
  BraceList _      -> Nothing
  ParenList [EAtom' "column-of", EAtom' tabName]
    -- TODO: look up quantified table names
    -> pure $ QColumnOf $ TableName $ T.unpack tabName
  SquareList [ty]  -> case parseType ty of
    Just (EType ty') -> Just $ EType $ SList ty'
    _                -> Nothing
  SquareList _     -> Nothing

  -- TODO
  -- # object schema type
  -- # table schema type
  _ -> Nothing

-- helper view pattern for checking quantifiers
viewQ :: PreProp -> Maybe
  ( VarId -> Text -> QType -> Prop 'TyBool -> PropSpecific 'TyBool
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
    Just (EType varTy') -> pure $ Existential varTy' prop
      -- (\Refl -> pure (ESimple varTy' prop))
      -- (\Refl -> pure (EList varTy' prop))
      -- (\Refl -> throwErrorT
      --   "impossible: objects cannot occur here -- please report this as a bug")
    -- Just (EObjectTy schema) -> pure (EObject schema prop)
    Just QTable             -> error "Table names cannot be vars"
    Just QColumnOf{}        -> error "Column names cannot be vars"

-- Note: there is a very similar @mkLiteralObject@ in @Analyze.Translate@.
-- These could probably be combined.
mkLiteralObject :: [(Text, PreProp)] -> PropCheck (Existential (Core Prop))
mkLiteralObject [] = pure $ Existential (SObject SNil) $
  LiteralObject (SObject SNil) (Object NilOf)
mkLiteralObject ((name, preProp) : tups) = do
  tups' <- mkLiteralObject tups
  case tups' of
    Existential (SObject objTy) (LiteralObject _ (Object objProp)) -> do
      eProp <- inferPreProp preProp
      case eProp of
        Existential ty prop -> case someSymbolVal (T.unpack name) of
          SomeSymbol (_proxy :: Proxy k) -> withTypeable ty $ withSing ty $ pure $
            let sym    = SSymbol @k
                objTy' = SObject (SCons sym ty objTy)
            in Existential objTy' $
                 LiteralObject objTy' $
                   Object $ ConsOf sym (Column ty prop) objProp
    Existential _ _ -> throwErrorT $ "unexpected non-literal object: " <> tShow tups'

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
  PreDecimalLit a -> pure (Existential SDecimal (Lit' a))
  PreIntegerLit a -> pure (Existential SInteger (Lit' a))
  PreStringLit a  -> pure (Existential SStr (TextLit a))
  PreTimeLit a    -> pure (Existential STime (Lit' a))
  PreBoolLit a    -> pure (Existential SBool (Lit' a))
  PreAbort        -> pure (Existential SBool (PropSpecific Abort))
  PreSuccess      -> pure (Existential SBool (PropSpecific Success))

  PreListLit as   -> do
    as' <- traverse inferPreProp as
    Existential listTy litList <- maybe
      (throwErrorT
        ("unable to make list of a single type from " <> userShow as'))
      pure
      $ mkLiteralList as'

    pure $ Existential listTy $ CoreProp litList

  -- identifiers
  PreResult         -> inferVar 0 SFunctionResult (PropSpecific Result)
  PreVar vid name   -> inferVar vid name (CoreProp (Var vid name))
  PreGlobalVar name -> do
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
        quantifyColumn = case ty' of
          QColumnOf{} -> Set.insert (ColumnName (T.unpack name))
          _           -> id

    let modEnv env = env & varTys . at vid   ?~ ty'
                         & quantifiedTables  %~ quantifyTable
                         & quantifiedColumns %~ quantifyColumn

    Existential SBool . PropSpecific . q vid name ty'
      <$> local modEnv (checkPreProp SBool p)

  PreAt ix container -> do
    ix'        <- inferPreProp ix
    container' <- inferPreProp container
    case (ix', container') of
      (Existential SInteger ix'', Existential (SList ty) lst)
        -> pure $ Existential ty $ CoreProp $ ListAt ty ix'' lst

--       error "TODO"
--       (Existential SStr (TextLit ix''), Existential objty@(SObject tyMap) objProp)
--         -> case tyMap ^? Lens.ix ix'' of
--           Nothing -> throwErrorIn preProp $
--             "could not find expected key " <> ix''
--           Just ety@(EType ty) -> Existential ty $ PObjAt objty (TextLit ix'') objProp ety

      (_, Existential ty _) -> throwErrorIn preProp $
        "expected object or list (with key " <> tShow ix' <>
        ") but found type " <> userShow ty

  PrePropRead tn rk ba -> do
    tn' <- parseTableName tn
    case tn' of
      StrLit litTn -> do
        rk' <- checkPreProp SStr rk
        ba' <- parseBeforeAfter ba
        cm  <- view $ tableEnv . at (TableName litTn)
        case cm of
          Just cm' -> case columnMapToSchema cm' of
            EType objTy@(SObject _schema) -> pure $
              Existential objTy $ PropSpecific $ PropRead objTy ba' tn' rk'
            _ -> error "TODO"
          Nothing -> throwErrorT $ "couldn't find table " <> tShow litTn
      _ -> throwErrorT $ "table name (" <> userShow tn <> ") must be a literal"

  PreLiteralObject obj -> do
    obj' <- mkLiteralObject (Map.toList obj)
    case obj' of
      Existential schema obj'' -> pure $ Existential schema $ CoreProp obj''
      -- $ LiteralObject schema obj'
    -- obj' <- traverse inferPreProp obj
    -- let schema = undefined -- Schema $ fmap existentialType obj'
    -- pure $ Existential schema $ CoreProp $ LiteralObject schema obj'

  -- applications:
  --
  -- Function types are inferred; arguments are checked.
  PreApp s [arg] | s == SStringLength -> do
    arg' <- inferPreProp arg
    case arg' of
      Existential SStr str
        -> pure $ Existential SInteger $ CoreProp $ StrLength str
      Existential (SList ty) lst
        -> pure $ Existential SInteger $ CoreProp $ ListLength ty lst
      _ -> throwErrorIn preProp "expected string or list argument to length"

  PreApp s [a, b] | s == SModulus -> do
    it <- PNumerical ... ModOp <$> checkPreProp SInteger a <*> checkPreProp SInteger b
    pure $ Existential SInteger it
  PreApp (toOp roundingLikeOpP -> Just op) [a] ->
    Existential SInteger . PNumerical . RoundingLikeOp1 op <$> checkPreProp SDecimal a
  PreApp (toOp roundingLikeOpP -> Just op) [a, b] -> do
    it <- RoundingLikeOp2 op <$> checkPreProp SDecimal a <*> checkPreProp SInteger b
    pure $ Existential SDecimal (PNumerical it)
  PreApp s [a, b] | s == STemporalAddition -> do
    a' <- checkPreProp STime a
    b' <- inferPreProp b
    case b' of
      Existential SInteger     b'' -> pure $ Existential STime $ PIntAddTime a' b''
      Existential SDecimal b'' -> pure $ Existential STime $ PDecAddTime a' b''
      _                    -> throwErrorIn b $
        "expected integer or decimal, found " <> userShow (existentialType b')

  PreApp op'@(toOp comparisonOpP -> Just op) [a, b] -> do
    a''@(Existential aTy a') <- inferPreProp a
    b''@(Existential bTy b') <- inferPreProp b
    let eqNeqMsg :: Text -> Text
        eqNeqMsg nouns = nouns <> " only support equality (" <> SEquality <>
          ") / inequality (" <> SInequality <> ") checks"

    -- special case for an empty list on either side
    case (a'', b'') of
      -- cast ([] :: [*]) to any other list type
      (Existential (SList SAny) (CoreProp (LiteralList _ [])), Existential (SList ty) prop)
        | Just eqNeq <- toOp eqNeqP op'
        -> pure $ Existential SBool $ CoreProp $
          ListEqNeq ty eqNeq (CoreProp (LiteralList ty [])) prop

      (Existential (SList ty) prop, Existential (SList SAny) (CoreProp (LiteralList _ [])))
        | Just eqNeq <- toOp eqNeqP op'
        -> pure $ Existential SBool $ CoreProp $
          ListEqNeq ty eqNeq (CoreProp (LiteralList ty [])) prop

      _ -> case singEq aTy bTy of
        Nothing   -> typeError preProp aTy bTy
        Just Refl -> if
          -- TODO: enable
          | False -> throwErrorIn preProp $ eqNeqMsg "lists"
          | False -> throwErrorIn preProp $ eqNeqMsg "objects"
          | False -> throwErrorIn preProp $ eqNeqMsg "keysets"
          | True -> pure $ Existential SBool $ CoreProp $ Comparison aTy op a' b'

  PreApp op'@(toOp logicalOpP -> Just op) args ->
    Existential SBool <$> case (op, args) of
      (NotOp, [a])    -> PNot <$> checkPreProp SBool a
      (AndOp, [a, b]) -> PAnd <$> checkPreProp SBool a <*> checkPreProp SBool b
      (OrOp, [a, b])  -> POr  <$> checkPreProp SBool a <*> checkPreProp SBool b
      _               -> throwErrorIn preProp $
        op' <> " applied to wrong number of arguments"

  PreApp s [a, b] | s == SLogicalImplication -> do
    propNotA <- PNot <$> checkPreProp SBool a
    Existential SBool . POr propNotA <$> checkPreProp SBool b

  PreApp s [tn] | s == STableWritten -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    pure $ Existential SBool (PropSpecific (TableWrite tn'))
  PreApp s [tn] | s == STableRead -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    pure $ Existential SBool (PropSpecific (TableRead tn'))

  PreApp s [tn, cn] | s == SColumnWritten -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    pure $ Existential SBool $ PropSpecific $ ColumnWritten tn' cn'
  PreApp s [tn, cn] | s == SColumnRead -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    pure $ Existential SBool $ PropSpecific $ ColumnRead tn' cn'

  PreApp s [tn, cn, rk] | s == SCellDelta -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    asum
      [ do
          _   <- expectColumnType tn' cn' SInteger
          Existential SInteger . PropSpecific . IntCellDelta tn' cn' <$> checkPreProp SStr rk
      , do
          _   <- expectColumnType tn' cn' SDecimal
          Existential SDecimal . PropSpecific . DecCellDelta tn' cn' <$> checkPreProp SStr rk
      ] <|> throwErrorIn preProp "couldn't find column of appropriate (integer / decimal) type"
  PreApp s [tn, cn] | s == SColumnDelta -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    asum
      [ do
          _   <- expectColumnType tn' cn' SInteger
          pure $ Existential SInteger (PropSpecific (IntColumnDelta tn' cn'))
      , do
          _   <- expectColumnType tn' cn' SDecimal
          pure $ Existential SDecimal (PropSpecific (DecColumnDelta tn' cn'))
      ] <|> throwErrorIn preProp "couldn't find column of appropriate (integer / decimal) type"
  PreApp s [tn, rk] | s == SRowRead -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    Existential SBool . PropSpecific . RowRead tn' <$> checkPreProp SStr rk
  PreApp s [tn, rk] | s == SRowReadCount -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    Existential SInteger . PropSpecific . RowReadCount tn' <$> checkPreProp SStr rk
  PreApp s [tn, rk] | s == SRowWritten -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    Existential SBool . PropSpecific . RowWrite tn' <$> checkPreProp SStr rk
  PreApp s [tn, rk] | s == SRowWriteCount -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    Existential SInteger . PropSpecific . RowWriteCount tn' <$> checkPreProp SStr rk
  PreApp s [tn, rk, beforeAfter] | s == SRowExists -> do
    tn' <- parseTableName tn
    _   <- expectTableExists tn'
    (Existential SBool . PropSpecific) ... RowExists tn'
      <$> checkPreProp SStr rk
      <*> parseBeforeAfter beforeAfter
  PreApp s [PreStringLit ks] | s == SAuthorizedBy ->
    pure $ Existential SBool (PropSpecific (KsNameAuthorized (KeySetName ks)))
  PreApp s [tn, cn, rk] | s == SRowEnforced -> do
    tn' <- parseTableName tn
    cn' <- parseColumnName cn
    _   <- expectTableExists tn'
    _   <- expectColumnType tn' cn' SKeySet
    Existential SBool . PropSpecific . RowEnforced tn' cn' <$> checkPreProp SStr rk

  PreApp (toOp arithOpP -> Just _) args -> asum
    [ Existential SInteger <$> checkPreProp SInteger preProp
    , Existential SDecimal <$> checkPreProp SDecimal preProp
    , Existential SStr     <$> checkPreProp SStr     preProp -- (string concat)
    ] <|> case args of
      [a, b] -> do
        a' <- inferPreProp a
        b' <- inferPreProp b
        case (a', b') of
          (Existential aTy@(SList aTy') aProp, Existential bTy bProp) ->
            case singEq aTy bTy of
              Nothing ->
                throwErrorIn preProp "can only concat lists of the same type"
              Just Refl -> pure $
                Existential aTy $ CoreProp $ ListConcat aTy' aProp bProp
          _ -> throwErrorIn preProp "can't infer the types of the arguments to +"
      _ -> throwErrorIn preProp "can't infer the types of the arguments to +"

  PreApp s [lst] | s == SReverse -> do
    Existential (SList ty) lst' <- inferPreProp lst
    pure $ Existential (SList ty) $ CoreProp $ ListReverse ty lst'

  PreApp s [lst] | s == SSort -> do
    Existential (SList ty) lst' <- inferPreProp lst
    pure $ Existential (SList ty) $ CoreProp $ ListSort ty lst'

  -- inline property definitions. see note [Inlining].
  PreApp fName args -> do
    defn <- view $ definedProps . at fName
    case defn of
      Nothing -> throwErrorIn preProp $ "couldn't find property named " <> fName
      Just (DefinedProperty argTys body) -> do
        when (length args /= length argTys) $
          throwErrorIn preProp "wrong number of arguments"
        propArgs <- for (zip args argTys) $ \case
          (arg, (name, EType ty)) -> do
            prop <- checkPreProp ty arg
            pure (name, Existential ty prop)
          _ -> throwErrorIn preProp "Internal pattern match failure."

        -- inline the function, removing it from `definedProps` so it can't
        -- recursively call itself.
        local (localVars %~ HM.union (HM.fromList propArgs)) $
          local (definedProps . at fName .~ Nothing) $
            inferPreProp body

  x -> vacuousMatch $
    "PreForall / PreExists are handled via the viewQ view pattern: " ++ show x

checkPreProp :: SingTy a -> PreProp -> PropCheck (Prop a)
checkPreProp ty preProp
  | inferrable preProp = do
    eprop <- inferPreProp preProp
    case eprop of
      Existential ty' prop -> case singEq ty ty' of
        Just Refl -> pure prop
        Nothing   -> typeError preProp ty ty'
  | otherwise = case (ty, preProp) of

  (SStr, PreApp SConcatenation [a, b])
    -> PStrConcat <$> checkPreProp SStr a <*> checkPreProp SStr b
  (SDecimal, PreApp opSym@(toOp arithOpP -> Just op) [a, b]) -> do
    a' <- inferPreProp a
    b' <- inferPreProp b
    case (a', b') of
      (Existential SDecimal aprop, Existential SDecimal bprop) ->
        pure $ PNumerical $ DecArithOp op aprop bprop
      (Existential SDecimal aprop, Existential SInteger bprop) ->
        pure $ PNumerical $ DecIntArithOp op aprop bprop
      (Existential SInteger aprop, Existential SDecimal bprop) ->
        pure $ PNumerical $ IntDecArithOp op aprop bprop
      (_, _) -> throwErrorIn preProp $
        "unexpected argument types for (" <> opSym <> "): " <>
        userShow (existentialType a') <> " and " <>
        userShow (existentialType b')
  (SInteger, PreApp (toOp arithOpP -> Just op) [a, b])
    -> PNumerical ... IntArithOp op <$> checkPreProp SInteger a <*> checkPreProp SInteger b
  (SDecimal, PreApp (toOp unaryArithOpP -> Just op) [a])
    -> PNumerical . DecUnaryArithOp op <$> checkPreProp SDecimal a
  (SInteger, PreApp (toOp unaryArithOpP -> Just op) [a])
    -> PNumerical . IntUnaryArithOp op <$> checkPreProp SInteger a

  _ -> throwErrorIn preProp $ "type error: expected type " <> userShow ty

typeError :: (UserShow a, UserShow b) => PreProp -> a -> b -> PropCheck c
typeError preProp a b = throwErrorIn preProp $
  "type error: " <> userShow a <> " vs " <> userShow b

expectColumnType
  :: Prop TyTableName -> Prop TyColumnName -> SingTy a -> PropCheck ()
expectColumnType (TextLit tn) (TextLit cn) expectedTy = do
  tys <- asks $ toListOf $
      tableEnv
    . Lens.ix (TableName (T.unpack tn))
    . Lens.ix (ColumnName (T.unpack cn))
  case tys of
    [EType foundTy] -> case singEq foundTy expectedTy of
      Nothing   -> throwErrorT $
        "expected column " <> userShow cn <> " in table " <> userShow tn <>
        " to have type " <> userShow expectedTy <> ", instead found " <>
        userShow foundTy
      Just Refl -> pure ()
    _ -> throwErrorT $
      "didn't find expected column " <> userShow cn <> " in table " <> userShow tn
expectColumnType _ _ _
  -- TODO(joel): make this better
  = error "table and column names must be concrete at this point"

expectTableExists :: Prop TyTableName -> PropCheck ()
expectTableExists (TextLit tn) = do
  let tn' = TableName (T.unpack tn)
  quantified <- view $ quantifiedTables . at tn'
  defined    <- view $ tableEnv . at tn'
  unless (isJust quantified || isJust defined) $
    throwErrorT $ "expected table " <> userShow tn <> " but it isn't in scope"
expectTableExists (PVar vid name) = do
  ty <- view (varTys . at vid)
  case ty of
    Nothing     -> throwErrorT $ "unable to look up variable " <> name <> " (expected table)"
    Just QTable -> pure ()
    _           -> throwErrorT $ "expected " <> name <> " to be a table"
expectTableExists tn = throwError $ "table name must be concrete at this point: " ++ showTm tn

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
  -> HM.HashMap Text EProp
  -- ^ Environment mapping names to constants
  -> HM.HashMap Text (DefinedProperty (Exp Info))
  -- ^ Defined props in the environment
  -> Exp Info
  -- ^ Exp to convert
  -> Either String Check
expToCheck tableEnv' genStart nameEnv idEnv consts propDefs body =
  PropertyHolds . prenexConvert
    <$> expToProp tableEnv' genStart nameEnv idEnv consts propDefs SBool body

expToProp
  :: TableEnv
  -- ^ Tables and schemas in scope
  -> VarId
  -- ^ ID to start issuing from
  -> Map Text VarId
  -- ^ Environment mapping names to var IDs
  -> Map VarId EType
  -- ^ Environment mapping var IDs to their types
  -> HM.HashMap Text EProp
  -- ^ Environment mapping names to constants
  -> HM.HashMap Text (DefinedProperty (Exp Info))
  -- ^ Defined props in the environment
  -> SingTy a
  -> Exp Info
  -- ^ Exp to convert
  -> Either String (Prop a)
expToProp tableEnv' genStart nameEnv idEnv consts propDefs ty body = do
  (preTypedBody, preTypedPropDefs)
    <- parseToPreProp genStart nameEnv propDefs body
  let env = PropCheckEnv (coerceQType <$> idEnv) tableEnv' Set.empty Set.empty
        preTypedPropDefs consts
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
  -> HM.HashMap Text EProp
  -- ^ Environment mapping names to constants
  -> HM.HashMap Text (DefinedProperty (Exp Info))
  -- ^ Defined props in the environment
  -> Exp Info
  -- ^ Exp to convert
  -> Either String EProp
inferProp tableEnv' genStart nameEnv idEnv consts propDefs body = do
  (preTypedBody, preTypedPropDefs)
    <- parseToPreProp genStart nameEnv propDefs body
  let env = PropCheckEnv (coerceQType <$> idEnv) tableEnv' Set.empty Set.empty
        preTypedPropDefs consts
  runReaderT (inferPreProp preTypedBody) env

parseToPreProp
  :: Traversable t
  => VarId
  -> Map Text VarId
  -> t (DefinedProperty (Exp Info))
  -> Exp Info
  -> Either String (PreProp, t (DefinedProperty PreProp))
parseToPreProp genStart nameEnv propDefs body =
  (`evalStateT` genStart) $ (`runReaderT` nameEnv) $ do
    body'     <- expToPreProp body
    propDefs' <- for propDefs $ \(DefinedProperty args argBody) ->
      DefinedProperty args <$> expToPreProp argBody
    pure (body', propDefs')
