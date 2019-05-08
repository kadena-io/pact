{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Parser from 'Exp' to the property language 'Prop'.
module Pact.Analyze.Parse.Prop
  ( TableEnv
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
import           Control.Lens                 (at, view)
import           Control.Monad.Except         (MonadError, lift)
import           Control.Monad.Reader         (local, runReaderT)
import           Control.Monad.State.Strict   (evalStateT)
import qualified Data.HashMap.Strict          as HM
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Traversable             (for)
import           Data.Type.Equality           ((:~:) (Refl))
import           Prelude                      hiding (exp)

import           Pact.Types.Lang              hiding (KeySet, KeySetName,
                                               PrimType (..), SchemaVar, TList,
                                               TableName, TyObject, Type, TyList)
import           Pact.Types.Pretty

import           Pact.Analyze.Feature         hiding (Doc, Type, Var, ks, obj,
                                               str)
import           Pact.Analyze.Parse.Types
import           Pact.Analyze.PrenexNormalize
import           Pact.Analyze.Types

import           Pact.Analyze.Typecheck       (typecheck)

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
expToPreProp :: Exp Info -> PropParse (Fix PreProp)
expToPreProp = \case
  ELiteral' (LDecimal d) -> pure $ Fix $ PreDecimalLit $ fromPact decimalIso d
  ELiteral' (LInteger i) -> pure $ Fix $ PreIntegerLit i
  ELiteral' (LString s)  -> pure $ Fix $ PreStringLit s
  ELiteral' (LTime t)    -> pure $ Fix $ PreTimeLit $ fromPact timeIso t
  ELiteral' (LBool b)    -> pure $ Fix $ PreBoolLit b
  SquareList elems       -> Fix . PreListLit <$> traverse expToPreProp elems

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
  ParenList [EAtom' SObjectProjection, ix, container] -> do
    ix' <- expToPreProp ix
    container' <- expToPreProp container
    pure $ Fix $ PreAt ix' container'

  -- XXX
  -- ParenList [EAtom' SPropRead, tn, rk, ba] -> PrePropRead
  --   <$> expToPreProp tn
  --   <*> expToPreProp rk
  --   <*> expToPreProp ba
  -- exp@(ParenList [EAtom' SPropRead, _tn, _rk]) -> throwErrorIn exp $
  --   pretty SPropRead <> " must specify a time ('before or 'after). example: " <>
  --   "(= result (read accounts user 'before))"

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
    in Fix . PreLiteralObject <$> go exps

  ParenList (EAtom' funName:args)
    -> Fix . PreApp funName <$> traverse expToPreProp args

  EAtom' STransactionAborts   -> pure $ Fix PreAbort
  EAtom' STransactionSucceeds -> pure $ Fix PreSuccess
  EAtom' SFunctionResult      -> pure $ Fix PreResult
  EAtom' var                  -> do
    mVid <- view (at var)
    pure $ case mVid of
      Just vid -> Fix $ PreVar vid var
      Nothing  -> Fix $ PreGlobalVar var

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
  exp -> throwErrorD $ "in " <> prettyList exp <> ", unexpected binding form"

parseType :: Exp Info -> Maybe QType
parseType = \case
  EAtom' "bool"    -> pure $ EType SBool
  EAtom' "decimal" -> pure $ EType SDecimal
  EAtom' "integer" -> pure $ EType SInteger
  EAtom' "string"  -> pure $ EType SStr
  EAtom' "time"    -> pure $ EType STime
  EAtom' "keyset"  -> pure $ EType SGuard
  EAtom' "guard"   -> pure $ EType SGuard
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
  let env = PropCheckEnv tableEnv' Set.empty Set.empty
        (coerceQType <$> idEnv) preTypedPropDefs consts
  _getEither $ evalStateT (runReaderT (checkPreProp ty preTypedBody) env) (CheckState 0 [])

checkPreProp :: SingTy a -> Fix PreProp -> PropCheck (Prop a)
checkPreProp ty preProp = do
  Some ty' prop <- typecheck preProp
  case singEq ty ty' of
    Nothing -> lift $ lift $ EitherFail $ Left $
      "checkPreProp: mismatched types: " ++ show ty ++ " / " ++ show ty'
    Just Refl -> pure  prop

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
  let env = PropCheckEnv tableEnv' Set.empty Set.empty
        (coerceQType <$> idEnv) preTypedPropDefs consts
  _getEither $ evalStateT (runReaderT (typecheck preTypedBody) env) (CheckState 0 [])

parseToPreProp
  :: Traversable t
  => VarId
  -> Map Text VarId
  -> t (DefinedProperty (Exp Info))
  -> Exp Info
  -> Either String (Fix PreProp, t (DefinedProperty (Fix PreProp)))
parseToPreProp genStart nameEnv propDefs body =
  (`evalStateT` genStart) $ (`runReaderT` nameEnv) $ do
    body'     <- expToPreProp body
    propDefs' <- for propDefs $ \(DefinedProperty args argBody) ->
      DefinedProperty args <$> expToPreProp argBody
    pure (body', propDefs')
