{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      :  Pact.Runtime.Utils
-- Copyright   :  (C) 2020 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Runtime utilities: module lookup, stack introspection, events.
--

module Pact.Runtime.Utils
  ( lookupModule
  , searchCallStackApps
  , calledByModule
  , getModule
  , findCallingModule
  , getCallingModule
  , emitEvent
  , emitReservedEvent
  , stripTermInfo
  ) where

import Bound
import Control.Lens
import Control.Monad
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Pact.Gas
import Pact.Types.Runtime
import Pact.Types.PactValue
import Pact.Types.Pretty

-- Strip all term info and native examples.
stripTermInfo :: Term Name -> Term Name
stripTermInfo = stripTerm' stripNameInfo
  where
  stripTerm' :: forall n. (n -> n) -> Term n -> Term n
  stripTerm' f = \case
    TModule defn body _info ->
      TModule
        (stripMDefInfo f defn)
        (stripScopeInfo f body)
        def
    TList li lt _info ->
      TList
        (stripTerm' f <$> li)
        (stripTypeInfo f lt)
        def
    TDef defn _info ->
      TDef
        (stripDefInfo f defn)
        def
    TNative n fun funtypes _exs _docs tlo _info ->
      TNative n fun
        (stripFunTypeInfo f <$> funtypes)
        mempty
        mempty
        tlo
        def
    TConst arg mname cval meta _info ->
      TConst
        (stripArgInfo f arg)
        mname
        (stripTerm' f <$> cval)
        (stripMetaInfo meta)
        def
    TApp app _info ->
      TApp (stripAppInfo f app) def
    TVar v _info ->
      TVar (f v) def
    TBinding bps body btyp _info ->
      TBinding
        (stripBindPair f <$> bps)
        (stripScopeInfo f body)
        (stripTypeInfo f <$> btyp)
        def
    TLam lam _info ->
      TLam (stripLamInfo f lam) def
    TObject obj _info ->
      TObject (stripObjectInfo f obj) def
    TSchema tn mn meta args _info ->
      TSchema tn mn
        (stripMetaInfo meta)
        (stripArgInfo f <$> args)
        def
    TLiteral lit _info ->
      TLiteral lit def
    TGuard g _info ->
      TGuard (stripTerm' f <$> g) def
    TUse u _info ->
      TUse (stripUseInfo u) def
    TStep step meta _info ->
      TStep
        (stripStepInfo f step)
        (stripMetaInfo meta)
        def
    TModRef mr _info ->
      TModRef (stripModRefInfo mr) def
    TTable tn mn hs typ meta _info ->
      TTable tn mn hs (stripTypeInfo f typ) (stripMetaInfo meta) def
    TDynamic e1 e2 _info ->
      TDynamic
        (stripTerm' f e1)
        (stripTerm' f e2)
        def
  stripScopeInfo f s =
    Scope $ stripTerm' stripVars (unscope s)
    where
    stripVars = \case
      F a -> F (stripTerm' f a)
      B b -> B b
  stripMDefInfo f = \case
    MDModule (Module mname mgov mmeta code hs mhs ifaces imports) ->
      MDModule $
        Module
          mname
          (stripTerm' f <$> mgov)
          (stripMetaInfo mmeta)
          code
          hs
          mhs
          ifaces
          (stripUseInfo <$> imports)
    MDInterface (Interface iname ic meta imports) ->
      MDInterface (Interface iname ic (stripMetaInfo meta) (stripUseInfo <$> imports))
  stripTypeInfo f = \case
    TyAny -> TyAny
    TyVar v -> TyVar v
    TyPrim p -> TyPrim p
    TyList t -> TyList (stripTypeInfo f t)
    TySchema s st sp ->
      TySchema s (stripTypeInfo f st) sp
    TyFun funType -> TyFun $ stripFunTypeInfo f funType
    TyUser v -> TyUser (stripTerm' f v)
    TyModule v -> TyModule (fmap (stripTerm' f) <$> v)
  stripArgInfo f (Arg an argtyp _info) =
    Arg an (stripTypeInfo f argtyp) def
  stripMetaInfo (Meta docs model) =
    Meta docs (fmap def <$> model)
  stripAppInfo f (App af args _info) =
    App (stripTerm' f af) (stripTerm' f <$> args) def
  stripStepInfo f = \case
    Step en exec rb _info ->
      Step (stripTerm' f <$> en) (stripTerm' f exec) (stripTerm' f <$> rb) def
  stripModRefInfo (ModRef mn ms _info) =
    ModRef mn ms def
  stripUseInfo u = u {_uInfo=def}
  stripDefInfo f (Def dn mn dt ftyp body meta dmeta _info) =
    Def dn mn dt
      (stripFunTypeInfo f ftyp)
      (stripScopeInfo f body)
      (stripMetaInfo meta)
      (fmap (stripTerm' f) <$> dmeta)
      def
  stripLamInfo f (Lam arg ty body _info) =
    Lam arg
      (stripFunTypeInfo f ty)
      (stripScopeInfo f body)
      def
  stripObjectInfo f (Object obj typ ko _info) =
    Object (stripTerm' f <$> obj) (stripTypeInfo f typ) ko def
  stripFunTypeInfo f (FunType args ret) =
    FunType (stripArgInfo f <$> args) (stripTypeInfo f ret)
  stripNameInfo = \case
    QName qn -> QName $ qn{ _qnInfo = def}
    Name bn -> Name $ bn{ _bnInfo = def}
    DName dn -> DName $ dn {_dynInfo = def}
  stripBindPair f (BindPair arg n) =
    BindPair (stripArgInfo f arg) (stripTerm' f n)

-- | Lookup module in state or database with exact match on 'ModuleName'.
lookupModule :: HasInfo i => i -> ModuleName -> Eval e (Maybe (ModuleData Ref))
lookupModule i mn = do
  loaded <- preuse $ evalRefs . rsLoadedModules . ix mn
  case loaded of
    Just (m,_) -> return $ Just m
    Nothing -> do
      stored <- readRow (getInfo i) Modules mn
      case stored of
        Just mdStored -> do
          _ <- computeGas (Left ((getInfo i), "lookup module")) $ case (_mdModule mdStored) of
            MDModule m -> GPostRead (ReadModule (_mName m) (_mCode m))
            MDInterface int -> GPostRead (ReadInterface (_interfaceName int) (_interfaceCode int))
          natives <- view $ eeRefStore . rsNatives
          let natLookup (NativeDefName n) = case HM.lookup (Name (BareName n def)) natives of
                Just (Direct t) -> Just t
                _ -> Nothing
          case traverse (traverse (fromPersistDirect natLookup)) mdStored of
            Right md -> do
              evalRefs . rsLoadedModules %= HM.insert mn (md,False)
              return $ Just md
            Left e -> evalError' i $ "Internal error: module restore failed: " <> pretty e
        Nothing -> return Nothing


-- | Search up through call stack apps to find the first `Just a`
searchCallStackApps :: (FunApp -> Maybe a) -> Eval e (Maybe a)
searchCallStackApps f = uses evalCallStack $
  preview (traverse . sfApp . _Just . _1 . to f . _Just)

-- | See if some entity was called by a module
--
calledByModule :: Module n -> Eval e Bool
calledByModule Module{..} =
  maybe False (const True) <$> searchCallStackApps forModule
  where
    forModule :: FunApp -> Maybe ()
    forModule FunApp{..} | _faModule == Just _mName = Just ()
                         | otherwise = Nothing


-- | Lookup a module and fail if not found.
getModule :: HasInfo i => i -> ModuleName -> Eval e (ModuleData Ref)
getModule i mn = lookupModule i mn >>= \r -> case r of
  Just m -> return m
  Nothing -> evalError' i $ "Unable to resolve module " <> pretty mn

-- | Look up the name of the most current module in the stack
--
findCallingModule :: Eval e (Maybe ModuleName)
findCallingModule =
  preuse $ evalCallStack . traverse . sfApp . _Just . _1 . faModule . _Just


-- | Retrieve current calling module data or fail if not found
--
getCallingModule :: HasInfo i => i -> Eval e (Module (Def Ref))
getCallingModule i = maybe resolveErr ((=<<) isModule . getModule i) =<< findCallingModule
  where
    resolveErr = evalError' i
      "Unable to resolve current calling module"

    isModule md = case _mdModule md of
      MDModule m -> return m
      MDInterface n -> evalError' i
        $ "Internal error: getCallingModule: called from interface"
        <> pretty (_interfaceName n)

-- | Emit event for calling module.
emitEvent :: HasInfo i => i -> QualifiedName -> [PactValue] -> Eval e ()
emitEvent i qn@QualifiedName{..} params = unlessExecutionFlagSet FlagDisablePactEvents $ do
  callMod <- getCallingModule i
  unless (_mName callMod == _qnQual) $ evalError' i $
    "emitEvent: event '" <> pretty qn <>
    "' does not match emitting module: " <> pretty (_mName callMod)
  emitEventUnsafe qn params (_mHash callMod)

-- | Emits an event in the root "pact" namespace using supplied module hash.
emitReservedEvent :: Text -> [PactValue] -> ModuleHash -> Eval e ()
emitReservedEvent name params mhash = unlessExecutionFlagSet FlagDisablePactEvents $
  emitEventUnsafe (QualifiedName "pact" name def) params mhash

-- | Internal function. Crappy list append is OK as transaction events are usually single digit.
emitEventUnsafe :: QualifiedName -> [PactValue] -> ModuleHash -> Eval e ()
emitEventUnsafe QualifiedName{..} params mh = do
  evalEvents %= (++ [PactEvent _qnName params _qnQual mh])
