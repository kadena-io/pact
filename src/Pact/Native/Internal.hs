{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Pact.Native.Internal
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Internal functions for built-ins.
--
module Pact.Native.Internal
  (success
  ,parseMsgKey,parseMsgKey'
  ,bindReduce
  ,enforceGuard
  ,defNative,defGasRNative,defRNative
  ,setTopLevelOnly
  ,foldDefs
  ,funType,funType'
  ,module Pact.Types.Native
  ,tTyInteger,tTyDecimal,tTyTime,tTyBool
  ,tTyString,tTyKeySet,tTyObject,tTyObjectAny,tTyGuard
  ,colsToList
  ,module Pact.Gas
  ,(<>)
  ,getPactId,enforceGuardDef,guardForModuleCall
  ,findCallingModule
  ,getCallingModule
  ,getModule
  ,provenanceOf
  ,enforceYield
  ) where

import Bound
import Control.Lens hiding (Fold)
import Control.Monad
import Data.Aeson hiding (Object)
import qualified Data.Aeson.Lens as A
import Data.Default
import qualified Data.Vector as V
import Data.Text (Text, pack)

import Unsafe.Coerce

import Pact.Eval
import Pact.Gas
import Pact.Types.Native
import Pact.Types.Pretty
import Pact.Types.Runtime


success :: Functor m => Text -> m a -> m (Term Name)
success = fmap . const . toTerm


colsToList
  :: Eval m [(Info,FieldKey)] -> Term n -> Eval m [(Info,FieldKey)]
colsToList _ (TList cs _ _) = forM (V.toList cs) $ \c -> case c of
    TLitString col -> return (_tInfo c,FieldKey col)
    _ -> evalError (_tInfo c) "read: only Strings/Symbols allowed for col keys"
colsToList argFail _ = argFail


parseMsgKey :: (FromJSON t) => FunApp -> String -> Text -> Eval e t
parseMsgKey f s t = parseMsgKey' f s (Just t)

parseMsgKey' :: (FromJSON t) => FunApp -> String -> (Maybe Text) -> Eval e t
parseMsgKey' i msg key = do
  b <- view eeMsgBody
  let go v = case fromJSON v of
        Success t -> return t
        Error e -> evalError' i $ prettyString msg <> ": parse failed: "
                   <> prettyString e <> ": " <> pretty v
  case key of
    Nothing -> go b
    Just k -> case preview (A.key k) b of
      Nothing -> evalError' i $ "No such key in message: " <> pretty k
      Just v -> go v


bindReduce :: [BindPair (Term Ref)] -> Scope Int Term Ref -> Info ->
              (Text -> Maybe (Term Name)) -> Eval e (Term Name)
bindReduce ps bd bi lkpFun = do
  !(vs :: [BindPair (Term Ref)]) <- forM ps $ \(BindPair a k) -> do
          var' <- reduce k
          case var' of
            (TLitString s) ->
              case lkpFun s of
                Nothing -> evalError bi $ "Bad column in binding: " <> pretty s
                Just v -> return (BindPair a (liftTerm v))
            t -> evalError bi $ "Invalid column identifier in binding: " <> pretty t
  let bd'' = instantiate (resolveArg bi (map _bpVal vs)) bd
  -- NB stack frame here just documents scope, but does not incur gas
  call (StackFrame (pack $ "(bind: " ++ show (map (fmap abbrev) vs) ++ ")") bi Nothing) $!
    ((0,) <$> reduceBody bd'')

setTopLevelOnly :: NativeDef -> NativeDef
setTopLevelOnly = set (_2 . tNativeTopLevelOnly) True

-- | Specify a 'NativeFun'
defNative :: NativeDefName -> NativeFun e -> FunTypes (Term Name) -> [Example] -> Text -> NativeDef
defNative n fun ftype examples docs =
  (n, TNative n (NativeDFun n (unsafeCoerce fun)) ftype examples docs False def)

-- | Specify a 'GasRNativeFun'
defGasRNative :: NativeDefName -> GasRNativeFun e -> FunTypes (Term Name) -> [Example] -> Text -> NativeDef
defGasRNative name fun = defNative name (reduced fun)
    where reduced f fi as = preGas fi as >>= \(g,as') -> f g fi as'

-- | Specify a 'RNativeFun'
defRNative :: NativeDefName -> RNativeFun e -> FunTypes (Term Name) -> [Example] -> Text -> NativeDef
defRNative name fun = defNative name (reduced fun)
    where reduced f fi as = preGas fi as >>= \(g,as') -> (g,) <$> f fi as'

foldDefs :: Monad m => [m a] -> m [a]
foldDefs = foldM (\r d -> d >>= \d' -> return (d':r)) []

funType :: Type n -> [(Text,Type n)] -> FunTypes n
funType t as = funTypes $ funType' t as


funType' :: Type n -> [(Text,Type n)] -> FunType n
funType' t as = FunType (map (\(s,ty) -> Arg s ty def) as) t

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
    resolveErr = evalError' i $
      "Unable to resolve current calling module"

    isModule md = case _mdModule md of
      MDModule m -> return m
      MDInterface n -> evalError' i
        $ "Internal error: getCallingModule: called from interface"
        <> pretty (_interfaceName n)

-- | See if some entity was called by a module
--
calledByModule :: Module n -> Eval e Bool
calledByModule Module{..} =
  maybe False (const True) <$> searchCallStackApps forModule
  where
    forModule :: FunApp -> Maybe ()
    forModule FunApp{..} | _faModule == Just _mName = Just ()
                         | otherwise = Nothing

tTyInteger :: Type n; tTyInteger = TyPrim TyInteger
tTyDecimal :: Type n; tTyDecimal = TyPrim TyDecimal
tTyTime :: Type n; tTyTime = TyPrim TyTime
tTyBool :: Type n; tTyBool = TyPrim TyBool
tTyString :: Type n; tTyString = TyPrim TyString
tTyKeySet :: Type n; tTyKeySet = TyPrim (TyGuard $ Just GTyKeySet)
tTyObject :: Type n -> Type n; tTyObject o = TySchema TyObject o def
tTyObjectAny :: Type n; tTyObjectAny = tTyObject TyAny
tTyGuard :: Maybe GuardType -> Type n; tTyGuard gt = TyPrim (TyGuard gt)

getPactId :: FunApp -> Eval e PactId
getPactId i = use evalPactExec >>= \pe -> case pe of
  Nothing -> evalError' i "pact-id: not in pact execution"
  Just PactExec{..} -> return _pePactId

enforceGuardDef :: NativeDefName -> NativeDef
enforceGuardDef dn =
  defRNative dn enforceGuard'
  (funType tTyBool [("guard",tTyGuard Nothing)] <>
   funType tTyBool [("keysetname",tTyString)])
  [ LitExample $ "(" <> asString dn <> " 'admin-keyset)"
  , LitExample $ "(" <> asString dn <> " row-guard)"
  ]
  "Execute GUARD, or defined keyset KEYSETNAME, to enforce desired predicate logic."
  where
    enforceGuard' :: RNativeFun e
    enforceGuard' i as = case as of
      [TGuard g _] -> enforceGuard i g >> return (toTerm True)
      [TLitString k] -> enforceGuard i (GKeySetRef (KeySetName k)) >> return (toTerm True)
      _ -> argsError i as

enforceGuard :: FunApp -> Guard -> Eval e ()
enforceGuard i g = case g of
  GKeySet k -> runSysOnly $ enforceKeySet (_faInfo i) Nothing k
  GKeySetRef n -> enforceKeySetName (_faInfo i) n
  GPact PactGuard{..} -> do
    pid <- getPactId i
    unless (pid == _pgPactId) $
      evalError' i $ "Pact guard failed, intended: " <> pretty _pgPactId <> ", active: " <> pretty pid
  GModule mg@ModuleGuard{..} -> do
    md <- _mdModule <$> getModule (_faInfo i) _mgModuleName
    case md of
      MDModule m@Module{..} -> calledByModule m >>= \r ->
        if r then
          return ()
        else
          enforceModuleAdmin (_faInfo i) _mGovernance
      MDInterface{} -> evalError' i $ "ModuleGuard not allowed on interface: " <> pretty mg
  GUser UserGuard{..} ->
    void $ runSysOnly $ evalByName _ugPredFun [TObject _ugData def] (_faInfo i)

-- | Test that first module app found in call stack is specified module,
-- running 'onFound' if true, otherwise requesting module admin.
guardForModuleCall :: Info -> ModuleName -> Eval e () -> Eval e ()
guardForModuleCall i modName onFound = findCallingModule >>= \r -> case r of
    (Just mn) | mn == modName -> onFound
    _ -> do
      md <- _mdModule <$> getModule i modName
      case md of
        MDModule m -> void $ acquireModuleAdmin i (_mName m) (_mGovernance m)
        MDInterface iface -> evalError i $
          "Internal error, interface found in call stack: " <> pretty iface

-- | Construct a 'Yield' endorsement with a user-supplied
-- 'PactId', as opposed to introspecting on the env info
-- to retrieve it.
--
provenanceOf
  :: FunApp
  -> ChainId
  -- ^ target chain id
  -> Eval e (Maybe Provenance)
provenanceOf fa tid =
  Just . Provenance tid . _mHash <$> getCallingModule fa

-- | Enforce that 'Yield' provenance matches env data
-- and fail otherwise.
--
enforceYield :: FunApp -> Yield -> Eval e Yield
enforceYield fa y = case _yProvenance y of
  Nothing -> return y
  Just p -> do
    m <- getCallingModule fa
    cid <- view $ eePublicData . pdPublicMeta . pmChainId
    let p' = Provenance cid (_mHash m)

    unless (p == p') $
      evalError' fa "enforceYield: yield provenance does not match"

    return y
