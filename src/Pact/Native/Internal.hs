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
  ,parseMsgKey
  ,bindReduce
  ,enforceGuard
  ,defNative,defGasRNative,defRNative
  ,setTopLevelOnly
  ,foldDefs
  ,funType,funType'
  ,getModule
  ,module Pact.Types.Native
  ,tTyInteger,tTyDecimal,tTyTime,tTyBool
  ,tTyString,tTyValue,tTyKeySet,tTyObject,tTyGuard
  ,colsToList
  ,module Pact.Gas
  ,(<>)
  ,getPactId,enforceGuardDef,guardForModuleCall
  ,findCallingModule
  ) where

import Control.Monad
import Prelude
import Data.Default
import Pact.Eval
import Unsafe.Coerce
import Control.Lens hiding (Fold)
import Data.Aeson
import Control.Arrow
import qualified Data.Aeson.Lens as A
import Bound
import qualified Data.HashMap.Strict as HM
import Pact.Types.Pretty

import Pact.Types.Runtime
import Pact.Types.Native
import Pact.Gas

success :: Functor m => Text -> m a -> m (Term Name)
success = fmap . const . toTerm


colsToList
  :: Eval m [(Info,ColumnId)] -> Term n -> Eval m [(Info,ColumnId)]
colsToList _ (TList cs _ _) = forM cs $ \c -> case c of
    TLitString col -> return (_tInfo c,ColumnId col)
    _ -> evalError (_tInfo c) "read: only Strings/Symbols allowed for col keys"
colsToList argFail _ = argFail


parseMsgKey :: (FromJSON t) => FunApp -> String -> Text -> Eval e t
parseMsgKey i msg key = do
  vm <- firstOf (A.key key) <$> view eeMsgBody
  case vm of
    Nothing -> evalError' i $ "No such key in message: " <> pretty key
    Just v -> case fromJSON v of
                Success t -> return t
                Error e -> evalError' i $ pretty msg <> ": parse failed: " <> pretty e <> ": " <> pretty v


bindReduce :: [(Arg (Term Ref),Term Ref)] -> Scope Int Term Ref -> Info -> (Text -> Maybe (Term Ref)) -> Eval e (Term Name)
bindReduce ps bd bi lkpFun = do
  !(vs :: [(Arg (Term Ref),Term Ref)]) <- forM ps $ mapM $ \var -> do
          var' <- reduce var
          case var' of
            (TLitString s) -> case lkpFun s of
                                Nothing -> evalError bi $ "Bad column in binding: " <> pretty s
                                Just v -> return v
            t -> evalError bi $ "Invalid column identifier in binding: " <> pretty t
  let bd'' = instantiate (resolveArg bi (map snd vs)) bd
  -- NB stack frame here just documents scope, but does not incur gas
  call (StackFrame (pack $ "(bind: " ++ show (map (second abbrev) vs) ++ ")") bi Nothing) $!
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


getModule :: Info -> ModuleName -> Eval e (ModuleDef (Def Ref))
getModule i n = do
  lm <- HM.lookup n <$> use (evalRefs.rsLoadedModules)
  case lm of
    Just m -> return m
    Nothing -> do
      rm <- HM.lookup n <$> view (eeRefStore.rsModules)
      case rm of
        Just ModuleData{..} -> return _mdModule
        Nothing -> evalError i $ "Unable to resolve module " <> pretty n

tTyInteger :: Type n; tTyInteger = TyPrim TyInteger
tTyDecimal :: Type n; tTyDecimal = TyPrim TyDecimal
tTyTime :: Type n; tTyTime = TyPrim TyTime
tTyBool :: Type n; tTyBool = TyPrim TyBool
tTyString :: Type n; tTyString = TyPrim TyString
tTyValue :: Type n; tTyValue = TyPrim TyValue
tTyKeySet :: Type n; tTyKeySet = TyPrim (TyGuard $ Just GTyKeySet)
tTyObject :: Type n -> Type n; tTyObject o = TySchema TyObject o def
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
  GKeySet k -> runPure $ enforceKeySet (_faInfo i) Nothing k
  GKeySetRef n -> enforceKeySetName (_faInfo i) n
  GPact PactGuard{..} -> do
    pid <- getPactId i
    unless (pid == _pgPactId) $
      evalError' i $ "Pact guard failed, intended: " <> pretty _pgPactId <> ", active: " <> pretty pid
  GModule mg@ModuleGuard{..} -> do
    m <- getModule (_faInfo i) _mgModuleName
    case m of
      MDModule Module{..} -> enforceModuleAdmin (_faInfo i) _mGovernance
      MDInterface{} -> evalError' i $ "ModuleGuard not allowed on interface: " <> pretty mg
  GUser UserGuard{..} -> do
    void $ runReadOnly (_faInfo i) $ evalByName _ugPredFun [_ugData] (_faInfo i)

findCallingModule :: Eval e (Maybe ModuleName)
findCallingModule = uses evalCallStack (firstOf (traverse . sfApp . _Just . _1 . faModule . _Just))

-- | Test that first module app found in call stack is specified module,
-- running 'onFound' if true, otherwise requesting module admin.
guardForModuleCall :: Info -> ModuleName -> Eval e () -> Eval e ()
guardForModuleCall i modName onFound = findCallingModule >>= \r -> case r of
    (Just mn) | mn == modName -> onFound
    _ -> do
      md <- getModule i modName
      case md of
        MDModule m -> void $ acquireModuleAdmin i (_mName m) (_mGovernance m)
        MDInterface iface -> evalError i $
          "Internal error, interface found in call stack: " <> pretty iface
