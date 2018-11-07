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
  ,defNative,defGasRNative,defRNative
  ,setTopLevelOnly
  ,foldDefs
  ,funType,funType'
  ,getModule
  ,module Pact.Types.Native
  ,tTyInteger,tTyDecimal,tTyTime,tTyBool,tTyString,tTyValue,tTyKeySet,tTyObject
  ,colsToList
  ,module Pact.Gas
  ,(<>)
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
    Nothing -> evalError' i $ "No such key in message: " ++ show key
    Just v -> case fromJSON v of
                Success t -> return t
                Error e -> evalError' i $ msg ++ ": parse failed: " ++ e ++ ": " ++ show v


bindReduce :: [(Arg (Term Ref),Term Ref)] -> Scope Int Term Ref -> Info -> (Text -> Maybe (Term Ref)) -> Eval e (Term Name)
bindReduce ps bd bi lkpFun = do
  !(vs :: [(Arg (Term Ref),Term Ref)]) <- forM ps $ mapM $ \var -> do
          var' <- reduce var
          case var' of
            (TLitString s) -> case lkpFun s of
                                Nothing -> evalError bi $ "Bad column in binding: " ++ unpack s
                                Just v -> return v
            t -> evalError bi $ "Invalid column identifier in binding: " ++ show t
  let bd'' = instantiate (resolveArg bi (map snd vs)) bd
  -- NB stack frame here just documents scope, but does not incur gas
  call (StackFrame (pack $ "(bind: " ++ show (map (second abbrev) vs) ++ ")") bi Nothing) $!
    ((0,) <$> reduceBody bd'')

setTopLevelOnly :: NativeDef -> NativeDef
setTopLevelOnly = set (_2 . tNativeTopLevelOnly) True

-- | Specify a 'NativeFun'
defNative :: NativeDefName -> NativeFun e -> FunTypes (Term Name) -> Text -> NativeDef
defNative n fun ftype docs =
  (n, TNative n (NativeDFun n (unsafeCoerce fun)) ftype docs False def)

-- | Specify a 'GasRNativeFun'
defGasRNative :: NativeDefName -> GasRNativeFun e -> FunTypes (Term Name) -> Text -> NativeDef
defGasRNative name fun = defNative name (reduced fun)
    where reduced f fi as = preGas fi as >>= \(g,as') -> f g fi as'

-- | Specify a 'RNativeFun'
defRNative :: NativeDefName -> RNativeFun e -> FunTypes (Term Name) -> Text -> NativeDef
defRNative name fun = defNative name (reduced fun)
    where reduced f fi as = preGas fi as >>= \(g,as') -> (g,) <$> f fi as'

foldDefs :: Monad m => [m a] -> m [a]
foldDefs = foldM (\r d -> d >>= \d' -> return (d':r)) []

funType :: Type n -> [(Text,Type n)] -> FunTypes n
funType t as = funTypes $ funType' t as


funType' :: Type n -> [(Text,Type n)] -> FunType n
funType' t as = FunType (map (\(s,ty) -> Arg s ty def) as) t


getModule :: Info -> ModuleName -> Eval e Module
getModule i n = do
  lm <- HM.lookup n <$> use (evalRefs.rsLoadedModules)
  case lm of
    Just m -> return m
    Nothing -> do
      rm <- HM.lookup n <$> view (eeRefStore.rsModules)
      case rm of
        Just ModuleData{..} -> return _mdModule
        Nothing -> evalError i $ "Unable to resolve module " ++ show n

tTyInteger :: Type n; tTyInteger = TyPrim TyInteger
tTyDecimal :: Type n; tTyDecimal = TyPrim TyDecimal
tTyTime :: Type n; tTyTime = TyPrim TyTime
tTyBool :: Type n; tTyBool = TyPrim TyBool
tTyString :: Type n; tTyString = TyPrim TyString
tTyValue :: Type n; tTyValue = TyPrim TyValue
tTyKeySet :: Type n; tTyKeySet = TyPrim TyKeySet
tTyObject :: Type n -> Type n; tTyObject o = TySchema TyObject o
