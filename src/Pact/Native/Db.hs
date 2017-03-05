{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Pact.Native.Db
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Builtins for database access.
--

module Pact.Native.Db
    (dbDefs)
    where

import Control.Monad
import Prelude hiding (exp)
import Bound
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Default
import Control.Arrow hiding (app)
import Control.Lens hiding (from,to,(.=))
import Data.Aeson (toJSON,object,(.=))
import Pact.Eval

import Pact.Types.Runtime
import Pact.Native.Internal



dbDefs :: NativeModule
dbDefs =
  let writeArgs = funType tTyString [("table",tableTy),("key",tTyString),("object",rowTy)]
      writeDocs s ex = "Write entry in TABLE for KEY of OBJECT column data" <> s <> "`$" <> ex <> "`"
      rt = mkSchemaVar "row"
      tableTy = TySchema TyTable rt
      rowTy = TySchema TyObject rt
      bindTy = TySchema TyBinding rt
      a = mkTyVar "a" []
  in ("Database",
    [defRNative "create-table" createTable'
     (funType tTyString [("table",tableTy)])
     "Create table TABLE. `$(create-table accounts)`"

    ,defNative (specialForm WithRead) withRead
     (funType a [("table",tableTy),("key",tTyString),("bindings",bindTy)])
     "Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements.\
     \`$(with-read 'accounts id { \"balance\":= bal, \"ccy\":= ccy }\n \
     \  (format \"Balance for {} is {} {}\" id bal ccy))`"

    ,defNative (specialForm WithDefaultRead) withDefaultRead
     (funType a
      [("table",tableTy),("key",tTyString),("defaults",rowTy),("bindings",bindTy)])
     "Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements. \
     \If row not found, read columns from DEFAULTS, an object with matching key names. \
     \`$(with-default-read 'accounts id { \"balance\": 0, \"ccy\": \"USD\" } { \"balance\":= bal, \"ccy\":= ccy }\n \
     \  (format \"Balance for {} is {} {}\" id bal ccy))`"

    ,defRNative "read" read'
     (funType rowTy [("table",tableTy),("key",tTyString)] <>
      funType rowTy [("table",tableTy),("key",tTyString),("columns",TyList tTyString)])
     "Read row from TABLE for KEY returning database record object, or just COLUMNS if specified. \
     \`$(read 'accounts id ['balance 'ccy])`"

    ,defRNative "keys" keys'
     (funType (TyList tTyString) [("table",tableTy)])
     "Return all keys in TABLE. `$(keys 'accounts)`"

    ,defRNative "txids" txids'
     (funType (TyList tTyInteger) [("table",tableTy),("txid",tTyInteger)])
     "Return all txid values greater than or equal to TXID in TABLE. `$(txids 'accounts 123849535)`"

    ,defRNative "write" (write Write) writeArgs
     (writeDocs "." "(write 'accounts { \"balance\": 100.0 })")
    ,defRNative "insert" (write Insert) writeArgs
     (writeDocs ", failing if data already exists for KEY."
     "(insert 'accounts { \"balance\": 0.0, \"note\": \"Created account.\" })")
    ,defRNative "update" (write Update) writeArgs
     (writeDocs ", failing if data does not exist for KEY."
      "(update 'accounts { \"balance\": (+ bal amount), \"change\": amount, \"note\": \"credit\" })")

    ,defRNative "txlog" txlog
     (funType (TyList tTyValue) [("table",tableTy),("txid",tTyInteger)])
      "Return all updates to TABLE performed in transaction TXID. `$(txlog 'accounts 123485945)`"
    ,defRNative "describe-table" descTable
     (funType tTyValue [("table",tTyString)]) "Get metadata for TABLE"
    ,defRNative "describe-keyset" descKeySet
     (funType tTyValue [("keyset",tTyString)]) "Get metadata for KEYSET"
    ,defRNative "describe-module" descModule
     (funType tTyValue [("module",tTyString)]) "Get metadata for MODULE"
    ])

descTable :: RNativeFun e
descTable i [TLitString t] =
    toTerm . (\(m,k) -> object ["name" .= t, "module" .= m, "keyset" .= k]) <$>
    getUserTableInfo (_faInfo i) (TableName t)
descTable i as = argsError i as

descKeySet :: RNativeFun e
descKeySet i [TLitString t] = do
  r <- readRow (_faInfo i) KeySets (KeySetName t)
  case r of
    Just v -> return (toTerm (toJSON v))
    Nothing -> evalError' i $ "Keyset not found: " ++ show t
descKeySet i as = argsError i as

descModule :: RNativeFun e
descModule i [TLitString t] = do
  mods <- view (eeRefStore.rsModules.at (ModuleName t))
  case mods of
    Just (_,m) -> (\l -> TList l TyAny def) <$> mapM deref (HM.elems m)
    Nothing -> evalError' i $ "Module not found: " ++ show t
descModule i as = argsError i as

-- | unsafe function to create domain from TTable.
userTable :: Show n => Term n -> Domain RowKey (Columns Persistable)
userTable = UserTables . userTable'

-- | unsafe function to create TableName from TTable.
userTable' :: Show n => Term n -> TableName
userTable' TTable {..} = TableName $ asString _tModule <> "_" <> asString _tTableName
userTable' t = error $ "creating user table from non-TTable: " ++ show t


read' :: RNativeFun e
read' i as@(table@TTable {}:TLitString key:rest) = do
  cols <- case rest of
    [] -> return []
    [TList cs _ _] -> forM cs $ \c ->
          case c of
            TLitString col -> return $ ColumnId col
            _ -> evalError (_tInfo c) "read: only Strings/Symbols allowed for col keys"
    _ -> argsError i as
  guardTable i table
  mrow <- readRow (_faInfo i) (userTable table) (RowKey key)
  case mrow of
    Nothing -> failTx $ "read: row not found: " ++ show key
    Just (Columns m) -> case cols of
        [] -> return $ (\ps -> TObject ps (_tTableType table) def) $ map (toTerm *** toTerm) $ M.toList m
        _ -> (\ps -> TObject ps (_tTableType table) def) <$> forM cols (\col ->
                case M.lookup col m of
                  Nothing -> evalError' i $ "read: invalid column: " ++ show col
                  Just v -> return (toTerm col,toTerm v))
read' i as = argsError i as


withDefaultRead :: NativeFun e
withDefaultRead fi as@[table',key',defaultRow',b@(TBinding ps bd (BindSchema _) _)] = do
  !tkd <- (,,) <$> reduce table' <*> reduce key' <*> reduce defaultRow'
  case tkd of
    (table@TTable {..},TLitString key,TObject defaultRow _ _) -> do
      guardTable fi table
      mrow <- readRow (_faInfo fi) (userTable table) (RowKey key)
      case mrow of
        Nothing -> bindToRow ps bd b =<< toColumns fi defaultRow
        (Just row) -> bindToRow ps bd b row
    _ -> argsError' fi as
withDefaultRead fi as = argsError' fi as

withRead :: NativeFun e
withRead fi as@[table',key',b@(TBinding ps bd (BindSchema _) _)] = do
  !tk <- (,) <$> reduce table' <*> reduce key'
  case tk of
    (table@TTable {..},TLitString key) -> do
      guardTable fi table
      mrow <- readRow (_faInfo fi) (userTable table) (RowKey key)
      case mrow of
        Nothing -> failTx $ "with-read: row not found: " ++ show key
        (Just row) -> bindToRow ps bd b row
    _ -> argsError' fi as
withRead fi as = argsError' fi as

bindToRow :: [(Arg (Term Ref),Term Ref)] ->
             Scope Int Term Ref -> Term Ref -> Columns Persistable -> Eval e (Term Name)
bindToRow ps bd b (Columns row) = bindReduce ps bd (_tInfo b) (\s -> toTerm <$> M.lookup (ColumnId s) row)

keys' :: RNativeFun e
keys' i [table@TTable {..}] = do
    guardTable i table
    (\b -> TList b tTyString def) . map toTerm <$> keys (_faInfo i) (userTable' table)
keys' i as = argsError i as


txids' :: RNativeFun e
txids' i [table@TTable {..},TLitInteger key] = do
  guardTable i table
  (\b -> TList b tTyInteger def) . map toTerm <$> txids (_faInfo i) (userTable' table) (fromIntegral key)
txids' i as = argsError i as


txlog :: RNativeFun e
txlog i [table@TTable {..},TLitInteger tid] = do
  guardTable i table
  (`TValue` def) . toJSON <$> getTxLog (_faInfo i) (userTable table) (fromIntegral tid)
txlog i as = argsError i as

write :: WriteType -> RNativeFun e
write wt i [table@TTable {..},TLitString key,TObject ps _ _] = do
  guardTable i table
  case _tTableType of
    TyAny -> return ()
    TyVar {} -> return ()
    tty -> void $ checkUserType (wt /= Update) (_faInfo i) ps tty
  success "Write succeeded" . writeRow (_faInfo i) wt (userTable table) (RowKey key) =<< toColumns i ps
write _ i as = argsError i as

toColumns :: FunApp -> [(Term Name,Term Name)] -> Eval e (Columns Persistable)
toColumns i = fmap (Columns . M.fromList) . mapM conv where
    conv (TLitString k, v) = return (ColumnId k,toPersistable v)
    conv (k,_) = evalError' i $ "Only string keys are supported for database writes: " ++ show (k, typeof' k)


createTable' :: RNativeFun e
createTable' i [t@TTable {..}] = do
  guardTable i t
  m <- getModule (_faInfo i) _tModule
  let (UserTables tn) = userTable t
  success "TableCreated" $ createUserTable (_faInfo i) tn _tModule (_mKeySet m)
createTable' i as = argsError i as

guardTable :: Show n => FunApp -> Term n -> Eval e ()
guardTable i TTable {..} = do
  let findMod _ r@Just {} = r
      findMod sf _ = firstOf (sfApp . _Just . _1 . faModule . _Just) sf
  r <- foldr findMod Nothing <$> use evalCallStack
  case r of
    (Just mn) | mn == _tModule -> return ()
    _ -> do
      m <- getModule (_faInfo i) _tModule
      enforceKeySetName (_faInfo i) (_mKeySet m)
guardTable i t = evalError' i $ "Internal error: guardTable called with non-table term: " ++ show t
