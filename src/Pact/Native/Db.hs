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
import Pact.Types
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.String
import Data.Default
import Control.Arrow hiding (app)
import Pact.Native.Internal
import Control.Lens hiding (from,to,(.=))
import Data.Aeson (toJSON,object,(.=))
import Pact.Eval
import Data.Semigroup ((<>))



dbDefs :: Eval e NativeDef
dbDefs = do
  let writeArgs = funType TyString [("table",tableTy),("key",TyString),("object",rowTy)]
      writeDocs s ex = "Write entry in TABLE for KEY of OBJECT column data" ++ s ++ "`$" ++ ex ++ "`"
      rt = Just "row"
      tableTy = TyTable rt
      rowTy = TyObject rt

  foldDefs
    [defRNative "create-table" createTable'
     (funType TyString [("table",tableTy)])
     "Create table TABLE. `$(create-table accounts)`"

    ,defNative (specialForm WithRead) withRead
     (funType TyString [("table",tableTy),("key",TyString),("bindings",TyBinding)])
     "Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements.\
     \`$(with-read 'accounts id { \"balance\":= bal, \"ccy\":= ccy }\n \
     \  (format \"Balance for {} is {} {}\" id bal ccy))`"

    ,defNative (specialForm WithDefaultRead) withDefaultRead
     (funType TyString
      [("table",tableTy),("key",TyString),("defaults",rowTy),("bindings",TyBinding)])
     "Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements. \
     \If row not found, read columns from DEFAULTS, an object with matching key names. \
     \`$(with-default-read 'accounts id { \"balance\": 0, \"ccy\": \"USD\" } { \"balance\":= bal, \"ccy\":= ccy }\n \
     \  (format \"Balance for {} is {} {}\" id bal ccy))`"

    ,defRNative "read" read'
     (funType rowTy [("table",tableTy),("key",TyString)] <>
      funType rowTy [("table",tableTy),("key",TyString),("columns",TyList (Just TyString))])
     "Read row from TABLE for KEY returning database record object, or just COLUMNS if specified. \
     \`$(read 'accounts id ['balance 'ccy])`"

    ,defRNative "keys" keys'
     (funType (TyList (Just TyString)) [("table",tableTy)])
     "Return all keys in TABLE. `$(keys 'accounts)`"

    ,defRNative "txids" txids'
     (funType (TyList (Just TyInteger)) [("table",tableTy),("txid",TyInteger)])
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
     (funType (TyList (Just TyValue)) [("table",tableTy),("txid",TyInteger)])
      "Return all updates to TABLE performed in transaction TXID. `$(txlog 'accounts 123485945)`"
    ,defRNative "describe-table" descTable
     (funType TyValue [("table",TyString)]) "Get metadata for TABLE"
    ,defRNative "describe-keyset" descKeySet
     (funType TyValue [("keyset",TyString)]) "Get metadata for KEYSET"
    ,defRNative "describe-module" descModule
     (funType TyValue [("module",TyString)]) "Get metadata for MODULE"
    ]

descTable :: RNativeFun e
descTable _ [TLitString t] =
    toTerm . (\(m,k) -> object ["name" .= t, "module" .= m, "keyset" .= k]) <$>
    getUserTableInfo (fromString t)
descTable i as = argsError i as

descKeySet :: RNativeFun e
descKeySet i [TLitString t] = do
  r <- readRow KeySets (fromString t)
  case r of
    Just v -> return (toTerm (toJSON v))
    Nothing -> evalError' i $ "Keyset not found: " ++ t
descKeySet i as = argsError i as

descModule :: RNativeFun e
descModule i [TLitString t] = do
  mods <- view (eeRefStore.rsModules.at (fromString t))
  case mods of
    Just (_,m) -> (\l -> TList l def def) <$> mapM deref (HM.elems m)
    Nothing -> evalError' i $ "Module not found: " ++ t
descModule i as = argsError i as

-- | unsafe function to create domain from TTable.
userTable :: Show n => Term n -> Domain RowKey (Columns Persistable)
userTable TTable {..} = UserTables $ fromString $ asString _tModule ++ "." ++ asString _tTableName
userTable t = error $ "creating user table from non-TTable: " ++ show t

read' :: RNativeFun e
read' i as@(table@TTable {}:TLitString key:rest) = do
  cols <- case rest of
    [] -> return []
    [TList cs _ _] -> forM cs $ \c ->
          case c of
            TLitString col -> return $ fromString col
            _ -> evalError (_tInfo c) "read: only Strings/Symbols allowed for col keys"
    _ -> argsError i as
  guardTable i table
  mrow <- readRow (userTable table) (fromString key)
  case mrow of
    Nothing -> failTx $ "read: row not found: " ++ show key
    Just (Columns m) -> case cols of
        [] -> return $ (\ps -> TObject ps def def) $ map (toTerm *** toTerm) $ M.toList m
        _ -> (\ps -> TObject ps def def) <$> forM cols (\col ->
                case M.lookup col m of
                  Nothing -> evalError' i $ "read: invalid column: " ++ show col
                  Just v -> return (toTerm col,toTerm v))
read' i as = argsError i as


withDefaultRead :: NativeFun e
withDefaultRead fi as@[table',key',defaultRow',b@(TBinding ps bd BindKV _)] = do
  !tkd <- (,,) <$> reduce table' <*> reduce key' <*> reduce defaultRow'
  case tkd of
    (table@TTable {..},TLitString key,TObject defaultRow _ _) -> do
      guardTable fi table
      mrow <- readRow (userTable table) (fromString key)
      case mrow of
        Nothing -> bindToRow ps bd b =<< toColumns fi defaultRow
        (Just row) -> bindToRow ps bd b row
    _ -> argsError' fi as
withDefaultRead fi as = argsError' fi as

withRead :: NativeFun e
withRead fi as@[table',key',b@(TBinding ps bd BindKV _)] = do
  !tk <- (,) <$> reduce table' <*> reduce key'
  case tk of
    (table@TTable {..},TLitString key) -> do
      guardTable fi table
      mrow <- readRow (userTable table) (fromString key)
      case mrow of
        Nothing -> failTx $ "with-read: row not found: " ++ show key
        (Just row) -> bindToRow ps bd b row
    _ -> argsError' fi as
withRead fi as = argsError' fi as

bindToRow :: [(Arg,Term Ref)] ->
             Scope Int Term Ref -> Term Ref -> Columns Persistable -> Eval e (Term Name)
bindToRow ps bd b (Columns row) = bindReduce ps bd (_tInfo b) (\s -> toTerm <$> M.lookup (fromString s) row)

keys' :: RNativeFun e
keys' i [table@TTable {..}] = do
    guardTable i table
    (\b -> TList b (Just TyString) def) . map toTerm <$> keys _tTableName
keys' i as = argsError i as


txids' :: RNativeFun e
txids' i [table@TTable {..},TLitInteger key] = do
  guardTable i table
  (\b -> TList b (Just TyInteger) def) . map toTerm <$> txids _tTableName (fromIntegral key)
txids' i as = argsError i as


txlog :: RNativeFun e
txlog i [table@TTable {..},TLitInteger tid] = do
  guardTable i table
  (`TValue` def) . toJSON <$> getTxLog (userTable table) (fromIntegral tid)
txlog i as = argsError i as

write :: WriteType -> RNativeFun e
write wt i [table@TTable {..},TLitString key,TObject ps _ _] = do
  guardTable i table
  case _tTableType of
    Nothing -> return ()
    Just tty -> void $ checkUserType (wt /= Update) (_faInfo i) ps tty
  success "Write succeeded" . writeRow wt (userTable table) (fromString key) =<< toColumns i ps
write _ i as = argsError i as

toColumns :: FunApp -> [(Term Name,Term Name)] -> Eval e (Columns Persistable)
toColumns i = fmap (Columns . M.fromList) . mapM conv where
    conv (TLitString k, TLiteral v _) = return (fromString k,PLiteral v)
    conv (TLitString k, TKeySet ks _) = return (fromString k,PKeySet ks)
    conv (TLitString k, TObject ks _ _) = toColumns i ks >>= \o' -> return (fromString k,PValue $ toJSON o')
    conv (TLitString k, TValue v _) = return (fromString k,PValue v)
    conv p = evalError' i $ "Invalid types in object pair: " ++ show p


createTable' :: RNativeFun e
createTable' i [t@TTable {..}] = do
  guardTable i t
  m <- getModule (_faInfo i) _tModule
  let (UserTables tn) = userTable t
  success "TableCreated" $ createUserTable tn _tModule (_mKeySet m)
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
