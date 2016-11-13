{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.String
import Data.Default
import Control.Arrow hiding (app)
import Pact.Native.Internal
import Control.Lens hiding (from,to,(.=))
import Data.Aeson (toJSON,object,(.=))
import Pact.Eval



dbDefs :: Eval e NativeDef
dbDefs = do
  let writeArgs = ["table","key","object"]
      writeDocs s ex = "Write entry in TABLE for KEY of OBJECT column data" ++ s ++ "`$" ++ ex ++ "`"

  foldDefs
    [defRNative "create-table" createTable' ["table","module"]
     "Create table TABLE guarded by module MODULE. `$(create-table 'accounts 'accounts-admin)`"
    ,defNative "with-read" withRead ["table","key","bindings", "body"]
     "Read row from TABLE for KEY and bind columns per BINDINGS over BODY.\
     \`$(with-read 'accounts id { \"balance\":= bal, \"ccy\":= ccy }\n \
     \  (format \"Balance for {} is {} {}\" id bal ccy))`"
    ,defNative "with-default-read" withDefaultRead ["table","key","defaults","bindings", "body"]
     "Read row from TABLE for KEY and bind columns per BINDINGS over BODY. \
     \If row not found, read columns from DEFAULTS, an object with matching key names. \
     \`$(with-default-read 'accounts id { \"balance\": 0, \"ccy\": \"USD\" } { \"balance\":= bal, \"ccy\":= ccy }\n \
     \  (format \"Balance for {} is {} {}\" id bal ccy))`"
    ,defRNative "read" read' ["table","key","colnames..."]
     "Read row from TABLE for KEY returning object of COLNAMES mapped to values, or entire record if not specified. \
     \`$(read 'accounts id 'balance 'ccy)`"
    ,defRNative "keys" keys' ["table"] "Return all keys in TABLE. `$(keys 'accounts)`"
    ,defRNative "txids" txids' ["table","txid"]
     "Return all txid values greater than or equal to TXID in TABLE. `$(txids 'accounts 123849535)`"
    ,defRNative "write" (write Write) writeArgs
     (writeDocs "." "(write 'accounts { \"balance\": 100.0 })")
    ,defRNative "insert" (write Insert) writeArgs
     (writeDocs ", failing if data already exists for KEY."
     "(insert 'accounts { \"balance\": 0.0, \"note\": \"Created account.\" })")
    ,defRNative "update" (write Update) writeArgs
     (writeDocs ", failing if data does not exist for KEY."
      "(update 'accounts { \"balance\": (+ bal amount), \"change\": amount, \"note\": \"credit\" })")
    ,defRNative "txlog" txlog ["table","txid"]
      "Return all updates to TABLE performed in transaction TXID. `$(txlog 'accounts 123485945)`"
    ,defRNative "describe-table" descTable ["table"] "Get metadata for TABLE"
    ,defRNative "describe-keyset" descKeySet ["keyset"] "Get metadata for KEYSET"
    ,defRNative "describe-module" descModule ["module"] "Get metadata for MODULE"
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
  r <- readRow Modules (fromString t)
  case r of
    Just v -> return (toTerm (toJSON v))
    Nothing -> evalError' i $ "Keyset not found: " ++ t
descModule i as = argsError i as


userTable :: String -> Domain RowKey (Columns Persistable)
userTable = UserTables . fromString

read' :: RNativeFun e
read' i (TLitString table:TLitString key:cs) = do
  cols <- forM cs $ \c ->
          case c of
            TLitString col -> return $ fromString col
            _ -> evalError (_tInfo c) "read: only Strings/Symbols allowed for col keys"
  guardTable i table
  mrow <- readRow (userTable table) (fromString key)
  case mrow of
    Nothing -> failTx $ "read: row not found: " ++ show key
    Just (Columns m) -> case cols of
        [] -> return $ (`TObject` def) $ map (toTerm *** toTerm) $ M.toList m
        _ -> (`TObject` def) <$> forM cols (\col ->
                case M.lookup col m of
                  Nothing -> evalError' i $ "read: invalid column: " ++ show col
                  Just v -> return (toTerm col,toTerm v))
read' i as = argsError i as


withDefaultRead :: NativeFun e
withDefaultRead fi as@[table',key',defaultRow',b@(TBinding ps bd _)] = do
  !tkd <- (,,) <$> reduce table' <*> reduce key' <*> reduce defaultRow'
  case tkd of
    (TLitString table,TLitString key,TObject defaultRow _) -> do
      guardTable fi table
      mrow <- readRow (userTable table) (fromString key)
      case mrow of
        Nothing -> bindToRow ps bd b =<< toColumns fi defaultRow
        (Just row) -> bindToRow ps bd b row
    _ -> argsError' fi as
withDefaultRead fi as = argsError' fi as

withRead :: NativeFun e
withRead fi as@[table',key',b@(TBinding ps bd _)] = do
  !tk <- (,) <$> reduce table' <*> reduce key'
  case tk of
    (TLitString table,TLitString key) -> do
      guardTable fi table
      mrow <- readRow (userTable table) (fromString key)
      case mrow of
        Nothing -> failTx $ "with-read: row not found: " ++ show key
        (Just row) -> bindToRow ps bd b row
    _ -> argsError' fi as
withRead fi as = argsError' fi as

bindToRow :: [(String,Term Ref)] ->
             Scope Int Term Ref -> Term Ref -> Columns Persistable -> Eval e (Term Name)
bindToRow ps bd b (Columns row) = bindReduce ps bd (_tInfo b) (\s -> toTerm <$> M.lookup (fromString s) row)

keys' :: RNativeFun e
keys' i [TLitString table] = do
    guardTable i table
    (`TList` def) . map toTerm <$> keys (fromString table)
keys' i as = argsError i as


txids' :: RNativeFun e
txids' i [TLitString table,TLitInteger key] = do
  guardTable i table
  (`TList` def) . map toTerm <$> txids (fromString table) (fromIntegral key)
txids' i as = argsError i as


txlog :: RNativeFun e
txlog i [TLitString table,TLitInteger tid] = do
  guardTable i table
  (`TValue` def) . toJSON <$> getTxLog (userTable table) (fromIntegral tid)
txlog i as = argsError i as

write :: WriteType -> RNativeFun e
write wt i [TLitString table,TLitString key,TObject ps _] = do
  guardTable i table
  success "Write succeeded" . writeRow wt (userTable table) (fromString key) =<< toColumns i ps
write _ i as = argsError i as

toColumns :: FunApp -> [(Term Name,Term Name)] -> Eval e (Columns Persistable)
toColumns i = fmap (Columns . M.fromList) . mapM conv where
    conv (TLitString k, TLiteral v _) = return (fromString k,PLiteral v)
    conv (TLitString k, TKeySet ks _) = return (fromString k,PKeySet ks)
    conv (TLitString k, TObject ks _) = toColumns i ks >>= \o' -> return (fromString k,PValue $ toJSON o')
    conv (TLitString k, TValue v _) = return (fromString k,PValue v)
    conv p = evalError' i $ "Invalid types in object pair: " ++ show p


createTable' :: RNativeFun e
createTable' i [TLitString table,TLitString mn] = do
  mm <- readRow Modules (fromString mn)
  case mm of
    Nothing -> evalError' i $ "create-table: module not found: " ++ show mn
    Just m ->
        success "TableCreated" $ createUserTable (fromString table) (_modName m) (_modKeySet m)
createTable' i as = argsError i as

guardTable :: FunApp -> String -> Eval e ()
guardTable i table = do
  (tm,tk) <- getUserTableInfo (fromString table)
  let findMod _ r@Just {} = r
      findMod sf _ = _sTermModule sf
  r <- foldr findMod Nothing <$> use evalCallStack
  case r of
    (Just mn) | mn == tm -> return ()
    _ -> enforceKeySetName (_faInfo i) tk
