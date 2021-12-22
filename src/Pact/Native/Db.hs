{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
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

import Bound
import Control.Arrow hiding (app)
import Control.Lens hiding ((.=))
import Control.Monad
import Control.Monad.Reader (ask)
import Data.Default
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import Data.Foldable (foldlM)
import qualified Data.Vector as V
import Data.Text (pack)

import Pact.Eval
import Pact.Runtime.Typecheck
import Pact.Runtime.Utils
import Pact.Native.Internal
import Pact.Types.Pretty
import Pact.Types.RowData
import Pact.Types.Runtime
import Pact.Types.PactValue


class Readable a where
  readable :: a -> ReadValue

instance Readable RowData where
  readable = ReadData
instance Readable RowKey where
  readable = ReadKey
instance Readable TxId where
  readable = const ReadTxId
instance Readable (TxLog RowData) where
  readable = ReadData . _txValue
instance Readable (TxId, TxLog RowData) where
  readable = ReadData . _txValue . snd

-- | Parameterize calls to 'guardTable' with op.
data GuardTableOp
  = GtRead
  | GtSelect
  | GtWithRead
  | GtWithDefaultRead
  | GtKeys
  | GtTxIds
  | GtTxLog
  | GtKeyLog
  | GtWrite
  | GtCreateTable


dbDefs :: NativeModule
dbDefs =
  let writeArgs part = funType tTyString
        [("table",tableTy),("key",tTyString),
         ("object",TySchema TyObject rt part)]
      writeDocs s = "Write entry in TABLE for KEY of OBJECT column data" <> s
      rt = mkSchemaVar "row"
      tableTy = TySchema TyTable rt def
      rowTy = TySchema TyObject rt def
      bindTy = TySchema TyBinding rt def
      partialize = set tySchemaPartial AnySubschema
      a = mkTyVar "a" []
      b = mkTyVar "b" []
  in ("Database",
    [setTopLevelOnly $ defGasRNative "create-table" createTable'
     (funType tTyString [("table",tableTy)])
     [LitExample "(create-table accounts)"] "Create table TABLE."

    ,defNative (specialForm WithRead) withRead
     (funType a [("table",tableTy),("key",tTyString),("bindings",bindTy)])
     [ LitExample "(with-read accounts id { \"balance\":= bal, \"ccy\":= ccy }\n\
       \  (format \"Balance for {} is {} {}\" [id bal ccy]))"
     ]
     "Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements."

    ,defNative (specialForm WithDefaultRead) withDefaultRead
     (funType a
      [("table",tableTy),("key",tTyString),("defaults",partialize rowTy),("bindings",partialize bindTy)])
     [ LitExample "(with-default-read accounts id { \"balance\": 0, \"ccy\": \"USD\" } { \"balance\":= bal, \"ccy\":= ccy }\n\
       \  (format \"Balance for {} is {} {}\" [id bal ccy]))"
     ]
     "Special form to read row from TABLE for KEY and bind columns per BINDINGS over subsequent body statements. \
     \If row not found, read columns from DEFAULTS, an object with matching key names."

    ,defGasRNative "read" read'
     (funType rowTy [("table",tableTy),("key",tTyString)] <>
      funType rowTy [("table",tableTy),("key",tTyString),("columns",TyList tTyString)])
     [LitExample "(read accounts id ['balance 'ccy])"]
     "Read row from TABLE for KEY, returning database record object, or just COLUMNS if specified."

    ,defNative (specialForm Select) select
      (funType (TyList rowTy)  [("table",tableTy),("where",TyFun $ funType' tTyBool [("row",rowTy)])] <>
       funType (TyList rowTy)  [("table",tableTy),("columns",TyList tTyString),("where",TyFun $ funType' tTyBool [("row",rowTy)])])
      [ LitExample "(select people ['firstName,'lastName] (where 'name (= \"Fatima\")))"
      , LitExample "(select people (where 'age (> 30)))?"
      ]
      "Select full rows or COLUMNS from table by applying WHERE to each row to get a boolean determining inclusion. Output sorted based on keys."

    ,defGasRNative "keys" keys'
     (funType (TyList tTyString) [("table",tableTy)])
     [LitExample "(keys accounts)"] "Return all keys in TABLE as a sorted list."

    ,defNative "fold-db" foldDB'
      (funType (TyList b)
        [ ("table", tableTy)
        , ("qry", TyFun (funType' (TyPrim TyBool) [("a", TyPrim TyString), ("b", rowTy)] ))
        , ("consumer", TyFun (funType' b [("a", TyPrim TyString), ("b", rowTy)]))])
      [LitExample "(let* \n\
                  \ ((qry (lambda (k obj) true)) ;; select all rows\n\
                  \  (f (lambda (x) [(at 'firstName x), (at 'b x)]))\n\
                  \ )\n\
                  \ (fold-db people (qry) (f))\n\
                  \)"]
      "Select rows from TABLE using QRY as a predicate with both key and value, and then accumulate results of the query \
      \in CONSUMER. Output is sorted by the ordering of keys."
    ,defGasRNative "txids" txids'
     (funType (TyList tTyInteger) [("table",tableTy),("txid",tTyInteger)])
     [LitExample "(txids accounts 123849535)"] "Return all txid values greater than or equal to TXID in TABLE."

    ,defNative "write" (write Write FullSchema) (writeArgs FullSchema)
     [LitExample "(write accounts id { \"balance\": 100.0 })"] (writeDocs ".")
    ,defNative "insert" (write Insert FullSchema) (writeArgs FullSchema)
     [LitExample "(insert accounts id { \"balance\": 0.0, \"note\": \"Created account.\" })"]
     (writeDocs ", failing if data already exists for KEY.")
    ,defNative "update" (write Update AnySubschema) (writeArgs AnySubschema)
     [LitExample "(update accounts id { \"balance\": (+ bal amount), \"change\": amount, \"note\": \"credit\" })"]
     (writeDocs ", failing if data does not exist for KEY.")
    ,defGasRNative "txlog" txlog
     (funType (TyList tTyObjectAny) [("table",tableTy),("txid",tTyInteger)])
      [LitExample "(txlog accounts 123485945)"] "Return all updates to TABLE performed in transaction TXID."
    ,defGasRNative "keylog" keylog
     (funType (TyList (tTyObject TyAny)) [("table",tableTy),("key",tTyString),("txid",tTyInteger)])
      [LitExample "(keylog accounts \"Alice\" 123485945)"] "Return updates to TABLE for a KEY in transactions at or after TXID, in a list of objects \
      \indexed by txid."

    ,setTopLevelOnly $ defRNative "describe-table" descTable
     (funType tTyObjectAny [("table",tableTy)])
     [LitExample "(describe-table accounts)"]
     "Get metadata for TABLE. Returns an object with 'name', 'hash', 'blessed', 'code', and 'keyset' fields."
    ,setTopLevelOnly $ defGasRNative "describe-keyset" descKeySet
     (funType tTyObjectAny [("keyset",tTyString)]) [] "Get metadata for KEYSET."
    ,setTopLevelOnly $ defRNative "describe-module" descModule
     (funType tTyObjectAny [("module",tTyString)])
     [LitExample "(describe-module 'my-module)"]
     "Get metadata for MODULE. Returns an object with 'name', 'hash', 'blessed', 'code', and 'keyset' fields."
    ])

descTable :: RNativeFun e
descTable _ [TTable {..}] = return $ toTObject TyAny def [
  ("name",tStr $ asString _tTableName),
  ("module", tStr $ asString _tModuleName),
  ("type", toTerm $ pack $ showPretty _tTableType)]
descTable i as = argsError i as

descKeySet :: GasRNativeFun e
descKeySet g i [TLitString t] = do
  r <- readRow (_faInfo i) KeySets (KeySetName t)
  case r of
    Just v -> computeGas' g i (GPostRead (ReadKeySet (KeySetName t) v)) $
              return $ toTerm v
    Nothing -> evalError' i $ "Keyset not found: " <> pretty t
descKeySet _ i as = argsError i as

descModule :: RNativeFun e
descModule i [TLitString t] = do
  mods <- lookupModule i (ModuleName t Nothing)
  case _mdModule <$> mods of
    Just m ->
      case m of
        MDModule Module{..} ->
          return $ toTObject TyAny def
            [ ("name"      , tStr $ asString _mName)
            , ("hash"      , tStr $ asString _mHash)
            , ("keyset"    , tStr $ pack $ show _mGovernance)
            , ("blessed"   , toTList tTyString def $ map (tStr . asString) (HS.toList _mBlessed))
            , ("code"      , tStr $ asString _mCode)
            , ("interfaces", toTList tTyString def $ (tStr . asString) <$> _mInterfaces)
            ]
        MDInterface Interface{..} ->
          return $ toTObject TyAny def
            [ ("name", tStr $ asString _interfaceName)
            , ("code", tStr $ asString _interfaceCode)
            ]
    Nothing -> evalError' i $ "Module not found: " <> pretty t
descModule i as = argsError i as

-- | unsafe function to create domain from TTable.
userTable :: Show n => Term n -> Domain RowKey RowData
userTable = UserTables . userTable'

-- | unsafe function to create TableName from TTable.
userTable' :: Show n => Term n -> TableName
userTable' TTable {..} = TableName $ asString _tModuleName <> "_" <> asString _tTableName
userTable' t = error $ "creating user table from non-TTable: " ++ show t


read' :: GasRNativeFun e
read' g0 i as@(table@TTable {}:TLitString key:rest) = do
  cols <- case rest of
    [] -> return []
    [l] -> colsToList (argsError i as) l
    _ -> argsError i as
  guardTable i table GtRead
  mrow <- readRow (_faInfo i) (userTable table) (RowKey key)
  case mrow of
    Nothing -> failTx (_faInfo i) $ "read: row not found: " <> pretty key
    Just cs -> do
      g <- gasPostRead i g0 cs
      fmap (g,) $ case cols of
        [] -> return $ columnsToObject (_tTableType table) cs
        _ -> columnsToObject' (_tTableType table) cols cs

read' _ i as = argsError i as


foldDB' :: NativeFun e
foldDB' i [tbl, tLamToApp -> TApp qry _, tLamToApp -> TApp consumer _] = do
  table <- reduce tbl >>= \case
    t@TTable{} -> return t
    t -> evalError' i $ "Expected table as first argument to foldDB, got: " <> pretty t
  !g0 <- computeGas (Right i) (GUnreduced [])
  !g1 <- computeGas (Right i) GFoldDB
  ks <- getKeys table
  (!g2, xs) <- foldlM (fdb table) (g0+g1, []) ks
  pure (g2, TList (V.fromList (reverse xs)) TyAny def)
  where
  asBool (TLiteral (LBool satisfies) _) = return satisfies
  asBool t = evalError' i $ "Unexpected return value from fold-db query condition " <> pretty t
  getKeys table = do
    guardTable i table GtKeys
    keys (_faInfo i) (userTable table)
  fdb table (!g0, acc) key = do
    mrow <- readRow (_faInfo i) (userTable table) key
    case mrow of
      Just row -> do
        g1 <- gasPostRead i g0 row
        let obj = columnsToObject (_tTableType table) row
        let key' = toTerm key
        cond <- asBool =<< apply qry [key', obj]
        if cond then do
          r' <- apply consumer [key', obj]
          pure (g1, r':acc)
        else pure (g1, acc)
      Nothing -> evalError (_faInfo i) $ "foldDb: unexpected error, key: "
                 <> pretty key <> " not found in table: " <> pretty table
foldDB' i as = argsError' i as

gasPostRead :: Readable r => FunApp -> Gas -> r -> Eval e Gas
gasPostRead i g0 row = (g0 +) <$> computeGas (Right i) (GPostRead $ readable row)

gasPostRead' :: Readable r => FunApp -> Gas -> r -> Eval e a -> Eval e (Gas,a)
gasPostRead' i g0 row action = gasPostRead i g0 row >>= \g -> (g,) <$> action

-- | TODO improve post-streaming
gasPostReads :: Readable r => FunApp -> Gas -> ([r] -> a) -> Eval e [r] -> Eval e (Gas,a)
gasPostReads i g0 postProcess action = do
  rs <- action
  (,postProcess rs) <$> foldM (gasPostRead i) g0 rs

columnsToObject :: Type (Term Name) -> RowData -> Term Name
columnsToObject ty m = TObject (Object (fmap (fromPactValue . rowDataToPactValue) (_rdData m)) ty def def) def

columnsToObject' :: Type (Term Name) -> [(Info,FieldKey)] ->
                    RowData -> Eval m (Term Name)
columnsToObject' ty cols (RowData _ (ObjectMap m)) = do
  ps <- forM cols $ \(ci,col) ->
                case M.lookup col m of
                  Nothing -> evalError ci $ "read: invalid column: " <> pretty col
                  Just v -> return (col, fromPactValue $ rowDataToPactValue v)
  return $ TObject (Object (ObjectMap (M.fromList ps)) ty def def) def



select :: NativeFun e
select i as@[tbl',cols',app] = do
  cols <- reduce cols' >>= colsToList (argsError' i as)
  reduce tbl' >>= select' i as (Just cols) app
select i as@[tbl',app] = reduce tbl' >>= select' i as Nothing app
select i as = argsError' i as

select' :: FunApp -> [Term Ref] -> Maybe [(Info,FieldKey)] ->
           Term Ref -> Term Name -> Eval e (Gas,Term Name)
select' i _ cols' app@TApp{} tbl@TTable{} = do
    g0 <- computeGas (Right i) (GUnreduced [])
    g1 <- computeGas (Right i) $ GSelect cols'
    guardTable i tbl GtSelect
    let fi = _faInfo i
        tblTy = _tTableType tbl
    ks <- keys fi (userTable tbl)
    fmap (second (\b -> TList (V.fromList (reverse b)) tblTy def)) $
      (\f -> foldM f (g0 + g1, []) ks) $ \(gPrev,rs) k -> do

      mrow <- readRow fi (userTable tbl) k
      case mrow of
        Nothing -> evalError fi $ "select: unexpected error, key not found in select: "
                   <> pretty k <> ", table: " <> pretty tbl
        Just row -> do
          g <- gasPostRead i gPrev row
          let obj = columnsToObject tblTy row
          result <- apply (_tApp app) [obj]
          fmap (g,) $ case result of
            (TLiteral (LBool include) _)
              | include -> case cols' of
                  Nothing -> return (obj:rs)
                  Just cols -> (:rs) <$> columnsToObject' tblTy cols row
              | otherwise -> return rs
            t -> evalError (_tInfo app) $ "select: filter returned non-boolean value: "
                 <> pretty t
select' i as _ _ _ = argsError' i as


withDefaultRead :: NativeFun e
withDefaultRead fi as@[table',key',defaultRow',b@(TBinding ps bd (BindSchema _) _)] = do
  let argsToReduce = [table',key',defaultRow']
  (!g0,!tkd) <- gasUnreduced fi argsToReduce (mapM reduce argsToReduce)
  case tkd of
    [table@TTable {}, TLitString key, TObject (Object defaultRow _ _ _) _] -> do
      guardTable fi table GtWithDefaultRead
      mrow <- readRow (_faInfo fi) (userTable table) (RowKey key)
      case mrow of
        Nothing -> (g0,) <$> (bindToRow ps bd b =<< enforcePactValue' defaultRow)
        (Just row) -> gasPostRead' fi g0 row $ bindToRow ps bd b (rowDataToPactValue <$> _rdData row)
    _ -> argsError' fi as
withDefaultRead fi as = argsError' fi as

withRead :: NativeFun e
withRead fi as@[table',key',b@(TBinding ps bd (BindSchema _) _)] = do
  let argsToReduce = [table',key']
  (!g0,!tk) <- gasUnreduced fi argsToReduce (mapM reduce argsToReduce)
  case tk of
    [table@TTable {},TLitString key] -> do
      guardTable fi table GtWithRead
      mrow <- readRow (_faInfo fi) (userTable table) (RowKey key)
      case mrow of
        Nothing -> failTx (_faInfo fi) $ "with-read: row not found: " <> pretty key
        (Just row) -> gasPostRead' fi g0 row $ bindToRow ps bd b (rowDataToPactValue <$> _rdData row)
    _ -> argsError' fi as
withRead fi as = argsError' fi as

bindToRow :: [BindPair (Term Ref)] ->
             Scope Int Term Ref -> Term Ref -> ObjectMap PactValue -> Eval e (Term Name)
bindToRow ps bd b (ObjectMap row) =
  bindReduce ps bd (_tInfo b) (\s -> fromPactValue <$> M.lookup (FieldKey s) row)

keys' :: GasRNativeFun e
keys' g i [table@TTable {}] = do
  gasPostReads i g
    ((\b -> TList (V.fromList b) tTyString def) . map toTerm) $ do
      guardTable i table GtKeys
      keys (_faInfo i) (userTable table)
keys' _ i as = argsError i as


txids' :: GasRNativeFun e
txids' g i [table@TTable {},TLitInteger key] = do
  checkNonLocalAllowed i
  gasPostReads i g
    ((\b -> TList (V.fromList b) tTyInteger def) . map toTerm) $ do
      guardTable i table GtTxIds
      txids (_faInfo i) (userTable' table) (fromIntegral key)
txids' _ i as = argsError i as

txlog :: GasRNativeFun e
txlog g i [table@TTable {},TLitInteger tid] = do
  checkNonLocalAllowed i
  gasPostReads i g (toTList TyAny def . map txlogToObj) $ do
      guardTable i table GtTxLog
      getTxLog (_faInfo i) (userTable table) (fromIntegral tid)
txlog _ i as = argsError i as

txlogToObj :: TxLog RowData -> Term Name
txlogToObj TxLog{..} = toTObject TyAny def
  [ ("table", toTerm _txDomain)
  , ("key", toTerm _txKey)
  , ("value", toTObjectMap TyAny def (fmap (fromPactValue . rowDataToPactValue) (_rdData _txValue)))
  ]

checkNonLocalAllowed :: HasInfo i => i -> Eval e ()
checkNonLocalAllowed i = do
  env <- ask
  disabledInTx <- isExecutionFlagSet FlagDisableHistoryInTransactionalMode
  let mode = view eeMode env
  when (mode == Transactional && disabledInTx) $ evalError' i $
    "Operation only permitted in local execution mode"

keylog :: GasRNativeFun e
keylog g i [table@TTable {..},TLitString key,TLitInteger utid] = do
  checkNonLocalAllowed i
  let postProc = toTList TyAny def . map toTxidObj
        where toTxidObj (t,r) =
                toTObject TyAny def [("txid", toTerm t),("value",columnsToObject _tTableType (_txValue r))]
  gasPostReads i g postProc $ do
    guardTable i table GtKeyLog
    tids <- txids (_faInfo i) (userTable' table) (fromIntegral utid)
    logs <- fmap concat $ forM tids $ \tid -> map (tid,) <$> getTxLog (_faInfo i) (userTable table) tid
    return $ filter (\(_,TxLog {..}) -> _txKey == key) logs

keylog _ i as = argsError i as

write :: WriteType -> SchemaPartial -> NativeFun e
write wt partial i as = do
  ts <- mapM reduce as
  case ts of
    [table@TTable {..},TLitString key,(TObject (Object ps _ _ _) _)] -> do
      ps' <- enforcePactValue' ps
      cost0 <- computeGas (Right i) (GUnreduced [])
      cost1 <- computeGas (Right i) (GPreWrite (WriteData wt (asString key) ps'))
      guardTable i table GtWrite
      case _tTableType of
        TyAny -> return ()
        TyVar {} -> return ()
        tty -> void $ checkUserType partial (_faInfo i) ps tty
      rdv <- ifExecutionFlagSet' FlagDisablePact420 RDV0 RDV1
      r <- success "Write succeeded" $ writeRow (_faInfo i) wt (userTable table) (RowKey key) $
          RowData rdv (pactValueToRowData <$> ps')
      return (cost0 + cost1, r)
    _ -> argsError i ts


createTable' :: GasRNativeFun e
createTable' g i [t@TTable {..}] = do
  guardTable i t GtCreateTable
  let (UserTables tn) = userTable t
  computeGas' g i (GPreWrite (WriteTable (asString tn))) $
    success "TableCreated" $ createUserTable (_faInfo i) tn _tModuleName
createTable' _ i as = argsError i as

guardTable :: Pretty n => FunApp -> Term n -> GuardTableOp -> Eval e ()
guardTable i TTable {..} dbop =
  checkLocalBypass $
    guardForModuleCall (_faInfo i) _tModuleName $
      enforceBlessedHashes i _tModuleName _tHash
  where checkLocalBypass notBypassed = do
          localBypassEnabled <- isExecutionFlagSet FlagAllowReadInLocal
          case dbop of
            GtWrite -> notBypassed
            GtCreateTable -> notBypassed
            _ | localBypassEnabled -> return ()
              | otherwise -> notBypassed

guardTable i t _ = evalError' i $ "Internal error: guardTable called with non-table term: " <> pretty t

enforceBlessedHashes :: FunApp -> ModuleName -> ModuleHash -> Eval e ()
enforceBlessedHashes i mn h = getModule i mn >>= \m -> case (_mdModule m) of
        MDModule Module{..}
          | h == _mHash -> return () -- current version ok
          | h `HS.member` _mBlessed -> return () -- hash is blessed
          | otherwise -> evalError' i $
            "Execution aborted, hash not blessed for module " <> pretty mn <> ": " <> pretty h
        _ -> evalError' i $ "Internal error: expected module reference " <> pretty mn
