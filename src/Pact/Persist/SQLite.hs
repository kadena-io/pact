{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Pact.Persist.SQLite
  (SQLite(..),initSQLite,persister,Pragma(..))
  where

import Control.Arrow
import Control.Monad
import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import Data.String
import Database.SQLite3.Direct as SQ3
import Prelude hiding (log)
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import Control.Monad.State

import Pact.Persist
import Pact.Types.SQLite


data TableStmts = TableStmts
  { sInsertReplace :: Statement
  , sInsert :: Statement
  , sReplace :: Statement
  , sRead :: Statement
  }

data TxStmts = TxStmts
  { tBegin :: Statement
  , tCommit :: Statement
  , tRollback :: Statement
  }

data SQLite = SQLite
  { conn :: Database
  , log :: String -> IO ()
  , tableStmts :: M.Map Utf8 TableStmts
  , txStmts :: TxStmts
  }

toUtf8 :: Text -> Utf8
toUtf8 = Utf8 . encodeUtf8

tableName :: Table k -> Utf8
tableName (DataTable t) = toUtf8 $ sanitize t <> "_DATA"
tableName (TxTable t) = toUtf8 $ sanitize t <> "_TX"

sanitize :: TableId -> Text
sanitize (TableId t) = T.replace "-" "_" t

persister :: Persister SQLite
persister = Persister {

  createTable = \t s -> (,()) <$> createTable' t s
  ,
  beginTx = \s -> execs_ (tBegin (txStmts s)) >> return (s,())
  ,
  commitTx = \s -> execs_ (tCommit (txStmts s)) >> return (s,())
  ,
  rollbackTx = \s -> execs_ (tRollback (txStmts s)) >> return (s,())
  ,
  queryKeys = \t kq s -> (s,) <$> queryKeys' t kq s
  ,
  query = \t kq s -> (s,) <$> query' t kq s
  ,
  readValue = \t k s -> (s,) <$> readData' t k s
  ,
  writeValue = \t wt k v s -> (s,) <$> writeData' t wt k v s


  }

data KeyTys k = KeyTys {
  textTy :: Utf8,
  inFun :: k -> SType,
  outTy :: RType,
  outFun :: SType -> IO k
  }

decodeText :: SType -> IO Text
decodeText (SText (Utf8 t)) = return $ decodeUtf8 t
decodeText v = throwDbError $ "Expected text, got: " ++ show v

decodeInt :: SType -> IO Int
decodeInt (SInt i) = return $ fromIntegral i
decodeInt v = throwDbError $ "Expected int, got: " ++ show v

decodeBlob :: (FromJSON v) => SType -> IO v
decodeBlob (SText (Utf8 t)) = liftEither (return $ eitherDecodeStrict' t)
decodeBlob v = throwDbError $ "Expected text blob, got: " ++ show v

encodeBlob :: ToJSON a => a -> SType
encodeBlob a = SText $ Utf8 $ BSL.toStrict $ encode a
{-# INLINE encodeBlob #-}

expectSing :: Show a => String -> [a] -> IO a
expectSing _ [s] = return s
expectSing desc v = throwDbError $ "Expected single-" ++ desc ++ " result, got: " ++ show v

expectTwo :: Show a => String -> [a] -> IO (a,a)
expectTwo _ [a,b] = return (a,b)
expectTwo desc v = throwDbError $ "Expected two-" ++ desc ++ " result, got: " ++ show v

kTextTys :: KeyTys Text
kTextTys = KeyTys "text" (SText . toUtf8) RText decodeText
kIntTys :: KeyTys Int
kIntTys = KeyTys "int" (SInt . fromIntegral) RInt decodeInt

kTys :: Table k -> KeyTys k
kTys DataTable {} = kTextTys
kTys TxTable {} = kIntTys

createTable' :: Table k -> SQLite -> IO SQLite
createTable' t e = do
  log e $ "createTable: " ++ show t
  let tn = tableName t
  exec_ (conn e) $
    "CREATE TABLE " <> tn <>
    " (key " <> textTy (kTys t) <> " PRIMARY KEY NOT NULL UNIQUE, value TEXT);"
  let mkstmt q = prepStmt (conn e) q
  ss <- TableStmts <$>
           mkstmt ("INSERT OR REPLACE INTO " <> tn <> " VALUES (?,?)") <*>
           mkstmt ("INSERT INTO " <> tn <> " VALUES (?,?)") <*>
           mkstmt ("REPLACE INTO " <> tn <> " VALUES (?,?)") <*>
           mkstmt ("SELECT VALUE FROM " <> tn <> " WHERE KEY = ?")
  return $ e { tableStmts = M.insert tn ss (tableStmts e) }


query' :: FromJSON v => Table k -> Maybe (KeyQuery k) -> SQLite -> IO [(k,v)]
query' t kq e = do
  let (cols,outParams) = ("key, value",[outTy (kTys t),RText])
  rs <- doQuery t kq cols outParams e
  mapM (expectTwo "field" >=> (\(k,v) -> (,) <$> outFun (kTys t) k <*> decodeBlob v)) rs

queryKeys' :: Table k -> Maybe (KeyQuery k) -> SQLite -> IO [k]
queryKeys' t kq e = do
  let (cols,outParams) = ("key",[outTy (kTys t)])
  rs <- doQuery t kq cols outParams e
  mapM (expectSing "field" >=> outFun (kTys t)) rs

doQuery :: Table k -> Maybe (KeyQuery k) -> Utf8 -> [RType] -> SQLite -> IO [[SType]]
doQuery t kq cols outParams e = do
  let (whereQ,inParams) = second (map (inFun (kTys t))) $ compileQuery "key" kq
      sql = "SELECT " <> cols <> " FROM " <> tableName t <> " " <> whereQ <> " ORDER BY key"
  -- log e (show sql)
  qry (conn e) sql inParams outParams

getStmts :: SQLite -> Table k -> IO TableStmts
getStmts e t = case M.lookup (tableName t) (tableStmts e) of
  Just ss -> return ss
  Nothing -> throwDbError $ "No such table: " ++ show t

readData' :: FromJSON v => Table k -> k -> SQLite -> IO (Maybe v)
readData' t k e = do
  r <- getStmts e t >>= \s -> qrys (sRead s) [inFun (kTys t) k] [RText]
  case r of
    [] -> return Nothing
    _ -> expectSing "row" r >>= expectSing "column" >>= (fmap Just . decodeBlob)

writeData' :: ToJSON v => Table k -> WriteType -> k -> v -> SQLite -> IO ()
writeData' t wt k v e = do
  let ws = case wt of
        Write -> sInsertReplace
        Insert -> sInsert
        Update -> sReplace
  getStmts e t >>= \s -> execs (ws s) [inFun (kTys t) k,encodeBlob v]


initSQLite :: [Pragma] -> (String -> IO ()) -> FilePath -> IO SQLite
initSQLite ps logFn f = do
  c <- liftEither $ open (fromString f)
  ts <- TxStmts <$> prepStmt c "BEGIN TRANSACTION"
         <*> prepStmt c "COMMIT TRANSACTION"
         <*> prepStmt c "ROLLBACK TRANSACTION"
  let s = SQLite {
        conn = c,
        log = \msg -> logFn $ "[Persist-SQLite] " ++ msg,
        tableStmts = M.empty,
        txStmts = ts
        }
  runPragmas (conn s) ps
  return s


_test :: IO ()
_test = do
  let db = "log/test.sqlite"
  doesFileExist db >>= \b -> when b (removeFile db)
  e <- initSQLite [] putStrLn db
  let p = persister
      dt = DataTable "data"
      tt = TxTable "tx"
      run f = do
        s <- get
        (s',r) <- liftIO (f s)
        put s'
        return r
  (`evalStateT` e) $ do
    run $ beginTx p
    run $ createTable p dt
    run $ createTable p tt
    run $ commitTx p
    run $ beginTx p
    run $ writeValue p dt Insert "stuff" (String "hello")
    run $ writeValue p dt Insert "tough" (String "goodbye")
    run $ writeValue p tt Write 1 (String "txy goodness")
    run $ writeValue p tt Insert 2 (String "txalicious")
    run $ commitTx p
    run (readValue p dt "stuff") >>= (liftIO . (print :: Maybe Value -> IO ()))
    run (query p dt (Just (KQKey KEQ "stuff"))) >>=
      (liftIO . (print :: [(Text,Value)] -> IO ()))
    run (queryKeys p dt (Just (KQKey KGTE "stuff"))) >>= liftIO . print
    run (query p tt (Just (KQKey KGT 0 `kAnd` KQKey KLT 2))) >>=
      (liftIO . (print :: [(Int,Value)] -> IO ()))
    run $ beginTx p
    run $ writeValue p tt Update 2 (String "txalicious-2!")
    run (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
    run $ rollbackTx p
    run (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
