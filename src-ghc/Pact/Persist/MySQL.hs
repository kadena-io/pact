{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Pact.Persist.MySQL
  ( DataKey(..)
  , beginTx'
  , dropRegressTables
  , FromResult(..)
  , initMySQL
  , persister
  , regressMSSQL
  , test
  , testConnectInfo
  , MySQL(..)
  , MySqlConfig(..)
  , parseConnectionString
  , Result(..)
  ) where

import Prelude hiding (log)
import Data.Aeson hiding (Result)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State
import Control.Exception
import Text.Read (readMaybe)
import Data.String (fromString)

import Database.MySQL.Simple hiding (query)
import qualified Database.MySQL.Simple as S
import Database.MySQL.Simple.QueryParams
import Database.MySQL.Simple.Param
import Database.MySQL.Simple.Result

import Pact.Persist
import Pact.Types.Pretty
import Pact.PersistPactDb.Regression
import Pact.Types.Logger (Logger,Loggers,alwaysLog,newLogger,logLog)
import Pact.Types.Persistence


data MySQL = MySQL
  { conn :: Connection
  , dataKeySize :: Int
  , logger :: Logger
  , loggers :: Loggers
  , config :: MySqlConfig
  }


data KeyFmt =
  DataKeyFmt DataKey |
  TxKeyFmt TxKey
instance Show KeyFmt where
  show (DataKeyFmt k) = show k
  show (TxKeyFmt k) = show k
instance Param KeyFmt where
  render (DataKeyFmt (DataKey k)) = render k
  render (TxKeyFmt (TxKey k)) = render k


toParam :: Table k -> k -> KeyFmt
toParam DataTable {} = DataKeyFmt
toParam TxTable {} = TxKeyFmt

class Result (ResTy k) => FromResult k where
  type ResTy k :: *
  fromResult :: Table k -> ResTy k -> k

instance FromResult DataKey where
  type ResTy DataKey = Text
  fromResult _ = DataKey
instance FromResult TxKey where
  type ResTy TxKey = Integer
  fromResult _ = TxKey

sanitize :: TableId -> Text
sanitize (TableId t) = T.replace "-" "_" t

tableName :: Table k -> String
tableName (DataTable t) = T.unpack $ sanitize t <> "_DATA"
tableName (TxTable t) = T.unpack $ sanitize t <> "_TX"


initMySQL :: MySqlConfig -> Loggers -> IO MySQL
initMySQL cfg@MySqlConfig { connectInfo } loggers_ = do
    c <- connect connectInfo
    autocommit c False
    return MySQL
      { conn = c
      , dataKeySize = 191 -- TODO move to config
                          -- MLN: changed this  fsfrom 1024 to 191 as some Linux distributions require a value less
                          -- than 767 bytes and this value apparently represents 4-byte blocks
      , logger = newLogger loggers_ "Persist-MySQL"
      , loggers = loggers_
      , config = cfg
      }

closeMySQL :: MySQL -> IO ()
closeMySQL e = catch (close (conn e)) (\(SomeException _) -> return ())

refresh :: MySQL -> IO MySQL
refresh e = do
  closeMySQL e
  initMySQL (config e) (loggers e)

log :: MySQL -> String -> String -> IO ()
log e = logLog (logger e)


persister :: Persister MySQL
persister = Persister {

  createTable = \t s -> (s,) <$> createTable' t s
  ,
  beginTx = \_t s -> return (s,())
  ,
  commitTx = \s -> commit (conn s) >> return (s,())
  ,
  rollbackTx = \s -> rollback (conn s) >> return (s,())
  ,
  queryKeys = \t kq s -> (s,) <$> queryKeys' t kq s
  ,
  query = \t kq s -> (s,) <$> query' t kq s
  ,
  readValue = \t k s -> (s,) <$> readData' t k s
  ,
  writeValue = \t wt k v s -> (s,) <$> writeData' t wt k v s
  ,
  refreshConn = \s -> (,()) <$> refresh s

  }



liftEither :: Show a => IO (Either a b) -> IO b
liftEither a = do
  er <- a
  case er of
    (Left e) -> throwDbError . prettyString $ show e
    (Right r) -> return r
{-# INLINE liftEither #-}

decodeBlob :: (FromJSON v) => ByteString -> IO v
decodeBlob t = liftEither (return $ eitherDecodeStrict' t)


encodeBlob :: ToJSON a => a -> ByteString
encodeBlob a = BSL.toStrict $ encode a
{-# INLINE encodeBlob #-}


beginTx' :: MySQL -> IO ()
beginTx' e = void $ execute (conn e) "start transaction" ()

createTable' :: Table k -> MySQL -> IO ()
createTable' t e = do
  log e "DDL" $ "createTable: " ++ show t
  let keysql :: String = case t of
        DataTable {} -> "VARCHAR(" <> show (dataKeySize e) <> ")"
        TxTable {} -> "BIGINT"
      sql :: String = "CREATE TABLE " ++ tableName t ++
            " (t_key " ++ keysql ++ " PRIMARY KEY, t_value TEXT)"
  log e "DDL" (show sql)
  void $ execute (conn e) (fromString sql) ()

query' :: (FromJSON v) => Table k -> Maybe (KeyQuery k) -> MySQL -> IO [(k,v)]
query' t kq e = do
  let (whereQ,inParams) = compileQuery "t_key" kq
      sql :: Query = fromString $ "SELECT t_key,t_value FROM " ++ tableName t ++ " " ++ whereQ
      params = map (toParam t) inParams
  case t of
    DataTable {} -> do
      rs <- S.query (conn e) sql params
      mapM (\(k,v) -> (,) <$> pure (DataKey k) <*> decodeBlob v) rs
    TxTable {} -> do
      rs <- S.query (conn e) sql params
      mapM (\(k,v) -> (,) <$> pure (TxKey k) <*> decodeBlob v) rs


queryKeys' :: Table k -> Maybe (KeyQuery k) -> MySQL -> IO [k]
queryKeys' t kq e = do
  let (whereQ,inParams) = compileQuery "t_key" kq
      sql :: Query = fromString $ "SELECT t_key FROM " ++ tableName t ++ " " ++ whereQ
      params = map (toParam t) inParams
  case t of
    DataTable {} -> do
      map (\(Only k) -> DataKey k) <$> S.query (conn e) sql params
    TxTable {} -> do
      map (\(Only k) -> TxKey k) <$> S.query (conn e) sql params


readData' :: FromJSON v => Table k -> k -> MySQL -> IO (Maybe v)
readData' t k e = do
  let sql :: Query = fromString $ "SELECT t_value FROM " ++ tableName t ++ " WHERE t_key = ?"
  rs <- S.query (conn e) sql [toParam t k]
  case rs of
    [] -> return Nothing
    [Only b] -> Just <$> decodeBlob b
    _ -> throwDbError $ prettyString $ "Expected single result, got: " ++ show rs



writeData' :: ToJSON v => Table k -> WriteType -> k -> v -> MySQL -> IO ()
writeData' t wt k v e = do
  let tn = tableName t
      vv = encodeBlob v
      ex :: QueryParams q => q -> String -> IO ()
      ex ps q = void $ execute (conn e) (fromString q) ps
      kp = toParam t k
  log e "DEBUG" $ "write: " ++ show (t,wt,kp,vv)
  case wt of
    Insert -> void $ ex (kp,vv) $ "INSERT " ++ tn ++ " VALUES (?,?)"
    Update -> void $ ex (vv,kp) $
      "UPDATE " ++ tn ++ " SET t_value = ? WHERE t_key = ?"
    Write -> void $ ex (kp,vv,vv) $
      "INSERT " ++ tn ++ " VALUES (?,?) ON DUPLICATE KEY UPDATE t_value=?"





regressMSSQL :: MySqlConfig -> IO ()
regressMSSQL cfg = do
  m <- initMySQL cfg alwaysLog
  handle (\(SomeException e) -> closeMySQL m >> throwIO e) $ do
    dropRegressTables m
    void $ runRegression (initDbEnv alwaysLog persister m)

dropRegressTables :: MySQL -> IO ()
dropRegressTables e = do
  ts <- S.query (conn e) "show tables" ()
  let startsWith s v = take (length v) s == v
      dropSql t = fromString $ "DROP TABLE IF EXISTS " ++ t
  forM_ ts $ \(Only t) -> when (t `startsWith` "USER_" || t `startsWith` "SYS_") $
    void $ execute (conn e) (dropSql t) ()


_drop :: MySqlConfig -> IO ()
_drop cfg = do
  e <- initMySQL cfg alwaysLog
  dropRegressTables e
  closeMySQL e


test :: MySqlConfig -> IO ()
test cfg = do
  e <- initMySQL cfg alwaysLog
  dropRegressTables e
  let p = persister
      dt = DataTable "USER_data"
      tt = TxTable "USER_tx"
      run' f = do
        s <- get
        (s',r) <- liftIO (f s)
        put s'
        return r
      here (i :: Int) = liftIO $ log e "DEBUG" $ "here: " ++ show i
  void $ closeMySQL e
  e' <- refresh e
  (`evalStateT` e') $ do
    run' $ beginTx p Transactional
    here 1
    run' $ createTable p dt
    here 2
    run' $ createTable p tt
    here 3
    run' $ commitTx p
    here 4
    run' $ beginTx p Transactional
    here 5
    run' $ writeValue p dt Insert "stuff" (String "hello")
    here 6
    run' $ writeValue p dt Insert "tough" (String "goodbye")
    here 7
    run' $ writeValue p tt Write 1 (String "txy goodness")
    here 8
    run' $ writeValue p tt Insert 2 (String "txalicious")
    here 9
    run' $ commitTx p
    here 10
    run' (readValue p dt "stuff") >>= (liftIO . (print :: Maybe Value -> IO ()))
    here 11
    run' (query p dt (Just (KQKey KEQ "stuff"))) >>=
      (liftIO . (print :: [(DataKey,Value)] -> IO ()))
    here 12
    run' (queryKeys p dt (Just (KQKey KGTE "stuff"))) >>= liftIO . print
    here 13
    run' (query p tt (Just (KQKey KGT 0 `kAnd` KQKey KLT 2))) >>=
      (liftIO . (print :: [(TxKey,Value)] -> IO ()))
    run' $ beginTx p Transactional
    run' $ writeValue p tt Update 2 (String "txalicious-2!")
    run' (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
    run' $ rollbackTx p
    run' (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))

newtype MySqlConfig = MySqlConfig {
  connectInfo :: ConnectInfo
}

parseConnectionString :: Text -> Either Text MySqlConfig
parseConnectionString s = do
  let
    parseAttribute s' =
      let (name, val) = T.breakOn "=" s'
      in fmap (\val' -> (name, T.unpack val')) (T.stripPrefix "=" val)

    attributes :: Map.Map Text String
    attributes =
      Map.fromList $ catMaybes $ fmap parseAttribute $ T.splitOn ";" s

    -- Helper function for annotating Maybes with an error message.
    note e a = maybe (Left e) Right a

  connectPort <- case Map.lookup "port" attributes of
    Nothing -> Right (connectPort defaultConnectInfo)
    Just v  -> note ("Could not parse " <> T.pack v <> " as port") (readMaybe v)

  return $ MySqlConfig $ ConnectInfo
    { connectHost = fromMaybe "localhost" $ Map.lookup "host" attributes
    , connectPort
    , connectUser = fromMaybe "root" $ Map.lookup "user" attributes
    , connectPassword = fromMaybe "" $ Map.lookup "password" attributes
    , connectDatabase = fromMaybe "main" $ Map.lookup "database" attributes
    , connectOptions = [] -- TODO: Parse this.
    , connectPath = "" -- TODO: Parse this.
    , connectSSL = Nothing -- TODO: Parse this somehow.
    }


testConnectInfo :: ConnectInfo
testConnectInfo = defaultConnectInfo
  { connectPassword="kadena", connectUser="pact", connectDatabase="pact" }
