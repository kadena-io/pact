{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Pact.Persist.Pure
  (
    Tbl(..),tbl,
    Tables(..),tbls,tblType,
    Db(..),dataTables,txTables,
    PureDb(..),committed,temp,
    initPureDb,
    persister
  ) where

import qualified Data.Map.Strict as M
import Control.Lens hiding (op)
import Data.Aeson
import qualified Data.ByteString as B
import Control.Monad.Reader ()
import Control.Monad.State
import Data.Default

import Pact.Types.Persistence
import Pact.Persist hiding (compileQuery)
import Pact.Types.Pretty

import qualified Pact.JSON.Encode as J

-- data PValue = forall a . (PactDbValue a) => PValue a
-- instance Show PValue where show (PValue a) = show a


newtype Tbl k = Tbl {
  _tbl :: M.Map k B.ByteString
  } deriving (Show,Semigroup,Monoid)
makeLenses ''Tbl

newtype Tables k = Tables {
  _tbls :: M.Map (Table k) (Tbl k)
  } deriving (Show,Semigroup,Monoid)
makeLenses ''Tables


data Db = Db {
  _dataTables :: !(Tables DataKey),
  _txTables :: !(Tables TxKey)
  } deriving (Show)
makeLenses ''Db
instance Default Db where def = Db mempty mempty

tblType :: Table k -> Lens' Db (Tables k)
tblType DataTable {} = dataTables
tblType TxTable {} = txTables

data PureDb = PureDb {
  _committed :: !Db,
  _temp :: !Db
  }
makeLenses ''PureDb
instance Default PureDb where def = PureDb def def

initPureDb :: PureDb
initPureDb = def

overM :: s -> Lens' s a -> (a -> IO a) -> IO s
-- overM s l f = f (view l s) >>= \a -> return (set l a s)
overM s l f = l f s
{-# INLINE overM #-}

persister :: Persister PureDb
persister = Persister {

  createTable = \t s -> fmap (,()) $ overM s (temp . tblType t . tbls) $ \ts -> case M.lookup t ts of
      Nothing -> return (M.insert t mempty ts)
      Just _ -> throwDbError $ "createTable: already exists: " <> pretty t
  ,
  beginTx = \_ s -> return $ (,()) $ set temp (_committed s) s
  ,
  commitTx = \s -> return $ (,()) $ set committed (_temp s) s
  ,
  rollbackTx = \s -> return $ (,()) $ set temp (_committed s) s
  ,
  queryKeys = \t kq s -> (s,) . map fst <$> qry t kq s
  ,
  query = \t kq s -> fmap (s,) $ qry t kq s >>= mapM (\(k,v) -> (k,) <$> conv v)
  ,
  readValue = \t k s -> fmap (s,) $ traverse conv $ firstOf (temp . tblType t . tbls . ix t . tbl . ix k) s
  ,
  -- writeValue :: forall k . (PactDbKey k) => Table k -> WriteType -> k -> B.ByteString -> Persist s ()
  writeValue = \t wt k v s -> fmap (,()) $ overM s (temp . tblType t . tbls) $ \ts -> case M.lookup t ts of
      Nothing -> throwDbError $ "writeValue: no such table: " <> pretty t
      Just tb -> fmap (\nt -> M.insert t nt ts) $ overM tb tbl $ \m -> case (M.lookup k m,wt) of
        (Just _,Insert) -> throwDbError $ "Insert: value already at key: " <> pretty k
        (Nothing,Update) -> throwDbError $ "Update: no value at key: " <> pretty k
        _ -> return $ M.insert k v m
  ,
  refreshConn = return . (,())
  }


compileQuery :: PactDbKey k => Maybe (KeyQuery k) -> (k -> Bool)
compileQuery Nothing = const True
compileQuery (Just kq) = compile kq
  where
    compile (KQKey cmp k) = (`op` k)
      where op = case cmp of
              KGT -> (>)
              KGTE -> (>=)
              KEQ -> (==)
              KNEQ -> (/=)
              KLT -> (<)
              KLTE -> (<=)
    compile (KQConj l o r) = conj o <$> compile l <*> compile r
    conj AND = (&&)
    conj OR = (||)
{-# INLINE compileQuery #-}

qry :: PactDbKey k => Table k -> Maybe (KeyQuery k) -> PureDb -> IO [(k,B.ByteString)]
qry t kq s = case firstOf (temp . tblType t . tbls . ix t . tbl) s of
  Nothing -> throwDbError $ "query: no such table: " <> pretty t
  Just m -> return $ filter (compileQuery kq . fst) $ M.toList m
{-# INLINE qry #-}


conv :: forall v . (FromJSON v) => B.ByteString -> IO v
conv v = case decodeStrict v of
  Nothing -> throwDbError $
       "FIXME"
  --   "Failed to reify DB value: " <> prettyPactDbValue v <> " as " <> pretty (show (typeRep (Proxy @v)))
  Just s -> return s
{-# INLINE conv #-}




_test :: IO ()
_test = do
  let e :: PureDb = def
  let p = persister
      dt = DataTable "data"
      tt = TxTable "tx"
      run f = do
        s <- get
        (s',r) <- liftIO (f s)
        put s'
        return r
  (`evalStateT` e) $ do
    run $ beginTx p Transactional
    run $ createTable p dt
    run $ createTable p tt
    run $ commitTx p
    run $ beginTx p Transactional
    run $ writeValue p dt Insert "stuff" (J.encodeStrict $ J.text "hello")
    run $ writeValue p dt Insert "tough" (J.encodeStrict $ J.text "goodbye")
    run $ writeValue p tt Write 1 (J.encodeStrict $ J.text "txy goodness")
    run $ writeValue p tt Insert 2 (J.encodeStrict $ J.text "txalicious")
    run $ commitTx p
    run (readValue p dt "stuff") >>= (liftIO . (print :: Maybe Value -> IO ()))
    run (query p dt (Just (KQKey KEQ "stuff"))) >>=
      (liftIO . (print :: [(DataKey,Value)] -> IO ()))
    run (queryKeys p dt (Just (KQKey KGTE "stuff"))) >>= liftIO . print
    run (query p tt (Just (KQKey KGT 0 `kAnd` KQKey KLT 2))) >>=
      (liftIO . (print :: [(TxKey,Value)] -> IO ()))
    run $ beginTx p Transactional
    run $ writeValue p tt Update 2 (J.encodeStrict $ J.text "txalicious-2!")
    run (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
    run $ rollbackTx p
    run (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
