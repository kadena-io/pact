{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}



module Pact.Persist.Pure
  (
    PValue (..),
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
import Control.Monad.Reader ()
import Control.Monad.State
import Data.Default
import Data.Typeable

import Pact.Persist hiding (compileQuery)

data PValue = forall a . PactValue a => PValue a
instance Show PValue where show (PValue a) = show a


newtype Tbl k = Tbl {
  _tbl :: M.Map k PValue
  } deriving (Show,Monoid)
--makeLenses ''Tbl
tbl ::
  forall k_a7x7L k_a7xbw.
  Iso (Tbl k_a7x7L) (Tbl k_a7xbw) (M.Map k_a7x7L PValue) (M.Map k_a7xbw PValue)
tbl = iso (\ (Tbl x_a7xbx) -> x_a7xbx) Tbl
{-# INLINE tbl #-}

newtype Tables k = Tables {
  _tbls :: M.Map (Table k) (Tbl k)
  } deriving (Show,Monoid)
--makeLenses ''Tables
tbls ::
  forall k_a7xbC k_a7xfj.
  Iso (Tables k_a7xbC) (Tables k_a7xfj) (M.Map (Table k_a7xbC) (Tbl k_a7xbC)) (M.Map (Table k_a7xfj) (Tbl k_a7xfj))
tbls = iso (\ (Tables x_a7xfk) -> x_a7xfk) Tables
{-# INLINE tbls #-}


data Db = Db {
  _dataTables :: !(Tables DataKey),
  _txTables :: !(Tables TxKey)
  } deriving (Show)
instance Default Db where def = Db mempty mempty
--makeLenses ''Db
dataTables :: Lens' Db (Tables DataKey)
dataTables f_a7xhg (Db x1_a7xhh x2_a7xhi)
  = fmap (\ y1_a7xhj -> Db y1_a7xhj x2_a7xhi) (f_a7xhg x1_a7xhh)
{-# INLINE dataTables #-}
txTables :: Lens' Db (Tables TxKey)
txTables f_a7xhk (Db x1_a7xhl x2_a7xhm)
  = fmap (\ y1_a7xhn -> Db x1_a7xhl y1_a7xhn) (f_a7xhk x2_a7xhm)
{-# INLINE txTables #-}

tblType :: Table k -> Lens' Db (Tables k)
tblType DataTable {} = dataTables
tblType TxTable {} = txTables

data PureDb = PureDb {
  _committed :: !Db,
  _temp :: !Db
  }
instance Default PureDb where def = PureDb def def
--makeLenses ''PureDb
committed :: Lens' PureDb Db
committed f_a7xja (PureDb x1_a7xjb x2_a7xjc)
  = fmap (\ y1_a7xjd -> PureDb y1_a7xjd x2_a7xjc) (f_a7xja x1_a7xjb)
{-# INLINE committed #-}
temp :: Lens' PureDb Db
temp f_a7xje (PureDb x1_a7xjf x2_a7xjg)
  = fmap (\ y1_a7xjh -> PureDb x1_a7xjf y1_a7xjh) (f_a7xje x2_a7xjg)
{-# INLINE temp #-}

initPureDb :: PureDb
initPureDb = def

overM :: s -> Lens' s a -> (a -> IO a) -> IO s
overM s l f = f (view l s) >>= \a -> return (set l a s)
{-# INLINE overM #-}

persister :: Persister PureDb
persister = Persister {

  createTable = \t s -> fmap (,()) $ overM s (temp . tblType t . tbls) $ \ts -> case M.lookup t ts of
      Nothing -> return (M.insert t mempty ts)
      Just _ -> throwDbError $ "createTable: already exists: " ++ show t
  ,
  beginTx = \_ s -> return $ (,()) $ set temp (_committed s) s
  ,
  commitTx = \s -> return $ (,()) $ set committed (_temp s) s
  ,
  rollbackTx = \s -> return $ (,()) $ set temp (_committed s) s
  ,
  queryKeys = \t kq s -> ((s,) . map fst) <$> qry t kq s
  ,
  query = \t kq s -> fmap (s,) $ qry t kq s >>= mapM (\(k,v) -> (k,) <$> conv v)
  ,
  readValue = \t k s -> fmap (s,) $ traverse conv $ firstOf (temp . tblType t . tbls . ix t . tbl . ix k) s
  ,
  writeValue = \t wt k v s -> fmap (,()) $ overM s (temp . tblType t . tbls) $ \ts -> case M.lookup t ts of
      Nothing -> throwDbError $ "writeValue: no such table: " ++ show t
      Just tb -> fmap (\nt -> M.insert t nt ts) $ overM tb tbl $ \m -> case (M.lookup k m,wt) of
        (Just _,Insert) -> throwDbError $ "Insert: value already at key: " ++ show k
        (Nothing,Update) -> throwDbError $ "Update: no value at key: " ++ show k
        _ -> return $ M.insert k (PValue v) m
  ,
  refreshConn = return . (,())
  }


compileQuery :: PactKey k => Maybe (KeyQuery k) -> (k -> Bool)
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

qry :: PactKey k => Table k -> Maybe (KeyQuery k) -> PureDb -> IO [(k,PValue)]
qry t kq s = case firstOf (temp . tblType t . tbls . ix t . tbl) s of
  Nothing -> throwDbError $ "query: no such table: " ++ show t
  Just m -> return $ filter (compileQuery kq . fst) $ M.toList m
{-# INLINE qry #-}


conv :: PactValue v => PValue -> IO v
conv (PValue v) = case cast v of
  Nothing -> throwDbError $ "Failed to reify DB value: " ++ show v
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
    run $ beginTx p True
    run $ createTable p dt
    run $ createTable p tt
    run $ commitTx p
    run $ beginTx p True
    run $ writeValue p dt Insert "stuff" (String "hello")
    run $ writeValue p dt Insert "tough" (String "goodbye")
    run $ writeValue p tt Write 1 (String "txy goodness")
    run $ writeValue p tt Insert 2 (String "txalicious")
    run $ commitTx p
    run (readValue p dt "stuff") >>= (liftIO . (print :: Maybe Value -> IO ()))
    run (query p dt (Just (KQKey KEQ "stuff"))) >>=
      (liftIO . (print :: [(DataKey,Value)] -> IO ()))
    run (queryKeys p dt (Just (KQKey KGTE "stuff"))) >>= liftIO . print
    run (query p tt (Just (KQKey KGT 0 `kAnd` KQKey KLT 2))) >>=
      (liftIO . (print :: [(TxKey,Value)] -> IO ()))
    run $ beginTx p True
    run $ writeValue p tt Update 2 (String "txalicious-2!")
    run (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
    run $ rollbackTx p
    run (readValue p tt 2) >>= (liftIO . (print :: Maybe Value -> IO ()))
