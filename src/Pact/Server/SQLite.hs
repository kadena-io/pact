{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

-- |
-- Module      :  Pact.Server.SQLite
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- SQLite backend for Pact service.
--

module Pact.Server.SQLite
  ( DbEnv(..),SQLite(..),initPSL, fastNoJournalPragmas
  ) where

import Prelude hiding (log)

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad

import Data.Aeson hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import Data.Text.Encoding
import qualified Data.Text as T

import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM

import Criterion hiding (env)
import System.CPUTime
import System.Directory

import Pact.Types.Runtime hiding ((<>))
import Pact.PersistPactDb
import Pact.Persist.SQLite hiding (log)
import Pact.Compile
import Pact.Eval
import Pact.Native (initEvalEnv)



initPSL :: [Pragma] -> (String -> IO ()) -> FilePath -> IO (DbEnv SQLite)
initPSL ps logFn f = do
  c <- initSQLite ps logFn f
  return DbEnv {
    _db = c,
    _persist = persister,
    _log = \m s -> logFn $ m ++ ": " ++ show s,
    _txRecord = M.empty,
    _txId = def }

fastNoJournalPragmas :: [Pragma]
fastNoJournalPragmas = [
  "synchronous = OFF",
  "journal_mode = MEMORY",
  "locking_mode = EXCLUSIVE",
  "temp_store = MEMORY"
  ]

_initPSL :: IO (DbEnv SQLite)
_initPSL = do
  let f = "deleteme.sqllite"
  doesFileExist f >>= \b -> when b (removeFile f)
  initPSL fastNoJournalPragmas putStrLn f

_run :: (MVar (DbEnv SQLite) -> IO ()) -> IO ()
_run a = do
  m <- _initPSL
  s <- newMVar m
  a s
  -- void $ close (_conn m)

_test1 :: IO ()
_test1 =
    _run $ \e -> do
      t <- getCPUTime
      createSchema e
      begin e
      createUserTable' e "stuff" "module" "keyset"
      -- withMVar e $ \m -> qry_ (_conn m) "select * from usertables" [RText,RText,RText] >>= print
      void $ commit' e
      begin e
      print . _txId =<< readMVar e
      print =<< _getUserTableInfo pactdb "stuff" e
      _writeRow pactdb Insert (UserTables "stuff") "key1"
               (Columns (M.fromList [("gah",PLiteral (LDecimal 123.454345))])) e
      print =<< _readRow pactdb (UserTables "stuff") "key1" e
      _writeRow pactdb Update (UserTables "stuff") "key1"
               (Columns (M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)])) e
      print =<< _readRow pactdb (UserTables "stuff") "key1" e
      _writeRow pactdb Write KeySets "ks1"
               (KeySet [PublicKey "frah"] "stuff") e
      print =<< _readRow pactdb KeySets "ks1" e
      _writeRow pactdb Write Modules "mod1"
               (Module "mod1" "mod-admin-keyset" Nothing "code") e
      print =<< _readRow pactdb Modules "mod1" e
      void $ commit' e
      tids <- _txids pactdb "stuff" (pred $ fromIntegral t) e
      print tids
      print =<< _getTxLog pactdb (UserTables "stuff") (head tids) e

begin :: MVar (DbEnv p) -> IO ()
begin e = do
  t <- Just . fromIntegral <$> getCPUTime
  _beginTx pactdb t e

commit' :: MVar (DbEnv p) -> IO ()
commit' e = _commitTx pactdb e

_bench :: IO ()
_bench = _run $ \e -> do
  begin e
  createSchema e
  _createUserTable pactdb "stuff" "module" "keyset" e
  void $ commit' e
  nolog e
  benchmark $ whnfIO $ do
       begin e
       _writeRow pactdb Write (UserTables "stuff") "key1"
                (Columns (M.fromList [("gah",PLiteral (LDecimal 123.454345))])) e
       void $ _readRow pactdb (UserTables "stuff") "key1" e
       _writeRow pactdb Update (UserTables "stuff") "key1"
                (Columns (M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)])) e
       r <- _readRow pactdb  (UserTables "stuff") "key1" e
       void $ commit' e
       return r
  benchmark $ whnfIO $ do
       begin e
       _writeRow pactdb Update (UserTables "stuff") "key1"
                (Columns (M.fromList [("gah",PLiteral (LBool False)),("fh",PValue Null)])) e
       commit' e

nolog :: MVar (DbEnv p) -> IO ()
nolog e = modifyMVar_ e $ \m -> return $ m { _log = \_ _ -> return () }

parseCompile :: T.Text -> [Term Name]
parseCompile code = compiled where
    (Right es) = AP.parseOnly exprs code
    (Right compiled) = mapM (compile mkEmptyInfo) es

_pact :: Bool -> IO ()
_pact doBench = do
      m <- _initPSL
      let body = object ["keyset" A..= object ["keys" A..= ["demoadmin" :: T.Text], "pred" A..= (">" :: T.Text)]]
      evalEnv <- set eeMsgBody body <$> initEvalEnv m pactdb
      e <- return (_eePactDbVar evalEnv)
      cf <- BS.readFile "demo/demo.pact"
      begin e
      createSchema e
      void $ commit' e
      (r,es) <- runEval def evalEnv $ do
          evalBeginTx def
          rs <- mapM eval (parseCompile $ decodeUtf8 cf)
          evalCommitTx def
          return rs
      print r
      let evalEnv' = over (eeRefStore.rsModules) (HM.union (HM.fromList (_rsNew (_evalRefs es)))) evalEnv
          pactBench benchterm = do
                                tid <- Just . fromIntegral <$> getCPUTime
                                er <- runEval def (set eeTxId tid evalEnv') $ do
                                      evalBeginTx def
                                      r' <- eval (head benchterm)
                                      evalCommitTx def
                                      return r'
                                return (fst er)
          pactSimple = fmap fst . runEval def evalEnv' . eval . head . parseCompile
          benchy :: Show a => String -> IO a -> IO ()
          benchy n a | doBench = putStr n >> putStr " " >> benchmark (whnfIO a)
                     | otherwise = putStrLn "===========" >> putStrLn (n ++ ": ") >> a >>= print
      when doBench $ nolog e
      benchy "read-account no tx" $ pactSimple "(demo.read-account \"Acct1\")"
      benchy "read-account tx" $ pactBench $ parseCompile "(demo.read-account \"Acct1\")"
      benchy "_readRow tx" $ do
                   begin e
                   rr <- _readRow pactdb (UserTables "demo-accounts") "Acct1" e
                   void $ commit' e
                   return rr
      benchy "_readRow no tx" $ runEval def evalEnv' $ readRow def (UserTables "demo-accounts") "Acct1"
      benchy "describe-table" $ pactSimple "(describe-table 'demo-accounts)"
      benchy "transfer" $ pactBench  $ parseCompile "(demo.transfer \"Acct1\" \"Acct2\" 1.0)"
      benchy "_getUserTableInfo" $ _getUserTableInfo pactdb "demo-accounts" e
      benchy "_writeRow tx" $ do
                   begin e
                   rr <- _writeRow pactdb Update (UserTables "demo-accounts") "Acct1"
                         (Columns (M.fromList [("balance",PLiteral (LDecimal 1000.0)),
                                               ("amount",PLiteral (LDecimal 1000.0)),
                                               ("data",PLiteral (LString "Admin account funding"))]))
                         e
                   void $ commit' e
                   return rr
      benchy "fund-account tx" $ pactBench $ parseCompile "(demo.fund-account \"Acct1\" 1000.0)"
