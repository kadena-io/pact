{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Pact.Types.SQLite
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Types for SQLite Pact backend.
--
module Pact.Types.SQLite
  ( SType(..), RType(..)
  , dbError
  , bindParams
  , liftEither
  , prepStmt
  , qry
  , qry_
  , qrys
  , exec
  , exec_
  , execs
  , execs_
  , exec'
  , TableStmts(..)
  , TxStmts(..)
  , SysCache(..), cachedKeySets, cachedModules, cachedTableInfo, cachedUserTables
  , PSL(..), conn, log, txRecord, tableStmts, txStmts, tmpSysCache, sysCache
  ) where

import Database.SQLite3.Direct as SQ3
import Control.Lens
import Data.String
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Default
import Prelude hiding (log)
import Control.Monad
import Data.Int

import Prelude hiding (log)
import Control.Monad.Catch

import Pact.Types.Runtime

-- | Statement input types
data SType = SInt Int64 | SDouble Double | SText Utf8 | SBlob BS.ByteString deriving (Eq,Show)
-- | Result types
data RType = RInt | RDouble | RText | RBlob deriving (Eq,Show)

dbError :: String -> IO a
dbError = throwM . userError . ("Database error: " ++)


bindParams :: Statement -> [SType] -> IO ()
bindParams stmt as =
    void $ liftEither
    (sequence <$> forM (zip as [1..]) ( \(a,i) -> do
      case a of
        SInt n -> bindInt64 stmt i n
        SDouble n -> bindDouble stmt i n
        SText n -> bindText stmt i n
        SBlob n -> bindBlob stmt i n))
{-# INLINE bindParams #-}


liftEither :: Show a => IO (Either a b) -> IO b
liftEither a = do
  er <- a
  case er of
    (Left e) -> dbError (show e)
    (Right r) -> return r
{-# INLINE liftEither #-}


prepStmt :: Database -> Utf8 -> IO Statement
prepStmt c q = do
    r <- prepare c q
    case r of
      Left e -> dbError (show e)
      Right Nothing -> dbError "Statement prep failed"
      Right (Just s) -> return s


-- | Prepare/execute query with params
qry :: Database -> Utf8 -> [SType] -> [RType] -> IO [[SType]]
qry e q as rts = do
  stmt <- prepStmt e q
  bindParams stmt as
  rows <- stepStmt stmt rts
  void $ finalize stmt
  return (reverse rows)
{-# INLINE qry #-}


-- | Prepare/execute query with no params
qry_ :: Database -> Utf8 -> [RType] -> IO [[SType]]
qry_ e q rts = do
            stmt <- prepStmt e q
            rows <- stepStmt stmt rts
            _ <- finalize stmt
            return (reverse rows)
{-# INLINE qry_ #-}

-- | Execute query statement with params
qrys :: Statement -> [SType] -> [RType] -> IO [[SType]]
qrys stmt as rts = do
  clearBindings stmt
  bindParams stmt as
  rows <- stepStmt stmt rts
  void $ reset stmt
  return (reverse rows)
{-# INLINE qrys #-}


stepStmt :: Statement -> [RType] -> IO [[SType]]
stepStmt stmt rts = do
  let acc rs Done = return rs
      acc rs Row = do
        as <- forM (zip rts [0..]) $ \(rt,ci) -> do
                      case rt of
                        RInt -> SInt <$> columnInt64 stmt ci
                        RDouble -> SDouble <$> columnDouble stmt ci
                        RText -> SText <$> columnText stmt ci
                        RBlob -> SBlob <$> columnBlob stmt ci
        sr <- liftEither $ step stmt
        acc (as:rs) sr
  sr <- liftEither $ step stmt
  acc [] sr
{-# INLINE stepStmt #-}

-- | Exec statement with no params
execs_ :: Statement -> IO ()
execs_ s = do
  r <- step s
  void $ reset s
  void $ liftEither (return r)
{-# INLINE execs_ #-}


-- | Exec statement with params
execs :: Statement -> [SType] -> IO ()
execs stmt as = do
    clearBindings stmt
    bindParams stmt as
    r <- step stmt
    void $ reset stmt
    void $ liftEither (return r)
{-# INLINE execs #-}

-- | Prepare/exec statement with no params
exec_ :: Database -> Utf8 -> IO ()
exec_ e q = liftEither $ SQ3.exec e q
{-# INLINE exec_ #-}


-- | Prepare/exec statement with params
exec' :: Database -> Utf8 -> [SType] -> IO ()
exec' e q as = do
             stmt <- prepStmt e q
             bindParams stmt as
             r <- step stmt
             void $ finalize stmt
             void $ liftEither (return r)
{-# INLINE exec' #-}

data TableStmts = TableStmts {
      sInsertReplace :: Statement
    , sInsert :: Statement
    , sReplace :: Statement
    , sRead :: Statement
    , sRecordTx :: Statement
}

data TxStmts = TxStmts {
      tBegin :: Statement
    , tCommit :: Statement
    , tRollback :: Statement
}

data SysCache = SysCache {
      _cachedKeySets :: HM.HashMap String KeySet
    , _cachedModules :: HM.HashMap String Module
    , _cachedTableInfo :: HM.HashMap TableName (ModuleName,KeySetName)
    , _cachedUserTables :: HM.HashMap TableName (HM.HashMap String (Columns Persistable))
} deriving (Show)
makeLenses ''SysCache

instance Default SysCache where def = SysCache HM.empty HM.empty HM.empty HM.empty

data PSL = PSL {
      _conn :: Database
    , _log :: forall s . Show s => (String -> s -> IO ())
    , _txRecord :: M.Map Utf8 [TxLog]
    , _tableStmts :: M.Map Utf8 TableStmts
    , _txStmts :: TxStmts
    , _tmpSysCache :: SysCache
    , _sysCache :: SysCache
}
makeLenses ''PSL
