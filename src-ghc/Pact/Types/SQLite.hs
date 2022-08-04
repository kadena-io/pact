{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      :  Pact.Types.SQLite
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Types for SQLite usage.
--
module Pact.Types.SQLite
  ( SType(..), RType(..)
  , dbError
  , bindParams
  , liftEither
  , prepStmt
  , closeStmt
  , qry
  , qry_
  , qrys
  , qrys_
  , exec
  , exec_
  , execs
  , execs_
  , exec'
  , Pragma(..), runPragmas, fastNoJournalPragmas
  , SQLiteConfig (..), dbFile, pragmas
  ) where


import Control.Lens
import Control.Monad
import Database.SQLite3.Direct as SQ3
import Data.String
import qualified Data.ByteString as BS
import Data.Int
import Data.Aeson

import GHC.Generics

import Prelude
import Control.Monad.Catch


newtype Pragma = Pragma String deriving (Eq,Show,FromJSON,ToJSON,IsString)

data SQLiteConfig = SQLiteConfig
  { _dbFile :: FilePath
  , _pragmas :: [Pragma]
  } deriving (Eq,Show,Generic)
instance FromJSON SQLiteConfig
instance ToJSON SQLiteConfig
makeLenses ''SQLiteConfig

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

closeStmt :: Statement -> IO (Either String ())
closeStmt s = either (Left . show) Right <$> finalize s


-- | Prepare/execute query with params
qry :: Database -> Utf8 -> [SType] -> [RType] -> IO [[SType]]
qry e q as rts = bracket (prepStmt e q) finalize $ \stmt -> do
  bindParams stmt as
  reverse <$> stepStmt stmt rts
{-# INLINE qry #-}


-- | Prepare/execute query with no params
qry_ :: Database -> Utf8 -> [RType] -> IO [[SType]]
qry_ e q rts = bracket (prepStmt e q) finalize $ \stmt ->
  reverse <$> stepStmt stmt rts
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

-- | Execute query statement with no params
qrys_ :: Statement -> [RType] -> IO [[SType]]
qrys_ stmt rts = do
  clearBindings stmt
  rows <- stepStmt stmt rts
  void $ reset stmt
  return (reverse rows)
{-# INLINE qrys_ #-}

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
exec' e q as = bracket (prepStmt e q) finalize $ \stmt -> do
    bindParams stmt as
    r <- step stmt
    void $ liftEither (return r)
{-# INLINE exec' #-}

runPragmas :: Database -> [Pragma] -> IO ()
runPragmas c = mapM_ (\(Pragma s) -> exec_ c (fromString ("PRAGMA " ++ s)))



fastNoJournalPragmas :: [Pragma]
fastNoJournalPragmas = [
  "synchronous = OFF",
  "journal_mode = MEMORY",
  "locking_mode = EXCLUSIVE",
  "temp_store = MEMORY"
  ]
