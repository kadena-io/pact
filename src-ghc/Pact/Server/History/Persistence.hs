{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pact.Server.History.Persistence
  ( createDB
  , insertCompletedCommand
  , queryForExisting
  , selectCompletedCommands
  , selectAllCommands
  ) where


import Control.Monad

import qualified Data.Text as T
import qualified Data.Aeson as A
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

import Data.List (sortBy)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe

import Database.SQLite3.Direct

import Pact.Types.Command
import Pact.Types.Runtime
import Pact.Types.SQLite

import Pact.Server.History.Types

hashToField :: Hash -> SType
hashToField h = SText $ Utf8 $ BSL.toStrict $ A.encode h

hashFromField :: ByteString -> Hash
hashFromField h = case A.eitherDecodeStrict' h of
  Left err -> error $ "hashFromField: unable to decode Hash from database! " ++ show err ++ " => " ++ show h
  Right v -> v

crToField :: A.Value -> SType
crToField r = SText $ Utf8 $ BSL.toStrict $ A.encode r

crFromField :: RequestKey -> Maybe TxId -> ByteString -> CommandResult
crFromField rk tid cr = CommandResult rk tid v
  where
    v = case A.eitherDecodeStrict' cr of
      Left err -> error $ "crFromField: unable to decode CommandResult from database! " ++ show err ++ "\n" ++ show cr
      Right v' -> v'

userSigsToField :: [UserSig] -> SType
userSigsToField us = SText $ Utf8 $ BSL.toStrict $ A.encode us

userSigsFromField :: ByteString -> [UserSig]
userSigsFromField us = case A.eitherDecodeStrict' us of
  Left err -> error $ "userSigsFromField: unable to decode [UserSigs] from database! " ++ show err ++ "\n" ++ show us
  Right v -> v

sqlDbSchema :: Utf8
sqlDbSchema =
  "CREATE TABLE IF NOT EXISTS 'main'.'pactCommands' \
  \( 'hash' TEXT PRIMARY KEY NOT NULL UNIQUE\
  \, 'txid' INTEGER NOT NULL\
  \, 'command' TEXT NOT NULL\
  \, 'result' TEXT NOT NULL\
  \, 'userSigs' TEXT NOT NULL\
  \)"

eitherToError :: Show e => String -> Either e a -> a
eitherToError _ (Right v) = v
eitherToError s (Left e) = error $ "SQLite Error in History exec: " ++ s ++ "\nWith Error: "++ show e

createDB :: FilePath -> IO DbEnv
createDB f = do
  conn' <- eitherToError "OpenDB" <$> open (Utf8 $ encodeUtf8 $ T.pack f)
  eitherToError "CreateTable" <$> exec conn' sqlDbSchema
--  eitherToError "pragmas" <$> exec conn "PRAGMA locking_mode = EXCLUSIVE"
  DbEnv <$> pure conn'
        <*> prepStmt conn' sqlInsertHistoryRow
        <*> prepStmt conn' sqlQueryForExisting
        <*> prepStmt conn' sqlSelectCompletedCommands
        <*> prepStmt conn' sqlSelectAllCommands

sqlInsertHistoryRow :: Utf8
sqlInsertHistoryRow =
    "INSERT INTO 'main'.'pactCommands' \
    \( 'hash'\
    \, 'txid' \
    \, 'command'\
    \, 'result'\
    \, 'userSigs'\
    \) VALUES (?,?,?,?,?)"

insertRow :: Statement -> (Command ByteString, CommandResult) -> IO ()
insertRow s (Command{..},CommandResult {..}) =
    execs s [hashToField _cmdHash
            ,SInt $ fromIntegral (fromMaybe (-1) _crTxId)
            ,SText $ Utf8 _cmdPayload
            ,crToField _crResult
            ,userSigsToField _cmdSigs]

insertCompletedCommand :: DbEnv -> [(Command ByteString, CommandResult)] -> IO ()
insertCompletedCommand DbEnv{..} v = do
  let sortCmds (_,cr1) (_,cr2) = compare (_crTxId cr1) (_crTxId cr2)
  eitherToError "start insert transaction" <$> exec _conn "BEGIN TRANSACTION"
  mapM_ (insertRow _insertStatement) $ sortBy sortCmds v
  eitherToError "end insert transaction" <$> exec _conn "END TRANSACTION"

sqlQueryForExisting :: Utf8
sqlQueryForExisting = "SELECT EXISTS(SELECT 1 FROM 'main'.'pactCommands' WHERE hash=:hash LIMIT 1)"

queryForExisting :: DbEnv -> HashSet RequestKey -> IO (HashSet RequestKey)
queryForExisting e v = foldM f v v
  where
    f s rk = do
      r <- qrys (_qryExistingStmt e) [hashToField $ unRequestKey rk] [RInt]
      case r of
        [[SInt 1]] -> return s
        _ -> return $ HashSet.delete rk s

sqlSelectCompletedCommands :: Utf8
sqlSelectCompletedCommands =
  "SELECT result,txid FROM 'main'.'pactCommands' WHERE hash=:hash LIMIT 1"

selectCompletedCommands :: DbEnv -> HashSet RequestKey -> IO (HashMap RequestKey CommandResult)
selectCompletedCommands e v = foldM f HashMap.empty v
  where
    f m rk = do
      rs <- qrys (_qryCompletedStmt e) [hashToField $ unRequestKey rk] [RText,RInt]
      if null rs
      then return m
      else case head rs of
          [SText (Utf8 cr),SInt tid] ->
            return $ HashMap.insert rk (crFromField rk (if tid < 0 then Nothing else Just (fromIntegral tid)) cr) m
          r -> dbError $ "Invalid result from query: " ++ show r

sqlSelectAllCommands :: Utf8
sqlSelectAllCommands = "SELECT txid,hash,command,userSigs FROM 'main'.'pactCommands' ORDER BY txid ASC"

selectAllCommands :: DbEnv -> IO [Command ByteString]
selectAllCommands e = do
  let rowToCmd [_, SText (Utf8 hash'),SText (Utf8 cmd'),SText (Utf8 userSigs')] =
              Command { _cmdPayload = cmd'
                      , _cmdSigs = userSigsFromField userSigs'
                      , _cmdHash = hashFromField hash'}
      rowToCmd err = error $ "During selectAllCommands, we encountered a non-SText type: " ++ show err
  fmap rowToCmd <$> qrys_ (_qrySelectAllCmds e) [RInt,RText,RText,RText]
