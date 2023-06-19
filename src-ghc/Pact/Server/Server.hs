{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- |
-- Module      :  Pact.Server.Server
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Launch pact dev server.

module Pact.Server.Server
  ( serve
  , serveLocal
  , withTestServe
  , Port
  ) where

import Control.Concurrent
import Control.Concurrent.Async (async, link, withAsync)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Word
import qualified Data.Yaml as Y

import GHC.Generics

import qualified Pact.JSON.Encode as J
import Pact.Server.ApiServer
import Pact.Server.History.Service
import Pact.Server.PactService
import Pact.Types.Command
import Pact.Types.Logger
import Pact.Types.Runtime hiding (Update)
import Pact.Types.Server
import Pact.Types.SPV
import Pact.Types.SQLite hiding (_pragmas)

import System.Directory (createDirectoryIfMissing)
import System.Log.FastLogger

data Config = Config {
  _port :: Word16,
  _persistDir :: Maybe FilePath,
  _logDir :: FilePath,
  _pragmas :: [Pragma],
  _verbose :: Bool,
  _entity :: Maybe EntityName,
  _gasLimit :: Maybe Int,
  _gasRate :: Maybe Int,
  _execConfig :: Maybe ExecutionConfig
  } deriving (Eq,Show,Generic)

instance FromJSON Config where
  parseJSON = lensyParseJSON 1

instance J.Encode Config where
  build o = J.object
    [  "gasLimit" J..= fmap J.Aeson (_gasLimit o)
    ,  "entity" J..= _entity o
    ,  "persistDir" J..= fmap J.string (_persistDir o)
    ,  "port" J..= J.Aeson (_port o)
    ,  "logDir" J..= J.string (_logDir o)
    ,  "pragmas" J..= J.Array (_pragmas o)
    ,  "gasRate" J..= fmap J.Aeson (_gasRate o)
    ,  "execConfig" J..= _execConfig o
    ,  "verbose" J..= _verbose o
    ]

usage :: String
usage = unlines
  [ "Config file is YAML format with the following properties:"
  , "port       - HTTP server port"
  , "persistDir - Directory for database files."
  , "             If ommitted, runs in-memory only."
  , "logDir     - Directory for HTTP logs"
  , "pragmas    - SQLite pragmas to use with persistence DBs"
  , "entity     - Entity name for simulating privacy, defaults to \"entity\""
  , "gasLimit   - Gas limit for each transaction, defaults to 0"
  , "gasRate    - Gas price per action, defaults to 0"
  , "flags      - Pact runtime execution flags"
  , "\n"
  ]

serve :: FilePath -> SPVSupport -> IO ()
serve = serve_ False

-- | Runs server that binds only to localhost.
--
-- This is more secure when only local access is needed. It also prevents the
-- firewall configuration window from popping up on MacOSX.
--
serveLocal :: FilePath -> SPVSupport -> IO ()
serveLocal = serve_ True

serve_ :: Bool -> FilePath -> SPVSupport -> IO ()
serve_ isLocal configFile spv = do
  Config {..} <- validateConfigFile configFile
  (inC,histC) <- initChans
  replayFromDisk' <- ReplayFromDisk <$> newEmptyMVar
  debugFn <- if _verbose then initFastLogger else return (return . const ())
  let cmdConfig = CommandConfig
          (fmap (\pd -> SQLiteConfig (pd ++ "/pact.sqlite") _pragmas) _persistDir)
          _entity
          _gasLimit
          _gasRate
          (fromMaybe def _execConfig)

  let histConf = initHistoryEnv histC inC _persistDir debugFn replayFromDisk'

  -- Must be individually killed with uninterruptibleCancel if parent thread not killed.
  asyncCmd <- async (startCmdThread cmdConfig inC histC replayFromDisk' debugFn spv)
  -- Must be individually killed with uninterruptibleCancel if parent thread not killed.
  asyncHist <- async (runHistoryService histConf Nothing)
  let runServer = if isLocal then runApiServerLocal else runApiServer

  link asyncCmd
  link asyncHist
  runServer histC inC debugFn (fromIntegral _port) _logDir

validateConfigFile :: FilePath -> IO Config
validateConfigFile fp = Y.decodeFileEither fp >>= \case
  Left pe -> do
    putStrLn usage
    throwIO $ userError $ "Error loading config file: " ++ show pe
  Right v -> do
    traverse_ (createDirectoryIfMissing True) $ _persistDir v
    createDirectoryIfMissing True $ _logDir v
    pure v


withTestServe :: FilePath -> SPVSupport -> (Port -> IO a) -> IO a
withTestServe configFile spv app = do
  Config {..} <- validateConfigFile configFile
  (inC,histC) <- initChans
  replayFromDisk' <- ReplayFromDisk <$> newEmptyMVar
  debugFn <- if _verbose then initFastLogger else return (return . const ())
  let cmdConfig = CommandConfig
          (fmap (\pd -> SQLiteConfig (pd ++ "/pact.sqlite") _pragmas) _persistDir)
          _entity
          _gasLimit
          _gasRate
          (fromMaybe def _execConfig)
  let histConf = initHistoryEnv histC inC _persistDir debugFn replayFromDisk'
      cmd = startCmdThread cmdConfig inC histC replayFromDisk' debugFn spv
      hist = runHistoryService histConf Nothing
  withAsync cmd $ \_->
    withAsync hist $ \_->
      withTestApiServer histC inC debugFn app

initFastLogger :: IO (String -> IO ())
initFastLogger = do
  tc <- newTimeCache "%Y/%m/%d-%H:%M:%S"
  (tfl,_) <- newTimedFastLogger tc (LogStdout 1000)
  return $ \m -> tfl $ \t -> toLogStr t <> " " <> toLogStr (B8.pack m) <> "\n"

startCmdThread
  :: CommandConfig
  -> InboundPactChan
  -> HistoryChannel
  -> ReplayFromDisk
  -> (String -> IO ())
  -> SPVSupport
  -> IO ()
startCmdThread cmdConfig inChan histChan (ReplayFromDisk rp) debugFn spv = do
  CommandExecInterface {..} <- initPactService cmdConfig (initLoggers debugFn doLog def) spv
  -- we wait for the history service to light up, possibly giving us backups from disk to replay
  replayFromDisk' <- liftIO $ takeMVar rp
  when (null replayFromDisk') $ liftIO $ debugFn "[disk replay]: No replay found"
  unless (null replayFromDisk') $
    forM_ replayFromDisk' $ \cmd -> do
      liftIO $ debugFn $ "[disk replay]: replaying => " ++ show cmd
      liftIO $ _ceiApplyCmd Transactional cmd
      -- NB: we don't want to update history with the results from the replay
  forever $ do
    -- now we're prepared, so start taking new entries
    inb <- liftIO $ readInbound inChan
    case inb of
      TxCmds cmds -> do
        liftIO $ debugFn $ "[cmd]: executing " ++ show (length cmds) ++ " command(s)"
        resps <- forM cmds $ \cmd -> do
          liftIO $ _ceiApplyCmd Transactional cmd
        liftIO $ writeHistory histChan $ Update $ HashMap.fromList $ (\cmdr@CommandResult{..} -> (_crReqKey, cmdr)) <$> resps
      LocalCmd cmd mv -> do
        resp <- liftIO $ _ceiApplyCmd Local cmd
        liftIO $ putMVar mv resp
