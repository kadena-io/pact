{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


-- |
-- Module      :  Pact.Server.Server
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Launch pact dev server.
--

module Pact.Server.Server
  ( serve
  , ServerPort(..)
  , ServerDebugLogging(..)
  , ServerEnablePersistence(..)
  ) where

import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Data.Word (Word16)
import qualified Data.HashMap.Strict as HashMap

import Pact.Server.PactService
import Pact.Server.ApiServer
import Pact.Server.History.Service
import Pact.Types.Runtime hiding (Update)
import Pact.Types.Server
import Pact.Types.Command

newtype ServerPort = ServerPort Word16 deriving (Show, Eq, Read)
newtype ServerDebugLogging = ServerDebugLogging Bool deriving (Show, Eq, Read)
newtype ServerEnablePersistence = ServerEnablePersistence Bool deriving (Show, Eq, Read)

serve :: ServerPort -> ServerDebugLogging -> ServerEnablePersistence -> IO ()
serve (ServerPort port') (ServerDebugLogging enableDebug) (ServerEnablePersistence enablePersistence) = do
  (inC,histC) <- initChans
  replayFromDisk' <- ReplayFromDisk <$> newEmptyMVar
  let debugFn = if enableDebug then putStrLn else return . const ()
  let serverPort = fromIntegral port'
  let cmdConfig = CommandConfig Nothing debugFn "entity"
  let histConf = initHistoryEnv histC inC (if enablePersistence then Just "log/" else Nothing) debugFn replayFromDisk'
  _ <- forkIO $ startCmdThread cmdConfig inC histC replayFromDisk'
  _ <- forkIO $ runHistoryService histConf Nothing
  runApiServer histC debugFn serverPort

startCmdThread :: CommandConfig -> InboundPactChan -> HistoryChannel -> ReplayFromDisk -> IO ()
startCmdThread cmdConfig inChan histChan (ReplayFromDisk rp) = do
  CommandExecInterface {..} <- initPactService cmdConfig
  void $ (`runStateT` (0 :: TxId)) $ do
    -- we wait for the history service to light up, possibly giving us backups from disk to replay
    replayFromDisk' <- liftIO $ takeMVar rp
    unless (null replayFromDisk') $ do
      forM_ replayFromDisk' $ \cmd -> do
      txid <- state (\i -> (i,succ i))
      liftIO $ _ceiApplyCmd (Transactional txid) cmd
      -- NB: we don't want to update history with the results from the replay
    forever $ do
      -- now we're prepared, so start taking new entries
      cmds <- liftIO $ readInbound inChan
      resps <- forM cmds $ \cmd -> do
        txid <- state (\i -> (i,succ i))
        liftIO $ _ceiApplyCmd (Transactional txid) cmd
      liftIO $ writeHistory histChan $ Update $ HashMap.fromList $ (\cmdr@CommandResult{..} -> (_prReqKey, cmdr)) <$> resps
