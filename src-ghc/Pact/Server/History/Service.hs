{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Server.History.Service
  ( initHistoryEnv
  , runHistoryService
  ) where

import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import Control.Concurrent.MVar
import System.Directory

import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)

import Pact.Types.Command
import Pact.Types.Server
import Pact.Server.History.Types
import Pact.Server.History.Persistence as DB

initHistoryEnv
  :: HistoryChannel
  -> InboundPactChan
  -> Maybe FilePath
  -> (String -> IO ())
  -> ReplayFromDisk
  -> HistoryEnv
initHistoryEnv historyChannel' inboundPactChannel' dbPath' debugPrint' rp = HistoryEnv
  { _historyChannel = historyChannel'
  , _inboundPactChannel = inboundPactChannel'
  , _debugPrint = debugPrint'
  , _dbPath = dbPath'
  , _replayFromDisk = rp
  }

runHistoryService :: HistoryEnv -> Maybe HistoryState -> IO ()
runHistoryService env mState = do
  let dbg = env ^. debugPrint
  let oChan = env ^. historyChannel
  initHistoryState <- case mState of
    Nothing -> do
      pers <- setupPersistence dbg (env ^. dbPath) (env ^. replayFromDisk)
      return $! HistoryState { _registeredListeners = HashMap.empty, _persistence = pers }
    Just mState' -> do
      return mState'
  (_,bouncedState,_) <- runRWST (handle oChan) env initHistoryState
  runHistoryService env (Just bouncedState)

debug :: String -> HistoryService ()
debug s = do
  dbg <- view debugPrint
  liftIO $! dbg $ "[history] " ++ s

setupPersistence :: (String -> IO ()) -> Maybe FilePath -> ReplayFromDisk -> IO PersistenceSystem
setupPersistence dbg Nothing (ReplayFromDisk rp) = do
  dbg "[history] Persistence Disabled"
  putMVar rp [] -- if there's no replays, we still need to unblock the cmd thread
  return $ InMemory HashMap.empty
setupPersistence dbg (Just dbPath') (ReplayFromDisk rp) = do
  let dbfile = dbPath' ++ "/commands.sqlite"
  dbg $ "[history] Persistence Enabled: " ++ dbfile
  dbExists <- doesFileExist dbfile
  conn <- DB.createDB dbfile
  when dbExists $ do
    replayFromDisk' <- DB.selectAllCommands conn
    dbg "[history] Replaying from disk"
    putMVar rp replayFromDisk'
  unless dbExists $
    putMVar rp [] -- if there's no replays, we still need to unblock the cmd thread
  return OnDisk { incompleteRequestKeys = HashMap.empty
                , dbConn = conn }

handle :: HistoryChannel -> HistoryService ()
handle oChan = do
  q <- liftIO $ readHistory oChan
  case q of
    AddNew{..} -> addNewKeys hNewKeys
    Update{..} -> updateExistingKeys hUpdateRks
    QueryForResults{..} -> queryForResults hQueryForResults
    RegisterListener{..} -> registerNewListeners hNewListener

addNewKeys :: [Command ByteString] -> HistoryService ()
addNewKeys cmds = do
  pers <- use persistence
  let writeNewCmds newCmdsHM = do
        newCmdsList <- return $ filter (\cmd -> HashMap.member (RequestKey $ _cmdHash cmd) newCmdsHM) cmds
        pactChan <- view inboundPactChannel
        liftIO $ writeInbound pactChan (TxCmds newCmdsList)
  case pers of
    InMemory m -> do
      asHM <- return $ HashMap.fromList $! (\cmd -> (RequestKey $ _cmdHash cmd,(cmd, Nothing))) <$> cmds
      newCmdsHM <- return $ HashMap.difference asHM m
      writeNewCmds newCmdsHM
      persistence .= InMemory (HashMap.union m newCmdsHM)
    OnDisk{..} -> do
      asHM <- return $ HashMap.fromList $! (\cmd -> (RequestKey $ _cmdHash cmd,cmd)) <$> cmds
      notInMem <- return $ HashMap.difference asHM incompleteRequestKeys
      if HashMap.null notInMem
      -- unlikely but worth a O(1) check
      then do
        debug $ "Each of the " ++ show (HashMap.size asHM) ++ " new command(s) had a previously seen hash/requestKey"
      else do
        foundOnDisk <- liftIO $ DB.queryForExisting dbConn $ HashSet.fromMap $ void notInMem
        newCmdsHM <- return $ HashMap.filterWithKey (\k _ -> not $ HashSet.member k foundOnDisk) notInMem
        if HashMap.null newCmdsHM
        then do
          debug $ "Each of the " ++ show (HashMap.size asHM) ++ " new command(s) had a previously hash/requestKey"
        else do
          writeNewCmds newCmdsHM
          persistence .= OnDisk { incompleteRequestKeys = HashMap.union incompleteRequestKeys newCmdsHM
                                , dbConn = dbConn }
          debug $ "Added " ++ show (HashMap.size newCmdsHM) ++ " new command(s)"
          when (HashMap.size asHM /= HashMap.size newCmdsHM) $ do
            debug $ "Some (" ++ show (HashMap.size asHM - HashMap.size newCmdsHM) ++ ") new command(s) had a previously seen hash/requestKey"


updateExistingKeys :: HashMap RequestKey CommandResult -> HistoryService ()
updateExistingKeys updates = do
  alertListeners updates
  pers <- use persistence
  case pers of
    InMemory m -> do
      newInMem <- return $! InMemory $! foldr updateInMemKey m $ HashMap.toList updates
      persistence .= newInMem
    OnDisk{..} -> do
      pairedCmdResults <- return $ pairResultWithCmd incompleteRequestKeys <$> HashMap.toList updates
      liftIO $ DB.insertCompletedCommand dbConn pairedCmdResults
      persistence .= OnDisk { incompleteRequestKeys = HashMap.filterWithKey (\k _ -> not $ HashMap.member k updates) incompleteRequestKeys
                            , dbConn = dbConn }
  debug $ "Updated " ++ show (HashMap.size updates) ++ " command(s)"

updateInMemKey :: (RequestKey, CommandResult) -> HashMap RequestKey (Command ByteString, Maybe CommandResult) -> HashMap RequestKey (Command ByteString, Maybe CommandResult)
updateInMemKey (k,v) m = HashMap.adjust (\(cmd, _) -> (cmd, Just v)) k m

pairResultWithCmd :: HashMap RequestKey (Command ByteString) -> (RequestKey, CommandResult) -> (Command ByteString, CommandResult)
pairResultWithCmd m (rk, cmdr) = case HashMap.lookup rk m of
  Nothing -> error $ "Fatal error: the results for a RequestKey came in, but we can't find the original command\n" ++ show rk ++ "\n#----#\n" ++ show m
  Just cmd -> (cmd, cmdr)

alertListeners :: HashMap RequestKey CommandResult -> HistoryService ()
alertListeners m = do
  listeners <- use registeredListeners
  triggered <- return $! HashMap.filterWithKey (\k _ -> HashMap.member k m) listeners
  unless (HashMap.null triggered) $ do
    res <- mapM (alertListener m) $ HashMap.toList triggered
    registeredListeners %= HashMap.filterWithKey (\k _ -> not $ HashMap.member k triggered)
    -- use registeredListeners >>= debug . ("Active Listeners: " ++) . show . HashMap.keysSet
    debug $ "Serviced " ++ show (sum res) ++ " listener(s)"

alertListener :: HashMap RequestKey CommandResult -> (RequestKey, [MVar ListenerResult]) -> HistoryService Int
alertListener res (k,mvs) = do
  commandRes <- return $! res HashMap.! k
  -- debug $ "Servicing Listener for: " ++ show k
  fails <- filter not <$> liftIO (mapM (`tryPutMVar` ListenerResult commandRes) mvs)
  unless (null fails) $ debug $ "Registered listener failure for " ++ show k ++ " (" ++ show (length fails) ++ " of " ++ show (length mvs) ++ " failed)"
  return $ length mvs

queryForResults :: (HashSet RequestKey, MVar PossiblyIncompleteResults) -> HistoryService ()
queryForResults (srks, mRes) = do
  pers <- use persistence
  case pers of
    InMemory m -> do
      found <- return $! HashMap.filterWithKey (checkForIndividualResultInMem srks) m
      liftIO $! putMVar mRes $ PossiblyIncompleteResults $ fmap (fromJust.snd) found
      debug $ "Querying for " ++ show (HashSet.size srks) ++ " keys, found " ++ show (HashMap.size found)
    OnDisk{..} -> do
      completed <- return $! HashSet.filter (\k -> not $ HashMap.member k incompleteRequestKeys) srks
      if HashSet.null completed
      then do
        liftIO $! putMVar mRes $ PossiblyIncompleteResults HashMap.empty
      else do
        found <- liftIO $ DB.selectCompletedCommands dbConn completed
        liftIO $! putMVar mRes $ PossiblyIncompleteResults found
        debug $ "Querying for " ++ show (HashSet.size srks) ++ " keys, found " ++ show (HashMap.size found)

-- This is here to try to get GHC to check the fast part first
checkForIndividualResultInMem :: HashSet RequestKey -> RequestKey -> (Command ByteString, Maybe CommandResult) -> Bool
checkForIndividualResultInMem _ _ (_,Nothing) = False
checkForIndividualResultInMem s k (_,Just _) = HashSet.member k s

registerNewListeners :: HashMap RequestKey (MVar ListenerResult) -> HistoryService ()
registerNewListeners newListeners' = do
  srks <- return $! HashSet.fromMap $ void newListeners'
  pers <- use persistence
  found <- case pers of
    InMemory m -> do
      return $! fromJust.snd <$> HashMap.filterWithKey (checkForIndividualResultInMem srks) m
    OnDisk{..} -> do
      liftIO $! DB.selectCompletedCommands dbConn srks
  noNeedToListen <- return $! HashSet.intersection (HashSet.fromMap $ void found) srks
  readyToServiceListeners <- return $! HashMap.filterWithKey (\k _ -> HashSet.member k noNeedToListen) newListeners'
  realListeners <- return $! HashMap.filterWithKey (\k _ -> not $ HashSet.member k noNeedToListen) newListeners'
  unless (HashMap.null readyToServiceListeners) $ do
    mapM_ (\(k,v) -> alertListener found (k,[v])) $ HashMap.toList readyToServiceListeners
    debug $ "Immediately serviced " ++ show (HashSet.size noNeedToListen) ++ " listeners"
  unless (HashMap.null realListeners) $ do
    registeredListeners %= HashMap.unionWith (<>) ((:[]) <$> realListeners)
    debug $ "Registered " ++ show (HashMap.size realListeners) ++ " listeners"

_gcVolLogListeners :: HashSet RequestKey -> HistoryService ()
_gcVolLogListeners srks = do
  toGc <- HashMap.filterWithKey (\k _ -> HashSet.member k srks) <$> use registeredListeners
  mapM_ (_gcListener "Transaction was GCed! This generally occurs because disk I/O is failing") $ HashMap.toList toGc
  registeredListeners %= HashMap.filterWithKey (\k _ -> not $ HashSet.member k srks)

_gcListener :: String -> (RequestKey, [MVar ListenerResult]) -> HistoryService ()
_gcListener res (k,mvs) = do
  fails <- filter not <$> liftIO (mapM (`tryPutMVar` GCed res) mvs)
  unless (null fails) $ debug $ "Registered listener failure during GC for " ++ show k ++ " (" ++ show (length fails) ++ " of " ++ show (length mvs) ++ " failed)"
