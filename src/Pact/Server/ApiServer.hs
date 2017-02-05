{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Pact.Server.ApiServer
--  ( runApiServer
--  ) where
  where

import Prelude hiding (log)
import Control.Lens hiding ((.=))
import Control.Concurrent
import Control.Monad.Reader

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy (toStrict)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map

import Data.Aeson hiding (defaultOptions, Result(..))
import qualified Data.Serialize as SZ

import Data.Thyme.Clock
import Data.Thyme.Time.Core (unUTCTime, toMicroseconds)

import Snap.Core
import Snap.Http.Server as Snap

import Pact.Server.Types

data ApiEnv = ApiEnv
  { _aiLog :: String -> IO ()
--  , _aiIdCounter :: MVar Int
--  , _aiDispatch :: Dispatch
--  , _aiConfig :: Config.Config
--  , _aiPubConsensus :: MVar PublishedConsensus
  }
makeLenses ''ApiEnv

type Api a = ReaderT ApiEnv Snap a

initRequestId :: IO Int
initRequestId = do
  UTCTime _ time <- unUTCTime <$> getCurrentTime
  return $ fromIntegral $ toMicroseconds time

runApiServer = undefined
--runApiServer :: Dispatch -> Config.Config -> (String -> IO ())
--             -> Int -> MVar PublishedConsensus -> IO ()
--runApiServer dispatch conf logFn port mPubConsensus' = do
--  putStrLn $ "runApiServer: starting on port " ++ show port
--  m <- newMVar =<< initRequestId
--  httpServe (serverConf port) $
--    applyCORS defaultOptions $ methods [GET, POST] $
--    route $ ("api", runReaderT api (ApiEnv logFn m dispatch conf mPubConsensus'))
--            :staticRoutes


api :: Api ()
api = route [
       ("public/send",sendPublicBatch)
      ,("poll",poll)
      ,("listen",registerListener)
      ]

sendPublicBatch = undefined
poll = undefined
registerListener = undefined

log :: String -> Api ()
log s = view aiLog >>= \f -> liftIO (f $ "[Service|Api]: " ++ s)

die :: String -> Api t
die res = do
  _ <- getResponse -- chuck what we've done so far
  setJSON
  log res
  writeLBS $ encode $ (ApiFailure res :: ApiResponse ())
  finishWith =<< getResponse

readJSON :: FromJSON t => Api (BS.ByteString,t)
readJSON = do
  b <- readRequestBody 1000000000
  tryParseJSON b

tryParseJSON
  :: FromJSON t =>
     BSL.ByteString -> Api (BS.ByteString, t)
tryParseJSON b = case eitherDecode b of
    Right v -> return (toStrict b,v)
    Left e -> die e

setJSON :: Api ()
setJSON = modifyResponse $ setHeader "Content-Type" "application/json"

writeResponse :: ToJSON j => j -> Api ()
writeResponse j = setJSON >> writeLBS (encode j)

{-
aliases need to be verified by api against public key
then, alias can be paired with requestId in message to create unique rid
polling can then be on alias and client rid
-}

--buildCmdRpc :: Command -> Api (RequestKey,SignedRPC)
--buildCmdRpc Command {..} = do
--  (_,PactEnvelope {..} :: PactEnvelope PactRPC) <- tryParseJSON (BSL.fromStrict $ _pmEnvelope)
--  storedCK <- Map.lookup _peAlias <$> view (aiConfig.clientPublicKeys)
--  unless (storedCK == Just _pmKey) $ die "Invalid alias/public key"
--  rid <- getNextRequestId
--  let ce = CommandEntry $! SZ.encode $ PublicMessage _pmEnvelope
--  return (RequestKey _pmHsh,mkCmdRpc ce _peAlias rid (Digest _peAlias _pmSig _pmKey CMD _pmHsh))
--
--group :: Int -> [a] -> [[a]]
--group _ [] = []
--group n l
--  | n > 0 = take n l : (group n (drop n l))
--  | otherwise = error "Negative n"
--
--sendPublicBatch :: Api ()
--sendPublicBatch = do
--  (_,SubmitBatch cmds) <- readJSON
--  when (null cmds) $ die "Empty Batch"
--  rks <- mapM sendInSensisbleChunks $ group 8000 cmds
--  writeResponse $ ApiSuccess $ RequestKeys $ concat rks
--
--sendInSensisbleChunks :: [Command] -> Api [RequestKey]
--sendInSensisbleChunks cmds = do
--  rpcs <- mapM buildCmdRpc cmds
--  let Command {..} = head cmds
--      btch = map snd rpcs
--      hsh = hash $ SZ.encode $ btch
--      dig = Digest "batch" (Sig "") _pmKey CMDB hsh
--      rpc = mkCmdBatchRPC (map snd rpcs) dig
--  enqueueRPC $! rpc -- CMDB' $! CommandBatch (reverse cmds) NewMsg
--  return $ fst <$> rpcs
--
--
--poll :: Api ()
--poll = do
--  (_,Poll rks) <- readJSON
--  log $ "Polling for " ++ show rks
--  PossiblyIncompleteResults{..} <- checkHistoryForResult (HashSet.fromList rks)
--  when (HashMap.null possiblyIncompleteResults) $ log $ "No results found for poll!" ++ show rks
--  setJSON
--  writeLBS $ encode $ pollResultToReponse possiblyIncompleteResults
--
--pollResultToReponse :: HashMap RequestKey AppliedCommand -> PollResponse
--pollResultToReponse m = ApiSuccess (kvToRes <$> HashMap.toList m)
--  where
--    kvToRes (rk,AppliedCommand{..}) = PollResult { _prRequestKey = rk, _prLatency = _acLatency, _prResponse = _acResult}
--
--serverConf :: MonadSnap m => Int -> Snap.Config m a
--serverConf port =
--  setErrorLog (ConfigFileLog "log/error.log") $
--  setAccessLog (ConfigFileLog "log/access.log") $
--  setPort port defaultConfig
--
--getNextRequestId :: Api RequestId
--getNextRequestId = do
--  cntr <- view aiIdCounter
--  cnt <- liftIO (takeMVar cntr)
--  liftIO $ putMVar cntr (cnt + 1)
--  return $ RequestId $ show cnt
--
--enqueueRPC :: SignedRPC -> Api ()
--enqueueRPC signedRPC = do
--  PublishedConsensus{..} <- view aiPubConsensus >>= liftIO . tryReadMVar >>= fromMaybeM (die "Invariant error: consensus unavailable")
--  conf <- view aiConfig
--  ldr <- fromMaybeM (die "System unavaiable, please try again later") _pcLeader
--  if _nodeId conf == ldr
--  then do -- dispatch internally if we're leader, otherwise send outbound
--    ts <- liftIO getCurrentTime
--    oChan <- view (aiDispatch.inboundCMD)
--    liftIO $ writeComm oChan $! InboundCMD (ReceivedAt ts, signedRPC)
--  else do
--    oChan <- view (aiDispatch.outboundGeneral)
--    liftIO $ writeComm oChan $! directMsg [(ldr,SZ.encode signedRPC)]
--
--checkHistoryForResult :: HashSet RequestKey -> Api PossiblyIncompleteResults
--checkHistoryForResult rks = do
--  hChan <- view (aiDispatch.historyChannel)
--  m <- liftIO $ newEmptyMVar
--  liftIO $ writeComm hChan $ QueryForResults (rks,m)
--  liftIO $ readMVar m
--
--registerListener :: Api ()
--registerListener = do
--  (_,ListenerRequest rk) <- readJSON
--  hChan <- view (aiDispatch.historyChannel)
--  m <- liftIO $ newEmptyMVar
--  liftIO $ writeComm hChan $ RegisterListener (HashMap.fromList [(rk,m)])
--  log $ "Registered Listener for: " ++ show rk
--  res <- liftIO $ readMVar m
--  case res of
--    History.GCed msg -> do
--      log $ "Listener GCed for: " ++ show rk ++ " because " ++ msg
--      die msg
--    History.ListenerResult AppliedCommand{..} -> do
--      log $ "Listener Serviced for: " ++ show rk
--      setJSON
--      ls <- return $ ApiSuccess $ PollResult { _prRequestKey = rk, _prLatency = _acLatency, _prResponse = _acResult}
--      writeLBS $ encode ls
