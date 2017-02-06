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
import qualified Data.HashSet as S
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Aeson hiding (defaultOptions, Result(..))
import qualified Data.Serialize as SZ
import Data.Thyme.Clock
import Data.Thyme.Time.Core (unUTCTime, toMicroseconds)
import Snap.Core
import Snap.Http.Server as Snap
import Data.Text.Encoding

import Pact.Server.Types hiding (log)

data ApiEnv = ApiEnv
  { _aiLog :: String -> IO ()
  , _aiIdCounter :: MVar Int
  , _aiInbound :: InboundPactChan
  , _aiOutbound :: OutboundPactChan
--  , _aiDispatch :: Dispatch
--  , _aiConfig :: Config.Config
  , _aiResults :: MVar (HM.HashMap RequestKey Value)
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

sendPublicBatch :: Api ()
sendPublicBatch = do
  (_,SubmitBatch cmds) <- readJSON
  when (null cmds) $ die "Empty Batch"
  rks <- mapM queueCmds $ group 8000 cmds
  writeResponse $ ApiSuccess $ RequestKeys $ concat rks

poll :: Api ()
poll = do
  (_,Poll rks) <- readJSON
  log $ "Polling for " ++ show rks
  rs <- HM.filterWithKey (\k _ -> k `elem` rks) <$> serviceOutbound
  when (HM.null rs) $ log $ "No results found for poll!" ++ show rks
  setJSON
  writeLBS $ encode $ pollResultToReponse rs

pollResultToReponse :: HM.HashMap RequestKey Value -> PollResponse
pollResultToReponse m = ApiSuccess (kvToRes <$> HM.toList m)
  where
    kvToRes (rk,v) = PollResult { _prRequestKey = rk, _prLatency = 0, _prResponse = v}

--poll :: Api ()
--poll = do
--  (_,Poll rks) <- readJSON
--  log $ "Polling for " ++ show rks
--  PossiblyIncompleteResults{..} <- checkHistoryForResult (HashSet.fromList rks)
--  when (HashMap.null possiblyIncompleteResults) $ log $ "No results found for poll!" ++ show rks
--  setJSON
--  writeLBS $ encode $ pollResultToReponse possiblyIncompleteResults

registerListener = undefined

log :: String -> Api ()
log s = view aiLog >>= \f -> liftIO (f $ "[Service|Api]: " ++ s)

die :: String -> Api t
die res = do
  _ <- getResponse -- chuck what we've done so far
  setJSON
  log res
  writeLBS $ encode (ApiFailure res :: ApiResponse ())
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


buildCmdRpc :: Command T.Text -> Api (RequestKey,Command Payload)
buildCmdRpc c@PublicCommand {..} = do
  (_,p) <- tryParseJSON (BSL.fromStrict $ encodeUtf8 _cmdPayload)
  return (RequestKey _cmdHash,p <$ c)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative n"


queueCmds :: [Command T.Text] -> Api [RequestKey]
queueCmds cmds = do
  rpcs <- mapM buildCmdRpc cmds
  ic <- view aiInbound
  liftIO $ writeInbound ic (map snd rpcs)
  return $ fst <$> rpcs

-- | Drain outbound queue, and return map of results
serviceOutbound :: Api (HM.HashMap RequestKey Value)
serviceOutbound = do
  outm <- view aiOutbound >>= liftIO . tryReadOutbound
  view aiResults >>= \v -> liftIO $ modifyMVar v $ \rm -> do
    let toResult (PactResult PublicCommand {..} r) = (RequestKey _cmdHash,r)
        rm' = maybe rm (HM.union rm . HM.fromList . map toResult) outm
    return (rm',rm')


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
getNextRequestId :: Api RequestId
getNextRequestId = do
  cntr <- view aiIdCounter
  cnt <- liftIO (takeMVar cntr)
  liftIO $ putMVar cntr (cnt + 1)
  return $ RequestId $ T.pack $ show cnt
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
