{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Pact.Server.ApiServer
-- Copyright   :  (C) 2016 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Snap server for Pact REST API.
--

module Pact.Server.ApiServer
  ( runApiServer
  , runApiServer'
  , ApiEnv(..), aiLog, aiHistoryChan
  ) where

import Prelude hiding (log)

import Control.Lens
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Arrow

import Data.Aeson hiding (defaultOptions, Result(..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.Proxy
import Data.Text.Encoding

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HM

import Servant
import Snap.Util.CORS
import Snap.Core
import Snap.Http.Server as Snap
import Network.Wai.Handler.Warp (run)

import Pact.Analyze.Remote.Server (verify)
import Pact.Server.API
import Pact.Types.Command
import Pact.Types.API
import Pact.Types.Server

data ApiEnv = ApiEnv
  { _aiLog :: String -> IO ()
  , _aiHistoryChan :: HistoryChannel
  , _aiInboundPactChan :: InboundPactChan
  }
makeLenses ''ApiEnv

type Api a = ReaderT ApiEnv Snap a

type ApiT a = ReaderT ApiEnv (ExceptT ServantErr IO) a

runApiServer' :: HistoryChannel -> InboundPactChan -> (String -> IO ()) -> Int -> FilePath -> IO ()
runApiServer' histChan inbChan logFn port logDir = do
  logFn $ "[api] starting on port " ++ show port
  let conf' = ApiEnv logFn histChan inbChan
  httpServe (serverConf port logDir) $
    applyCORS defaultOptions $ methods [Snap.Core.GET, Snap.Core.POST] $
    Snap.Core.route [("api/v1", runReaderT api conf')
          ,("verify", method Snap.Core.POST verify)]

runApiServer :: HistoryChannel -> InboundPactChan -> (String -> IO ()) -> Int -> FilePath -> IO ()
runApiServer histChan inbChan logFn port _logDir = do
  logFn $ "[api] starting on port " ++ show port
  let conf' = ApiEnv logFn histChan inbChan
  run port $ serve pactServerAPI (servantServer conf')

servantServer :: ApiEnv -> Server PactServerAPI
servantServer conf = apiV1Server conf :<|> verifyHandler

apiV1Server :: ApiEnv -> Server ApiV1API
apiV1Server conf = hoistServer apiV1API nt (sendHandler :<|> pollHandler :<|> listenHandler :<|> localHandler)
  where
    apiV1API = Proxy :: Proxy ApiV1API
    nt :: forall a. ApiT a -> Handler a
    nt s = Handler $ runReaderT s conf

sendHandler :: SubmitBatch -> ApiT (ApiResponse RequestKeys)
sendHandler (SubmitBatch cmds) = do
  when (null cmds) $ die' "Empty Batch"
  crs <- forM cmds $ \c -> do
    cr@(_,Command{..}) <- buildCmdRpc c
    return cr
  rks <- mapM queueCmds $ group 8000 crs
  pure $ ApiSuccess $ RequestKeys $ concat rks

pollHandler :: Poll -> ApiT (ApiResponse PollResponses)
pollHandler (Poll rks) = do
  log $ "Polling for " ++ show rks
  PossiblyIncompleteResults{..} <- checkHistoryForResult (HashSet.fromList rks)
  when (HM.null possiblyIncompleteResults) $ log $ "No results found for poll!" ++ show rks
  pure $ pollResultToReponse possiblyIncompleteResults

listenHandler :: ListenerRequest -> ApiT (ApiResponse ApiResult)
listenHandler (ListenerRequest rk) = do
  hChan <- view aiHistoryChan
  m <- liftIO newEmptyMVar
  liftIO $ writeHistory hChan $ RegisterListener (HM.fromList [(rk,m)])
  log $ "Registered Listener for: " ++ show rk
  res <- liftIO $ readMVar m
  case res of
    GCed msg -> do
      log $ "Listener GCed for: " ++ show rk ++ " because " ++ msg
      die' msg
    ListenerResult cr -> do
      log $ "Listener Serviced for: " ++ show rk
      pure $ ApiSuccess (crToAr cr)

localHandler :: Command T.Text -> ApiT (ApiResponse (CommandSuccess Value))
localHandler commandText = do
  let (cmd :: Command ByteString) = fmap encodeUtf8 commandText
  mv <- liftIO newEmptyMVar
  c <- view aiInboundPactChan
  liftIO $ writeInbound c (LocalCmd cmd mv)
  r <- liftIO $ takeMVar mv
  pure $ ApiSuccess (CommandSuccess r)

verifyHandler :: Value -> Handler Value
verifyHandler _value = undefined

api :: Api ()
api = Snap.Core.route [
       ("send",sendBatch)
      ,("poll",poll)
      ,("listen",registerListener)
      ,("local",sendLocal)
      ]

sendBatch :: Api ()
sendBatch = do
  (SubmitBatch cmds) <- readJSON
  when (null cmds) $ die "Empty Batch"
  crs <- forM cmds $ \c -> do
    cr@(_,Command{..}) <- buildCmdRpc c
    return cr
  rks <- mapM queueCmds $ group 8000 crs
  writeResponse $ ApiSuccess $ RequestKeys $ concat rks

sendLocal :: Api ()
sendLocal = do
  (cmd :: Command ByteString) <- fmap encodeUtf8 <$> readJSON
  mv <- liftIO newEmptyMVar
  c <- view aiInboundPactChan
  liftIO $ writeInbound c (LocalCmd cmd mv)
  r <- liftIO $ takeMVar mv
  writeResponse $ ApiSuccess r

checkHistoryForResult :: (MonadReader ApiEnv m, MonadIO m) => HashSet RequestKey -> m PossiblyIncompleteResults
checkHistoryForResult rks = do
  hChan <- view aiHistoryChan
  m <- liftIO newEmptyMVar
  liftIO $ writeHistory hChan $ QueryForResults (rks,m)
  liftIO $ readMVar m

poll :: Api ()
poll = do
  (Poll rks) <- readJSON
  log $ "Polling for " ++ show rks
  PossiblyIncompleteResults{..} <- checkHistoryForResult (HashSet.fromList rks)
  when (HM.null possiblyIncompleteResults) $ log $ "No results found for poll!" ++ show rks
  writeResponse $ pollResultToReponse possiblyIncompleteResults

pollResultToReponse :: HM.HashMap RequestKey CommandResult -> ApiResponse PollResponses
pollResultToReponse m = ApiSuccess $ PollResponses $ HM.fromList $ map (second crToAr) $ HM.toList m

crToAr :: CommandResult -> ApiResult
crToAr CommandResult {..} = ApiResult (toJSON _crResult) _crTxId Nothing

log :: (MonadReader ApiEnv m, MonadIO m) => String -> m ()
log s = view aiLog >>= \f -> liftIO (f $ "[api]: " ++ s)

die' :: String -> ApiT t
die' str = throwError err404 { errBody = BSL8.pack str }

die :: String -> Api t
die res = do
  _ <- Snap.Core.getResponse -- chuck what we've done so far
  log res
  writeResponse (ApiFailure res :: ApiResponse ())
  finishWith =<< Snap.Core.getResponse

readJSON :: FromJSON t => Api t
readJSON = do
  b <- readRequestBody 1000000000
  snd <$> tryParseJSON b

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

buildCmdRpc :: (MonadReader ApiEnv m, MonadIO m) => Command T.Text -> m (RequestKey,Command ByteString)
buildCmdRpc c@Command {..} = do
  log $ "Processing command with hash: " ++ show _cmdHash
  return (RequestKey _cmdHash,fmap encodeUtf8 c)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative n"

queueCmds :: (MonadReader ApiEnv m, MonadIO m) => [(RequestKey,Command ByteString)] -> m [RequestKey]
queueCmds rpcs = do
  hc <- view aiHistoryChan
  liftIO $ writeHistory hc $ AddNew (map snd rpcs)
  return $ fst <$> rpcs

serverConf :: MonadSnap m => Int -> FilePath -> Snap.Config m a
serverConf port logDir =
  setErrorLog (ConfigFileLog $ logDir ++ "/error.log") $
  setAccessLog (ConfigFileLog $ logDir ++ "/access.log") $
  setPort port $
  setVerbose False defaultConfig

registerListener :: Api ()
registerListener = do
  (ListenerRequest rk) <- readJSON
  hChan <- view aiHistoryChan
  m <- liftIO newEmptyMVar
  liftIO $ writeHistory hChan $ RegisterListener (HM.fromList [(rk,m)])
  log $ "Registered Listener for: " ++ show rk
  res <- liftIO $ readMVar m
  case res of
    GCed msg -> do
      log $ "Listener GCed for: " ++ show rk ++ " because " ++ msg
      die msg
    ListenerResult cr -> do
      log $ "Listener Serviced for: " ++ show rk
      writeResponse $ ApiSuccess (crToAr cr)
