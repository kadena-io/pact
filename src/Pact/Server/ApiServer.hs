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
  , ApiEnv(..), aiLog, aiHistoryChan
  ) where

import Prelude hiding (log)

import Control.Lens hiding ((.=))
import Control.Concurrent
import Control.Monad.Reader
import Control.Arrow

import Data.Aeson hiding (defaultOptions, Result(..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Text.Encoding

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HM

import Snap.CORS
import Snap.Core
import Snap.Http.Server as Snap
import Snap.Util.FileServe

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

runApiServer :: HistoryChannel -> InboundPactChan -> (String -> IO ()) -> Int -> FilePath -> IO ()
runApiServer histChan inbChan logFn port logDir = do
  logFn $ "[api] starting on port " ++ show port
  let conf' = ApiEnv logFn histChan inbChan
  httpServe (serverConf port logDir) $
    applyCORS defaultOptions $ methods [GET, POST] $
    route [("api", runReaderT api conf')
          ,("/", noCacheStatic)]

noCacheStatic :: Snap ()
noCacheStatic = do
  modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  modifyResponse $ setHeader "Expires" "0"
  serveDirectory "."

api :: Api ()
api = route [
       ("send",sendPublicBatch)
      ,("poll",poll)
      ,("listen",registerListener)
      ,("local",sendLocal)
      ]

sendPublicBatch :: Api ()
sendPublicBatch = do
  (SubmitBatch cmds) <- readJSON
  when (null cmds) $ die "Empty Batch"
  rks <- mapM queueCmds $ group 8000 cmds
  writeResponse $ ApiSuccess $ RequestKeys $ concat rks

sendLocal :: Api ()
sendLocal = do
  (cmd :: Command ByteString) <- fmap encodeUtf8 <$> readJSON
  mv <- liftIO newEmptyMVar
  c <- view aiInboundPactChan
  liftIO $ writeInbound c (LocalCmd cmd mv)
  r <- liftIO $ takeMVar mv
  writeResponse $ ApiSuccess $ r

checkHistoryForResult :: HashSet RequestKey -> Api PossiblyIncompleteResults
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
crToAr CommandResult {..} = ApiResult (toJSON _crResult) _crTxId

log :: String -> Api ()
log s = view aiLog >>= \f -> liftIO (f $ "[api]: " ++ s)

die :: String -> Api t
die res = do
  _ <- getResponse -- chuck what we've done so far
  log res
  writeResponse (ApiFailure res :: ApiResponse ())
  finishWith =<< getResponse

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

buildCmdRpc :: Command T.Text -> Api (RequestKey,Command ByteString)
buildCmdRpc c@PublicCommand {..} = do
  log $ "Processing command with hash: " ++ show _cmdHash
  return (RequestKey _cmdHash,fmap encodeUtf8 c)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = take n l : group n (drop n l)
  | otherwise = error "Negative n"

queueCmds :: [Command T.Text] -> Api [RequestKey]
queueCmds cmds = do
  rpcs <- mapM buildCmdRpc cmds
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
