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
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy (toStrict)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Aeson hiding (defaultOptions, Result(..))
import Snap.Core
import Snap.CORS
import Snap.Util.FileServe
import Snap.Http.Server as Snap
import Data.Text.Encoding
import Data.ByteString (ByteString)

import Pact.Types.Command
import Pact.Types.API
import Pact.Types.Server

data ApiEnv = ApiEnv
  { _aiLog :: String -> IO ()
  , _aiHistoryChan :: HistoryChannel
  }
makeLenses ''ApiEnv

type Api a = ReaderT ApiEnv Snap a

runApiServer :: HistoryChannel -> (String -> IO ()) -> Int -> IO ()
runApiServer histChan logFn port = do
  putStrLn $ "runApiServer: starting on port " ++ show port
  let conf' = ApiEnv logFn histChan
  httpServe (serverConf port) $
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
      ]

sendPublicBatch :: Api ()
sendPublicBatch = do
  (SubmitBatch cmds) <- readJSON
  when (null cmds) $ die "Empty Batch"
  rks <- mapM queueCmds $ group 8000 cmds
  writeResponse $ ApiSuccess $ RequestKeys $ concat rks

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
pollResultToReponse m = ApiSuccess $ PollResponses $ HM.fromList $ map (second _prResult) $ HM.toList m


log :: String -> Api ()
log s = view aiLog >>= \f -> liftIO (f $ "[pact server]: " ++ s)

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

serverConf :: MonadSnap m => Int -> Snap.Config m a
serverConf port =
  setErrorLog (ConfigFileLog "log/error.log") $
  setAccessLog (ConfigFileLog "log/access.log") $
  setPort port defaultConfig


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
    ListenerResult CommandResult{..} -> do
      log $ "Listener Serviced for: " ++ show rk
      writeResponse $ ApiSuccess _prResult
