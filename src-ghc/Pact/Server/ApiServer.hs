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
-- Servant server for Pact REST API.
--

module Pact.Server.ApiServer
  ( runApiServer
  , ApiEnv(..), aiLog, aiHistoryChan
  , sendHandler, pollHandler, listenHandler, localHandler, versionHandler
  ) where

import Prelude hiding (log)

import Control.Lens
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Arrow

import Data.Aeson hiding (defaultOptions, Result(..))
import Data.Aeson.Types (parseMaybe)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.Proxy
import Data.Text.Encoding

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HM

import Servant
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors

import Pact.Analyze.Remote.Server (verifyHandler)
import Pact.Server.API
import Pact.Types.Command
import Pact.Types.API
import Pact.Types.Server
import Pact.Types.Version

data ApiEnv = ApiEnv
  { _aiLog :: String -> IO ()
  , _aiHistoryChan :: HistoryChannel
  , _aiInboundPactChan :: InboundPactChan
  }
makeLenses ''ApiEnv

type Api a = ReaderT ApiEnv (ExceptT ServantErr IO) a

runApiServer :: HistoryChannel -> InboundPactChan -> (String -> IO ()) -> Int -> FilePath -> IO ()
runApiServer histChan inbChan logFn port _logDir = do
  logFn $ "[api] starting on port " ++ show port
  let conf' = ApiEnv logFn histChan inbChan
  run port $ cors (const policy) $ serve pactServerAPI (servantServer conf')
  where
    policy = Just CorsResourcePolicy
      { corsOrigins = Nothing
      , corsMethods = ["GET", "POST"]
      , corsRequestHeaders = ["authorization", "content-type"]
      , corsExposedHeaders = Nothing
      , corsMaxAge = Just $ 60*60*24 -- one day
      , corsVaryOrigin = False
      , corsRequireOrigin = False
      , corsIgnoreFailures = False
      }

servantServer :: ApiEnv -> Server PactServerAPI
servantServer conf = apiV1Server conf :<|> verifyHandler :<|> versionHandler

apiV1Server :: ApiEnv -> Server ApiV1API
apiV1Server conf = hoistServer apiV1API nt
  (sendHandler :<|> pollHandler :<|> listenHandler :<|> localHandler)
  where
    apiV1API = Proxy :: Proxy ApiV1API
    nt :: forall a. Api a -> Handler a
    nt s = Handler $ runReaderT s conf

sendHandler :: SubmitBatch -> Api RequestKeys
sendHandler (SubmitBatch cmds) = do
  when (null cmds) $ die' "Empty Batch"
  crs <- forM cmds $ \c -> do
    cr@(_,Command{..}) <- buildCmdRpc c
    return cr
  rks <- mapM queueCmds $ group 8000 crs
  pure $ RequestKeys $ concat rks

pollHandler :: Poll -> Api PollResponses
pollHandler (Poll rks) = do
  log $ "Polling for " ++ show rks
  PossiblyIncompleteResults{..} <- checkHistoryForResult (HashSet.fromList rks)
  when (HM.null possiblyIncompleteResults) $ log $ "No results found for poll!" ++ show rks
  pure $ pollResultToReponse possiblyIncompleteResults

listenHandler :: ListenerRequest -> Api ApiResult
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
      pure $ crToAr cr

localHandler :: Command T.Text -> Api (CommandSuccess Value)
localHandler commandText = do
  let (cmd :: Command ByteString) = fmap encodeUtf8 commandText
  mv <- liftIO newEmptyMVar
  c <- view aiInboundPactChan
  liftIO $ writeInbound c (LocalCmd cmd mv)
  r <- liftIO $ takeMVar mv
  case parseMaybe parseJSON r of
    Just v@CommandSuccess{} -> pure v
    Nothing -> die' "command could not be run locally"

versionHandler :: Handler T.Text
versionHandler = pure pactVersion

checkHistoryForResult :: (MonadReader ApiEnv m, MonadIO m) => HashSet RequestKey -> m PossiblyIncompleteResults
checkHistoryForResult rks = do
  hChan <- view aiHistoryChan
  m <- liftIO newEmptyMVar
  liftIO $ writeHistory hChan $ QueryForResults (rks,m)
  liftIO $ readMVar m

pollResultToReponse :: HM.HashMap RequestKey CommandResult -> PollResponses
pollResultToReponse m = PollResponses $ HM.fromList $ map (second crToAr) $ HM.toList m

crToAr :: CommandResult -> ApiResult
crToAr CommandResult {..} = ApiResult (toJSON _crResult) _crTxId Nothing

log :: (MonadReader ApiEnv m, MonadIO m) => String -> m ()
log s = view aiLog >>= \f -> liftIO (f $ "[api]: " ++ s)

die' :: String -> Api t
die' str = throwError err404 { errBody = BSL8.pack str }

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
