{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Control.Concurrent
import Control.Lens hiding ((<|))
import Control.Monad.Reader
import Control.Monad.Trans.Except

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NEL
import Data.Semigroup.Foldable (fold1)
import qualified Data.Text as T
import Data.Text.Encoding

import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant

import Pact.Analyze.Remote.Server (verifyHandler)
import Pact.Server.API
import Pact.Types.API
import Pact.Types.Command
import Pact.Types.Hash
import Pact.Types.Server
import Pact.Types.Version


#if !MIN_VERSION_servant(0,16,0)
type ServerError = ServantErr
#endif

data ApiEnv = ApiEnv
  { _aiLog :: String -> IO ()
  , _aiHistoryChan :: HistoryChannel
  , _aiInboundPactChan :: InboundPactChan
  }
makeLenses ''ApiEnv

type Api a = ReaderT ApiEnv (ExceptT ServerError IO) a

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
    nt :: forall a. Api a -> Handler a
    nt s = Handler $ runReaderT s conf

sendHandler :: SubmitBatch -> Api RequestKeys
sendHandler (SubmitBatch cmds) = do
  crs <- traverse buildCmdRpc cmds
  rks <- traverse queueCmds $ group 8000 crs
  pure . RequestKeys $ fold1 rks

pollHandler :: Poll -> Api PollResponses
pollHandler (Poll rks) = do
  log $ "Polling for " ++ show rks
  PossiblyIncompleteResults{..} <- checkHistoryForResult (HashSet.fromList $ NEL.toList rks)
  when (HM.null possiblyIncompleteResults) $ log $ "No results found for poll!" ++ show rks
  pure $ pollResultToReponse possiblyIncompleteResults

listenHandler :: ListenerRequest -> Api ListenResponse
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
      pure (ListenResponse cr)

localHandler :: Command T.Text -> Api (CommandResult Hash)
localHandler commandText = do
  let (cmd :: Command ByteString) = fmap encodeUtf8 commandText
  mv <- liftIO newEmptyMVar
  c <- view aiInboundPactChan
  liftIO $ writeInbound c (LocalCmd cmd mv)
  r <- liftIO $ takeMVar mv
  pure r

versionHandler :: Handler T.Text
versionHandler = pure pactVersion

checkHistoryForResult :: (MonadReader ApiEnv m, MonadIO m) => HashSet RequestKey -> m PossiblyIncompleteResults
checkHistoryForResult rks = do
  hChan <- view aiHistoryChan
  m <- liftIO newEmptyMVar
  liftIO $ writeHistory hChan $ QueryForResults (rks,m)
  liftIO $ readMVar m

pollResultToReponse :: HM.HashMap RequestKey (CommandResult Hash) -> PollResponses
pollResultToReponse m = PollResponses m

log :: (MonadReader ApiEnv m, MonadIO m) => String -> m ()
log s = view aiLog >>= \f -> liftIO (f $ "[api]: " ++ s)

die' :: String -> Api t
die' str = throwError err404 { errBody = BSL8.pack str }

buildCmdRpc :: (MonadReader ApiEnv m, MonadIO m) => Command T.Text -> m (RequestKey,Command ByteString)
buildCmdRpc c@Command {..} = do
  log $ "Processing command with hash: " ++ show _cmdHash
  return (cmdToRequestKey c,fmap encodeUtf8 c)

-- | Reorganize a `NonEmpty` into sub-batches.
group :: Int -> NonEmpty a -> NonEmpty (NonEmpty a)
group n l
  | n < 1 = error "Non-positive n"
  | otherwise = case rest of
      Nothing -> items :| []
      Just rs -> items <| group n rs
  where
    (items, rest) = bimap NEL.fromList NEL.nonEmpty $ NEL.splitAt n l

queueCmds :: (MonadReader ApiEnv m, MonadIO m) =>
  NonEmpty (RequestKey, Command ByteString) -> m (NonEmpty RequestKey)
queueCmds rpcs = do
  hc <- view aiHistoryChan
  liftIO . writeHistory hc . AddNew $ NEL.toList cmds
  return rks
  where
    (rks, cmds) = NEL.unzip rpcs
