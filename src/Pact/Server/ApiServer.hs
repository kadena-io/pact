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
  , ApiEnv(..), aiLog, aiInbound, aiOutbound, aiResults
  ) where

import Prelude hiding (log)
import Control.Lens hiding ((.=))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy (toStrict)
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
  , _aiInbound :: InboundPactChan
  , _aiOutbound :: OutboundPactChan
  , _aiResults :: TVar (HM.HashMap RequestKey Value)
  }
makeLenses ''ApiEnv

type Api a = ReaderT ApiEnv Snap a

runApiServer :: InboundPactChan -> OutboundPactChan -> (String -> IO ()) -> Int -> IO ()
runApiServer inChan outChan logFn port = do
  putStrLn $ "runApiServer: starting on port " ++ show port
  rm <- newTVarIO HM.empty
  let conf' = (ApiEnv logFn inChan outChan rm)
  _ <- forkIO $ consumeResults conf'
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
       ("public/send",sendPublicBatch)
      ,("poll",poll)
      ,("listen",registerListener)
      ]

sendPublicBatch :: Api ()
sendPublicBatch = do
  (SubmitBatch cmds) <- readJSON
  when (null cmds) $ die "Empty Batch"
  rks <- mapM queueCmds $ group 8000 cmds
  writeResponse $ ApiSuccess $ RequestKeys $ concat rks

poll :: Api ()
poll = do
  (Poll rks) <- readJSON
  log $ "Polling for " ++ show rks
  rs <- HM.filterWithKey (\k _ -> k `elem` rks) <$> (view aiResults >>= liftIO . readTVarIO)
  when (HM.null rs) $ log $ "No results found for poll! " ++ show rks
  writeResponse $ pollResultToReponse rs

pollResultToReponse :: HM.HashMap RequestKey Value -> PollResponse
pollResultToReponse m = ApiSuccess (kvToRes <$> HM.toList m)
  where
    kvToRes (rk,v) = PollResult { _prRequestKey = rk, _prLatency = 0, _prResponse = v}


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
  ic <- view aiInbound
  liftIO $ writeInbound ic (map snd rpcs)
  return $ fst <$> rpcs

serverConf :: MonadSnap m => Int -> Snap.Config m a
serverConf port =
  setErrorLog (ConfigFileLog "log/error.log") $
  setAccessLog (ConfigFileLog "log/access.log") $
  setPort port defaultConfig

registerListener :: Api ()
registerListener = do
  (ListenerRequest rk) <- readJSON
  log $ "listening for: " ++ show rk
  resTVar <- view aiResults
  res <- liftIO $ atomically $ do
    resMap <- readTVar resTVar
    case HM.lookup rk resMap of
      Nothing -> retry
      Just r -> return $ ApiSuccess $ PollResult rk 0 r
  writeResponse res

consumeResults :: ApiEnv -> IO ()
consumeResults ApiEnv{..} = do
  let (OutboundPactChan oc) = _aiOutbound
      log' s = liftIO (_aiLog $ "[pact server]: " ++ s)
  forever $ do
    outm <- liftIO $ atomically $ tryReadTChan oc
    when (not $ null outm) $ log' $ "received result(s) for: " ++ show outm
    liftIO $ atomically $ modifyTVar' _aiResults $ \rm ->
      let toResult (CommandResult rk r) = (rk,r)
          rm' = maybe rm (HM.union rm . HM.fromList . map toResult) outm
      in rm'
