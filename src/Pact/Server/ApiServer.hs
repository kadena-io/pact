{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Pact.Server.ApiServer
  ( runApiServer
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
import Snap.Http.Server as Snap
import Data.Text.Encoding
import Data.ByteString (ByteString)

import Pact.Types.Command
import Pact.Types.API

data ApiEnv = ApiEnv
  { _aiLog :: String -> IO ()
  , _aiInbound :: InboundPactChan
  , _aiOutbound :: OutboundPactChan
  , _aiResults :: MVar (HM.HashMap RequestKey Value)
  }
makeLenses ''ApiEnv

type Api a = ReaderT ApiEnv Snap a

runApiServer :: InboundPactChan -> OutboundPactChan -> (String -> IO ()) -> Int -> IO ()
runApiServer inChan outChan logFn port = do
  putStrLn $ "runApiServer: starting on port " ++ show port
  rm <- newMVar HM.empty
  httpServe (serverConf port) $
    applyCORS defaultOptions $ methods [GET, POST] $
    route [("api", runReaderT api (ApiEnv logFn inChan outChan rm))]


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
  rs <- HM.filterWithKey (\k _ -> k `elem` rks) <$> serviceOutbound False
  when (HM.null rs) $ log $ "No results found for poll!" ++ show rks
  writeResponse $ pollResultToReponse rs

pollResultToReponse :: HM.HashMap RequestKey Value -> PollResponse
pollResultToReponse m = ApiSuccess (kvToRes <$> HM.toList m)
  where
    kvToRes (rk,v) = PollResult { _prRequestKey = rk, _prLatency = 0, _prResponse = v}


log :: String -> Api ()
log s = view aiLog >>= \f -> liftIO (f $ "[Service|Api]: " ++ s)

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
buildCmdRpc c@PublicCommand {..} = return (RequestKey _cmdHash,fmap encodeUtf8 c)

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

-- | Drain outbound queue, optionally blocking for new results, and return map of results
serviceOutbound :: Bool -> Api (HM.HashMap RequestKey Value)
serviceOutbound retrying = do
  (OutboundPactChan oc) <- view aiOutbound
  log $ "serviceOutbound " ++ show retrying
  outm <- liftIO $ atomically $ (if retrying then fmap Just . readTChan else tryReadTChan) oc
  log $ "serviceOutbound: " ++ show outm
  view aiResults >>= \v -> liftIO $ modifyMVar v $ \rm -> do
    let toResult (PactResult rk r) = (rk,r)
        rm' = maybe rm (HM.union rm . HM.fromList . map toResult) outm
    return (rm',rm')


serverConf :: MonadSnap m => Int -> Snap.Config m a
serverConf port =
  setErrorLog (ConfigFileLog "log/error.log") $
  setAccessLog (ConfigFileLog "log/access.log") $
  setPort port defaultConfig

registerListener :: Api ()
registerListener = do
  (ListenerRequest rk) <- readJSON
  log $ "listen: " ++ show rk
  let loop retrying = do
        m <- serviceOutbound retrying
        case HM.lookup rk m of
          Nothing -> log ("doh: " ++ show rk) >> loop True
          Just r -> writeResponse $ ApiSuccess $ PollResult rk 0 r
  loop False
