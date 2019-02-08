{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pact.Analyze.Remote.Client
  ( verifyModule
  ) where

import           Control.Concurrent.MVar    (MVar, newEmptyMVar, putMVar,
                                             takeMVar)
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Foldable              as Foldable
import qualified Data.HashMap.Strict        as HM
import           Data.IORef                 (atomicModifyIORef', newIORef)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           GHCJS.DOM.EventM           (onAsync)

import qualified GHCJS.DOM.XMLHttpRequest   as XHR

import qualified Pact.Analyze.Remote.Types  as Remote
import           Pact.Types.Runtime         (ModuleData (..))
import           Pact.Types.Term            (Module (_mName), ModuleName)

verifyModule
  :: HM.HashMap ModuleName ModuleData -- ^ all loaded modules
  -> ModuleData                       -- ^ the module we're verifying
  -> String
  -> IO [Text]
verifyModule namedMods mod' uri = do
  let requestURI = uri ++ "/verify"
      body       = Remote.Request (Foldable.toList $ fmap _mdModule namedMods) (_mName $ _mdModule mod')
      jsonBody   = T.decodeUtf8 . BSL.toStrict $ A.encode body
  req <- XHR.newXMLHttpRequest
  let nothingText :: Maybe Text
      nothingText = Nothing
  XHR.open req ("POST" :: Text) requestURI True nothingText nothingText
  XHR.setTimeout req 5000 -- Terminate at some point (5 seconds).
  XHR.setRequestHeader req ("content-type" :: Text) ("application/json;charset=utf-8" :: Text)
  alreadyHandled <- liftIO $ newIORef False
  respVar :: MVar [Text] <- newEmptyMVar
  void $ req `onAsync` XHR.readyStateChange $ do
    readyState <- XHR.getReadyState req
    status <- XHR.getStatus req
    statusText :: Text <- XHR.getStatusText req
    handled <- liftIO $ atomicModifyIORef' alreadyHandled $ \handled ->
      if readyState == 4 then (True, handled) else (handled, handled)

    when (readyState == 4 && not handled) $ do
      er :: Either Text [Text] <- runExceptT $ do
        when (status /= 200 && status /= 201) $
          throwE $ "Request failed with status: " <> (T.pack . show) status <> "(" <> statusText <> ")."

        raw :: Text <- do
          mRaw <- XHR.getResponseText req
          maybe (throwE "Got no response body from verification server!") pure mRaw
        case A.decodeStrict $ T.encodeUtf8 raw of
          Nothing -> throwE $ "Parsing result from verification server: " <> raw
          Just (Remote.Response outputLines) -> pure outputLines
      let r = either (pure . ("Error: " <>)) id er
      liftIO $ putMVar respVar r
  XHR.sendString req jsonBody
  liftIO $ takeMVar respVar
