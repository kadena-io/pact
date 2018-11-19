{-# LANGUAGE OverloadedStrings #-}

module Pact.Analyze.Remote.Client
  ( verifyModule
  ) where

import qualified Control.Exception             as C
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.Aeson                    as A
import qualified Data.Foldable                 as Foldable
import qualified Data.HashMap.Strict           as HM
import qualified Data.JSString                 as JS
import qualified Data.JSString.Text            as JS
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as LT
import qualified Data.Text.Lazy.Encoding       as LTE

import qualified JavaScript.Web.XMLHttpRequest as XHR

import qualified Pact.Analyze.Remote.Types as Remote
import           Pact.Types.Runtime        (ModuleData(..))
import           Pact.Types.Term           (Module(_mName), ModuleName)
import           Pact.Types.Util           (tShow)

verifyModule
  :: HM.HashMap ModuleName ModuleData -- ^ all loaded modules
  -> ModuleData                       -- ^ the module we're verifying
  -> String
  -> Int
  -> IO [Text]
verifyModule namedMods mod' host port = do
  let requestURI = "https://" ++ host ++ ":" ++ show port ++ "/verify"
      body       = Remote.Request (Foldable.toList $ fmap _mdModule namedMods) (_mName $ _mdModule mod')
      jsonBody   = JS.lazyTextToJSString $ LTE.decodeUtf8 $ A.encode body
  eResponse <- liftIO (C.try $ post requestURI jsonBody :: IO (Either XHR.XHRError (XHR.Response Text)))
  return $ case eResponse of
    Left s       -> ["error communicating with verification server: " <> tShow s]
    Right result -> case XHR.contents result of
      Nothing  -> ["no response from verification server"]
      Just txt -> case A.decode $ LTE.encodeUtf8 $ LT.fromStrict txt of
        Nothing -> ["error parsing result from verification server: " <> txt]
        Just (Remote.Response outputLines) -> outputLines

  where
    post requestURI body = XHR.xhrText $ XHR.Request
      { XHR.reqMethod          = XHR.POST
      , XHR.reqURI             = JS.pack requestURI
      , XHR.reqLogin           = Nothing
      , XHR.reqHeaders         = []
      , XHR.reqWithCredentials = False
      , XHR.reqData            = XHR.StringData body
      }
