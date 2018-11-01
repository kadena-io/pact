{-# LANGUAGE RankNTypes, OverloadedStrings, DeriveDataTypeable,
             ForeignFunctionInterface, JavaScriptFFI, EmptyDataDecls,
             TypeFamilies, DataKinds, ScopedTypeVariables,
             FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
             LambdaCase, MultiParamTypeClasses, DeriveGeneric #-}

-- | This is heavily adapted from
-- https://raw.githubusercontent.com/ghcjs/ghcjs-base/01014ade3f8f5ae677df192d7c2a208bd795b96c/JavaScript/Web/XMLHttpRequest.hs
-- because reflex-platform uses a patch to omit this module (to avoid a
-- circular dependency related to @text@). We had to drop support for
-- non-String/Text types because of lack of access to internal modules of
-- ghcjs-base.
module JavaScript.Web.XMLHttpRequest ( xhr
                                     , xhrText
                                     , xhrString
                                     , Method(..)
                                     , Request(..)
                                     , RequestData(..)
                                     , Response(..)
                                     , ResponseType(..)
                                     , FormDataVal(..)
                                     , XHRError(..)
                                     ) where

import Control.Exception
import Control.Monad

import GHCJS.Types
import GHCJS.Marshal.Pure

import GHC.Generics

import Data.Data
import Data.Text (Text)

import           Data.JSString.Text (textFromJSString)
import qualified Data.JSString as JSS

import Unsafe.Coerce (unsafeCoerce)

data Method = GET | POST | PUT | DELETE
  deriving (Show, Eq, Ord, Enum)

data XHRError = XHRError String
              | XHRAborted
              deriving (Generic, Data, Typeable, Show, Eq)

instance Exception XHRError

methodJSString :: Method -> JSString
methodJSString GET    = "GET"
methodJSString POST   = "POST"
methodJSString PUT    = "PUT"
methodJSString DELETE = "DELETE"

type Header = (JSString, JSString)

data FormDataVal = StringVal JSString
  deriving (Typeable)

data Request = Request { reqMethod          :: Method
                       , reqURI             :: JSString
                       , reqLogin           :: Maybe (JSString, JSString)
                       , reqHeaders         :: [Header]
                       , reqWithCredentials :: Bool
                       , reqData            :: RequestData
                       }
  deriving (Typeable)

data RequestData = NoData
                 | StringData     JSString
  deriving (Typeable)

data Response a = Response { contents              :: Maybe a
                           , status                :: Int
                           , getAllResponseHeaders :: IO JSString
                           , getResponseHeader     :: JSString -> IO (Maybe JSString)
                           }

instance Functor Response where fmap f r = r { contents = fmap f (contents r) }

class ResponseType a where
    getResponseTypeString :: Proxy a  -> JSString
    wrapResponseType      :: JSVal -> a

instance ResponseType JSString where
  getResponseTypeString _ = "text"
  wrapResponseType        = unsafeCoerce -- newtype-wrapped JSVal

newtype XHR = XHR JSVal deriving (Typeable)

-- -----------------------------------------------------------------------------
-- main entry point

xhr :: forall a. ResponseType a => Request -> IO (Response a)
xhr req = js_createXHR >>= \x ->
  let doRequest = do
        case reqLogin req of
          Nothing           ->
            js_open2 (methodJSString (reqMethod req)) (reqURI req) x
          Just (user, pass) ->
            js_open4 (methodJSString (reqMethod req)) (reqURI req) user pass x
        --
        -- TODO: override this so that JS doesn't automatically deserialize
        -- the JSON -- we'll use Aeson instead:
        --
        js_overrideMimeType "text/plain" x
        js_setResponseType
          (getResponseTypeString (Proxy :: Proxy a)) x
        forM_ (reqHeaders req) (\(n,v) -> js_setRequestHeader n v x)

        case reqWithCredentials req of
          True  -> js_setWithCredentials x
          False -> return ()

        r <- case reqData req of
          NoData                            ->
            js_send0 x
          StringData str                    ->
            js_send1 (pToJSVal str) x
        case r of
          0 -> do
            status' <- js_getStatus x
            r'      <- do
              hr <- js_hasResponse x
              if hr then Just . wrapResponseType <$> js_getResponse x
                    else pure Nothing
            return $ Response r'
                              status'
                              (js_getAllResponseHeaders x)
                              (\h -> getResponseHeader' h x)
          1 -> throwIO XHRAborted
          2 -> throwIO (XHRError "network request error")
          _ -> throwIO (XHRError "network request error")
  in doRequest `onException` js_abort x

getResponseHeader' :: JSString -> XHR -> IO (Maybe JSString)
getResponseHeader' name x = do
  h <- js_getResponseHeader name x
  return $ if isNull h then Nothing else Just (wrapResponseType h)

-- -----------------------------------------------------------------------------
-- utilities

xhrString :: Request -> IO (Response String)
xhrString = fmap (fmap JSS.unpack) . xhr

xhrText :: Request -> IO (Response Text)
xhrText = fmap (fmap textFromJSString) . xhr

-- -----------------------------------------------------------------------------

foreign import javascript unsafe
  "$1.withCredentials = true;"
  js_setWithCredentials :: XHR -> IO ()

foreign import javascript unsafe
  "new XMLHttpRequest()"
  js_createXHR :: IO XHR
foreign import javascript unsafe
  "$2.responseType = $1;"
  js_setResponseType :: JSString -> XHR -> IO ()
foreign import javascript unsafe
  "$1.abort();"
  js_abort :: XHR -> IO ()
foreign import javascript unsafe
  "$3.setRequestHeader($1,$2);"
  js_setRequestHeader :: JSString -> JSString -> XHR -> IO ()
foreign import javascript unsafe
  "$3.open($1,$2)"
  js_open2 :: JSString -> JSString -> XHR -> IO ()
foreign import javascript unsafe
  "$5.open($1,$2,true,$4,$5);"
  js_open4 :: JSString -> JSString -> JSString -> JSString -> XHR -> IO ()
foreign import javascript unsafe
  "$1.status"
  js_getStatus :: XHR -> IO Int
foreign import javascript unsafe
  "$1.response"
  js_getResponse :: XHR -> IO JSVal
foreign import javascript unsafe
  "$1.response ? true : false"
  js_hasResponse :: XHR -> IO Bool
foreign import javascript unsafe
  "$1.getAllResponseHeaders()"
  js_getAllResponseHeaders :: XHR -> IO JSString
foreign import javascript unsafe
  "$2.getResponseHeader($1)"
  js_getResponseHeader :: JSString -> XHR -> IO JSVal
foreign import javascript unsafe
  "$2.overrideMimeType($1)"
  js_overrideMimeType :: JSString -> XHR -> IO ()

-- -----------------------------------------------------------------------------

foreign import javascript interruptible
  "h$sendXHR($1, null, $c);"
  js_send0 :: XHR -> IO Int
foreign import javascript interruptible
  "h$sendXHR($2, $1, $c);"
  js_send1 :: JSVal -> XHR -> IO Int

