{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types for remote verification of pact programs from GHCJS in the browser.
module Pact.Analyze.Remote.Types
( Request(..)
, Response(..)
, responseLines
, ClientError(..)
) where

import Control.Lens (makeLenses)

import qualified Data.Aeson as A
import qualified Data.Text as T

import GHC.Generics

import qualified Pact.JSON.Encode as J
import Pact.Types.PactError
import Pact.Types.Term (ModuleDef, ModuleName, Name)
import Pact.Types.Util

import Test.QuickCheck

data Request
  = Request ![ModuleDef Name] !ModuleName -- ^ verify one of the modules, by name
  deriving (Eq, Show, Generic)

instance A.FromJSON Request where
  parseJSON = A.withObject "Request" $ \o ->
    Request <$> o A..: "modules"
            <*> o A..: "verify"

#ifdef PACT_TOJSON
requestProperties :: JsonProperties Request
requestProperties (Request mods modName) =
  [ "verify"  A..= modName
  , "modules" A..= mods
  ]
{-# INLINE requestProperties #-}

instance A.ToJSON Request where
  toJSON = enableToJSON "Pact.Analyze.Remote.Types" . A.object . requestProperties
  toEncoding = A.pairs . mconcat . requestProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance J.Encode Request where
  build (Request mods modName) = J.object
    [ "verify"  J..= modName
    , "modules" J..= J.Array mods
    ]
  {-# INLINE build #-}

instance Arbitrary Request where
  arbitrary = Request <$> scale (min 10) arbitrary <*> arbitrary

newtype Response
  = Response
    { _responseLines :: [RenderedOutput]
      -- ^ Repl interactive output
    }
    deriving (Eq, Show, Generic)
    deriving newtype (Arbitrary)

instance A.FromJSON Response where
  parseJSON = A.withObject "Response" $ \o ->
    Response <$> o A..: "output"

#ifdef PACT_TOJSON
responseProperties :: JsonProperties Response
responseProperties (Response outputLines) =
  [ "output" A..= outputLines
  ]
{-# INLINE responseProperties #-}

instance A.ToJSON Response where
  toJSON = enableToJSON "Pact.Analyze.Remote.Types" . A.object . responseProperties
  toEncoding = A.pairs . mconcat . responseProperties
  {-# INLINE toJSON #-}
  {-# INLINE toEncoding #-}
#endif

instance J.Encode Response where
  build o = J.object
    [ "output" J..= J.Array (_responseLines o)
    ]

newtype ClientError
  = ClientError String
  deriving (Show, Eq)
  deriving newtype (Arbitrary)

instance A.FromJSON ClientError where
  parseJSON = A.withObject "ClientError" $ \o ->
    ClientError <$> o A..: "error"

#ifdef PACT_TOJSON
instance A.ToJSON ClientError where
  toJSON (ClientError err) = A.object
    ["error" A..= err
    ]
#endif

instance J.Encode ClientError where
  build (ClientError err) = J.object
    ["error" J..= J.text (T.pack err)
    ]

makeLenses ''Response
