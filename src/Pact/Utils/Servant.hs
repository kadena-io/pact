{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Pact.Utils.Servant
-- Copyright: Copyright © 2023 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Pact.Utils.Servant
( PactJson(..)
) where

import Data.Aeson

import qualified Pact.JSON.Encode as J

import Servant.API.ContentTypes
import Servant.API.UVerb

newtype PactJson = PactJson JSON
  deriving newtype (Accept)

instance {-# OVERLAPPABLE #-} J.Encode a => MimeRender PactJson a where
    mimeRender _ = J.encode

instance {-# OVERLAPPING #-} MimeRender PactJson a => MimeRender PactJson (WithStatus _status a) where
  mimeRender contentTypeProxy (WithStatus a) = mimeRender contentTypeProxy a

instance FromJSON a => MimeUnrender PactJson a where
  mimeUnrender = mimeUnrender @PactJson @a

