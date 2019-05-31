{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Pact.Types.Swagger
-- Copyright   :  (C) 2019 Stuart Popejoy
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Stuart Popejoy <stuart@kadena.io>
--
-- Swagger specification utilities and exports.
--

module Pact.Types.Swagger
  (
    -- | Schemas
    byteBase64url
    -- | ToSchema implementations
  , declareGenericEmpty
  , declareGenericSchema
  , declareGenericString
  , namedSchema
    -- | Modifiers
  , fixedLength
  , swaggerType
  , schemaOf
  , withSchema
    -- | Re-exports
  , ToSchema(..)
    -- | Debugging
  , debugSchema
  ) where

import Data.Proxy
import Data.Swagger
import Data.Swagger.Declare
import Data.Swagger.Internal
import GHC.Generics
import Control.Lens (set)
import Data.Text (Text)


-- | 'ToSchema' generic implementation with empty schema.
declareGenericEmpty ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericEmpty _ = declareGenericSchema mempty (Proxy @a)

-- | 'ToSchema' generic implementation with provided schema.
declareGenericSchema ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  Schema -> proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericSchema s _ = pure $ genericNameSchema defaultSchemaOptions (Proxy @a) s

declareGenericString ::
  forall a d f proxy. (Generic a, Rep a ~ D1 d f, Datatype d) =>
  proxy a -> Declare (Definitions Schema) NamedSchema
declareGenericString _ = declareGenericSchema (schemaOf $ swaggerType SwaggerString) (Proxy @a)

namedSchema ::
    forall a proxy .
    Text -> Schema -> proxy a -> Declare (Definitions Schema) NamedSchema
namedSchema n s _ = return $ NamedSchema (Just n) s

-- | like 'byteSchema' but with (non-standard) "base64url" in format.
byteBase64url :: Schema
byteBase64url = swaggerType SwaggerString . set format (Just "base64url") $ mempty

fixedLength :: Integral i => i -> Schema -> Schema
fixedLength i =
  set maxLength (Just $ fromIntegral i) .
  set minLength (Just $ fromIntegral i)

swaggerType :: SwaggerType 'SwaggerKindSchema -> Schema -> Schema
#if MIN_VERSION_swagger2(2,4,0)
swaggerType = set type_ . Just
#else
swaggerType = set type_
#endif

schemaOf :: (Schema -> Schema) -> Schema
schemaOf f = f mempty

withSchema :: Schema -> (Schema -> Schema) -> Schema
withSchema s f = f s

-- debugSchema
debugSchema :: forall a proxy. ToSchema a => proxy a -> NamedSchema
debugSchema _ = undeclare $ declareNamedSchema (Proxy @a)
